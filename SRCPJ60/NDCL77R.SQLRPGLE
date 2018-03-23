000100090331     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200090331     H*PARMS BNDDIR(PJXBND)
000300090331     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000400090331     H*PARMS DYNUSRPRF(*OWNER) RDB(*NONE)
000401151022     H DFTACTGRP(*NO) ACTGRP('QILE') DATFMT(*ISO) BNDDIR('PJXBND')
000500051114     H DECEDIT('0,') DATEDIT(*DMY.)
000600041126      **********************************************************************
000700041126      *
000800090330    > *                      SCHEDA CLIENTE
000900090330      *           -------------------------------------
001000090407     F*------------
001100090416     FAnUni01L  IF   E           K DISK
001200100524     FNdImp01L  UF   E           K DISK
001300140313     FNdScd01L  IF   E           K DISK
001400100524     FNDRca04L  UF   E           K DISK
001500130916C2226F                                     InfDs(Inf001)
001600130916     FNDRca01L  IF   E           K DISK
001700100524     F                                     RENAME(NDRCA000:NDRCA01)
001800090330     F*------------
001900090331    >FNDCL77D   CF   E             WORKSTN
002000090330    >F                                     RENAME(CL77D1:FMTD1)
002100090330    >F                                     RENAME(CL77D3:FMTD3)
002200090330    >F                                     RENAME(CL77D4:FMTD4)
002300090330    >F                                     RENAME(CL77S1:FMTS1)
002400090330    >F                                     RENAME(CL77C1:FMTC1)
002500090330    >F                                     RENAME(CL77S2:FMTS2)
002600090330    >F                                     RENAME(CL77C2:FMTC2)
002700090330    >F                                     RENAME(CL77T1:FMTT1)
002800090330    >F                                     RENAME(CL77Z1:FMTZ1)
002900110407D2680F                                     RENAME(CL77D5:FMTD5)
003000090330     F                                     SFILE(FMTS1:S1NRR)
003100090330     F                                     SFILE(FMTS2:S2NRR)
003200090330     F                                     INFDS(DSFMT)
003300041213      *----------------------------------------------------*
003400041213      *Schiere opzioni
003500041213     D $DIMOP          C                   CONST(30)
003600041213     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003700041213     D $TX1            S             50    DIM($dimop) ALT($MS1)
003800041213     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003900041213     D $FL1            S              1    DIM($dimop) ALT($OP1)
004000041213     D $PGO1           S             10    DIM($dimop)
004100090331     D $MS2            S             27    DIM($dimop) CTDATA PERRCD(1)
004200090331     D $TX2            S             50    DIM($dimop) ALT($MS2)
004300090331     D $OP2            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004400090331     D $FL2            S              1    DIM($dimop) ALT($OP2)
004500041213      *----------------------------------------------------*
004600041213     D $TX             S             50    DIM(50)
004700041213     D $FL             S              1    DIM(50)
004800041213      *----------------------------------------------------*
004900041213      *Schiere tasti funzione generiche
005000041213     D $FM             S             27    DIM(24)
005100041213     D $FX             S             50    DIM(24)
005200041213     D $FC             S              1    DIM(24)
005300041213     D $FR             S              1    DIM(24)
005400041213      *----------------------------------------------------*
005500090330      * Tasti funzione D1/S1/S2
005600041213     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005700041213     D $FX1            S             50    DIM(24) ALT($FM1)
005800041213     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005900041213     D $FR1            S              1    DIM(24) ALT($FC1)
006000041213      *----------------------------------------------------*
006100041213      * schiera nomi pgm richiamati con tasti funzione personalizzati S1
006200041213     D $PGF2           S             10    DIM(25)
006300041213      *----------------------------------------------------*
006400041213      *contiene la dimensione del campo a video che conterrà
006500041213     D $D1L01          S              3  0
006600041213     D $D1L02          S              3  0
006700041213     D $C1Op1          S              3  0
006800090331     D $C2Op1          S              3  0
006900041213      *----------------------------------------------------*
007000041213      * Indice della schiera tasti funzione
007100041213     D PosFun          S              2  0
007200041213      *----------------------------------------------------*
007300041213      * Valori assunti
007400041213     D XMPEDS        E DS                  INZ
007500051115     D XMPEDS1       E DS                  INZ
007600041213     D DSMPE           DS                  INZ
007700041213     D  $LOP                   1      1  0
007800041213     D  $ORD                   2      3  0
007900041213      *----------------------------------------------------*
008000100408     D NDRCA         E DS                  EXTNAME(NDRCA00F)
008100100408      *----------------------------------------------------*
008200041213      * Passaggio Parametri
008300041213     D KPJBA         E DS
008400041213      *----------------------------------------------------*
008500041213      * Reperimento Autorizzazioni
008600041213     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
008700041213      *----------------------------------------------------*
008800041213      * Passaggio Autorizzazioni
008900041213     D DSAUT           DS           512
009000041213      *----------------------------------------------------*
009100051115      * Maschera dati autorizzazioni
009200100409    >D CONRECDS      E DS                  inz
009300090330     D*-------------
009400090330     D* Dati per pgm XNOAUT
009500090330     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
009600051116     D*-------------
009700090406     D* Parametri gestione SOLLECITI
009800090330     D ANP057DS      E DS
009900090406     D*-------------
010000090406     D* Parametri gestione SCADENZARIO
010100090406     D NDC601DS      E DS
010200140604     D*-------------
010201140604     D tnta12ds      E DS
010206140604     D*-------------
010300140314     D* DS per input condizione di pagamento
010400140314     DDVCDPDS        E DS                  INZ
010500140314     D* Ds per output
010600140314     DDVOCDPDS       E DS                  INZ
010700090406     D*-------------
010800051116     D* DS gestione parametri modulo
010900051116     D XPAOUT          DS          2000
011000041213      *----------------------------------------------------*
011100041213      * Dati di ambiente ottenuti da XSOC
011200041213     D SOC001        E DS                  EXTNAME(XSOC001DS)
011300041213      *------
011400041213      * DS Interna per dati di output di XSOC
011500041213     D XSOCDS          DS          1000
011600041213      *----------------------------------------------------*
011700090330      * Parametri in ricezione
011800090330     D NDCL77DS      E DS
011900090330     D*----------------------------------------------------*
012000090330     D* Reperimento profilo
012100090330     D parmok          s              1
012200090330     D* parametro utente
012300090330     D ANUTNParm       s                   like(ANUTN00F)
012400090330     D* lunghezza parametro
012500090330     D LungUTN         S              5s 0
012600090330     D*
012700090330     D Anutn00f      e ds
012800051115     D*-----------
012900051115     D* Definizioni parametri standard chiamate moduli
013000090331     D$Esito           S              1
013100090331     D$gesmsg          S              1
013200090331     D$lenout          S              9B 0
013300090331     D$struttura       S             10
013400090331     D$kcc             S              6
013500090331     D$ksc             S              8
013600090330     D*-------------
013700090330     D* Parametri del PGM di interrogazione conti
013800090330    >D ANA399DS      E DS                  inz
013900090330    >D  A39RET       E                     INZ('0')
014000090330    >D  A39ERRORI    E                     INZ('0')
014100090401     D*----------------------------------------------------*
014200090401     D* DS per richiesta dati a X20
014300090401     D X20ScadDS     E DS                  inz
014400090401     D* DS parametro esteso se richiesto x20parscd='S'
014500090401     D X20SCADXDS    E DS                  inz
014600090401     D*------------
014700090401     D* DS esterna per dati clienti
014800090401     D DVACLI        E DS                  INZ
014900090401     D*-------------
015000090401     D* Definizioni parametri driver anagrafici
015100090401     DDvaric           S              1
015200090401     DDvaSocieta       S              3
015300090401     DDvaUnita         S              8
015400090401     DDvaStrutt        S             10
015500090401     DDvaDtRif         S               D
015600090401     DDvaLenOut        S              9B 0
015700090401     DDvaSnatura       S              1
015800090401     DDvaCodice        S              8
015900090401     DDvaLineav        S              3
016000090401     DDvaFiliale       S              3  0
016100090401     DDvaSogg          S              8
016200090401     DDvaTpInd         S              2
016300090401     DDvaCdInd         S              3
016400090401     DDvaErrore        S              1
016500090401     DDvaOutput        S           4000
016600090415     D*-------------
016700090330     D* Controllo conti
016800090330     D ND002DS       E DS
016900090415     D Categorie             131    250
017000090415     D*-------------
017100090415     D* Schiera categorie da ND002DS
017200090415     D DsCateg         DS           120
017300090415     D SkCateg                        4    dim(30)
017400051117     D*-------------
017500051117     D* Chiamata ricorsiva
017600051117     D X48CHPGMDS    E DS                  INZ
017700090401     D*----------
017800090401     D* Parametri del PGM di reperimento informazioni recupero credito
017900090401     D NDCL99DS      E DS                  inz
018000090407     D*-------------
018100090407     D* Parametri del PGM di gestione impegni
018200090407     D NDC988DS      E DS                  inz
018300130916C2226D*-------------
018400130916  "  D* Parametri del PGM di gestione attività manuali
018500130916C2226D NDC980DS      E DS                  inz
018600090407     D*-------------
018700100416     D* Parametri del PGM di immissione fase
018800100416     D NDCL69DS      E DS                  inz
018900100416     D*-------------
019000100415     D* Parametri del PGM di gestione note
019100100415     D NDCL75DS      E DS                  inz
019200090408     D*-------------
019300090408     D XATBDS        E DS                  INZ
019400090408     D*-------------
019500090408     D* DS esterna tabella 0CF
019600090408     D Ang0CFDS      E DS                  Inz
019700090415     D*-------------
019800100408     D* DS esterna tabella 047
019900100408     D Ang047DS      E DS                  Inz
020000090415     D*-------------
020100100408     D* DS esterna tabella 048
020200100408     D Ang048DS      E DS                  Inz
020300100419     D*-------------
020400100419     D* Tabella rimesse dirette
020500100419     D angG56ds      E DS                  inz
020600100419     D*-------------
020700100419     D* Modalità di pagamento
020800100419     D angG57ds      E DS                  inz
020900100419     D*-------------
021000100611c2156D*  ds attributi contabili
021100100611c2156D Ang0A1ds      E DS                  INZ
021200041213      *----------------------------------------------------*
021300041213      * campo di salvataggio per la DS d'ingresso durante la chiamata
021400041213      * alla funzione personalizzata
021500090331     D $SaveDS         S                   like(NDCL77DS)
021600130916C2226 *----------------------------------------------------*
021700130916  "  D* DS Numero relativo NDRCA00F
021800130916  "  D Inf001          DS
021900130916C2226D  RRNRCA               397    400B 0
022000041213      *----------------------------------------------------*
022100041213     D DSFMT           DS           512
022200041213     D  $TASTO               369    369
022300041213     D  NRG                  370    370
022400041213     D  NCL                  371    371
022500090330     D  SFLNRR               378    379B 0
022600090330      * riga cursore window
022700090330     D  NRgWin               382    382
022800090330      * colonna cursore window
022900090330     D  NClWin               383    383
023000041223      *----------------------------------------------------*
023100041223      * DS per UserExit
023200041223     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
023300041213      *----------------------------------------------------*
023400041213      * posizione cursore
023500041213     D CURSOR          DS
023600041213     D  RIRI                   1      2B 0 INZ
023700041213     D  R$$                    2      2
023800041213     D  COCO                   3      4B 0 INZ
023900041213     D  C$$                    4      4
024000041213      *----------------------------------------------------*
024100041213      * Reperimento nome PGM
024200041213     D STATUS         SDS           333
024300041213     D  DSPGM            *PROC
024400041213     D  PARMS            *PARMS
024500100416     D  SDSUSR               254    263
024600041213      *----------------------------------------------------*
024700041213      * Costanti
024800041213      * nr.di righe del sfl1(SFLPAG)
024900090519     D SflPagS1        C                   CONST(9)
025000041217      * nr.di righe del sfl2(SFLPAG)
025100041217      * impostato al nr. max delle righe del sfl per fare
025200041217      * il caricamento sfl non a pagine
025300090519     D SflPagS2        C                   CONST(9)
025400041213      *----------------------------------------------------*
025500041213      * Tasti di funzione
025600041213     D F01             C                   CONST(X'31')
025700041213     D F02             C                   CONST(X'32')
025800041213     D F03             C                   CONST(X'33')
025900041213     D F04             C                   CONST(X'34')
026000041213     D F05             C                   CONST(X'35')
026100041213     D F06             C                   CONST(X'36')
026200041213     D F07             C                   CONST(X'37')
026300041213     D F08             C                   CONST(X'38')
026400041213     D F09             C                   CONST(X'39')
026500041213     D F10             C                   CONST(X'3A')
026600041213     D F11             C                   CONST(X'3B')
026700041213     D F12             C                   CONST(X'3C')
026800041213     D F13             C                   CONST(X'B1')
026900041213     D F14             C                   CONST(X'B2')
027000041213     D F15             C                   CONST(X'B3')
027100041213     D F16             C                   CONST(X'B4')
027200041213     D F17             C                   CONST(X'B5')
027300041213     D F18             C                   CONST(X'B6')
027400041213     D F19             C                   CONST(X'B7')
027500041213     D F20             C                   CONST(X'B8')
027600041213     D F21             C                   CONST(X'B9')
027700041213     D F22             C                   CONST(X'BA')
027800041213     D F23             C                   CONST(X'BB')
027900041213     D F24             C                   CONST(X'BC')
028000041213     D ENTER           C                   CONST(X'F1')
028100041213     D ROLDWN          C                   CONST(X'F4')
028200041213     D ROLLUP          C                   CONST(X'F5')
028300041213      *----------------------------------------------------*
028400051111      * Sottotitolo di testata.
028500090330     D SOTTIT          C                   CONST(' ')
028600041213      *----------------------------------------------------*
028700041213      * Variabili per gestione videate
028800041213     D $Gest           S              2A
028900050103      * indica quale sfl gestire in base agli errori oppure al cursore
029000041221     D WGest           S                   like($Gest) inz
029100050103      * indica su quale sfl si è verificato un errore specifico per quel sfl
029200050103     D $SflErr         S                   like($Gest) inz
029300041213     D $Fine           S              1A
029400041213     D $InzD1          S              1A
029500041213     D $InzS1          S              1A
029600041217     D $InzS2          S              1A
029700041213     D $InzD2          S              1A
029800041213     D $LastG          S              2A
029900041213     D $LastV          S              2A
030000090331     D $EndFileS1      S              1A
030100090331     D $EndFileS2      S              1A
030200041213     D $Esci           S              1A
030300041213     D $RcdOK          S              1A
030400041213     D $Unique         S              1A
030500041213      *----------------------------------------------------*
030600041213      * Variabili appoggio sempre presenti in tutte le anagrafiche
030700041221     D WC1Rcd          S                   like(C1Rcd)
030800041221     D WC2Rcd          S                   like(C2Rcd)
030900041221     D S1Nrr           S                   like(C1Rcd)
031000041217     D S2Nrr           S                   like(C2Rcd)
031100041217     D WSflS1          S                   like(C1Nrr)
031200041217     D WSflS2          S                   like(C2Nrr)
031300041217     D WMaxS1          S                   like(C1Rcd)
031400041217     D WMaxS2          S                   like(C2Rcd)
031500041217     D WPagS1          S                   like(C1Rcd)
031600041217     D WPagS2          S                   like(C2Rcd)
031700041213     D WInzS1          S                   like($InzS1)
031800041217     D WInzS2          S                   like($InzS2)
031900090331     D WOpr            S                   like(OprL77)
032000041213     D CurR            S              2S 0
032100041213     D CurC            S              2S 0
032200041213     D Win             S             99A
032300041213     D W1Pos           S                   like(C1Pos)
032400041213     D WINKR           S              1A
032500130621D2798D SavOpzS1        S                   like(S1Opz)
032600041213      *----------------------------------------------------*
032700041213      * Variabili appoggio specifiche della singola anagrafica
032800041213      *  campi per parzializzazioni su rcd (solo alfanumerici)
032900041213      *----------------------------------------------------*
033000041213      *  Indici
033100041213     D X               S              3S 0
033200041213     D Y               S              3S 0
033300041213      *----------------------------------------------------*
033400041213      * Indice della schiera per le opzioni personalizzate
033500041213     D PosOpz          S              2  0
033600041213      * numero max opzioni
033700041213     D NumOpz          S              2  0 Inz($DIMOP)
033800041213      * numero effettive opzioni
033900100419     D OpzLen1         S              2  0
034000100419     D OpzLen2         S              2  0
034100041213      *----------------------------------------------------*
034200041213      * Attributi video
034300041213     D Normale         C                   Const(X'32')
034400041213     D Protetto        C                   Const(X'A0')
034500090330     D*-------------
034600090330     DJCMIt          E Ds                  extname(JcMit00F)
034700090331     DANTCM          E Ds                  extname(AnTcm00F)
034800090330     D*-------------
034900090331     D* ds per fetch S1
035000090331     D DsFetchS1       DS
035100100408     D DsSocieta                           like(RcaSocieta)
035200100408     D DsPosiz                             like(RcaPosiz)
035300100408     D DsKcc                               like(RcaKcc)
035400100408     D DsKsc                               like(RcaKsc)
035500100408     D DsDtPar                             like(RcaDtPar)
035600100408     D DsNrPar                             like(RcaNrPar)
035700100408     D DsSerPar                            like(RcaSerPar)
035800100408     D DsDtSca                             like(RcaDtSca)
035900090331     D*-------------
036000090331     D* ds per fetch S2
036100090331     D DsFetchS2       DS
036200100408     D DsSocieta2                          like(RcaSocieta)
036300100408     D DsPosiz2                            like(RcaPosiz)
036400100408     D DsGrute2                            like(RcaGrute)
036500100408     D DsKcc2                              like(RcaKcc)
036600100408     D DsKsc2                              like(RcaKsc)
036700100408     D DsFase2                             like(RcaFase)
036800100408     D DsAzione2                           like(RcaAzione)
036900100408     D DsDtPar2                            like(RcaDtPar)
037000100408     D DsNrPar2                            like(RcaNrPar)
037100100408     D DsSerPar2                           like(RcaSerPar)
037200100408     D DsDtSca2                            like(RcaDtSca)
037300100413     D DsDtChiu2                           like(RcaDtChiu)
037400100413     D DsUteCHiu2                          like(RcaUteChiu)
037500100413     D DsStato2                            like(RcaStato)
037600100413     D DsNote2                             like(RcaNota)
037700100416     D DsDtAtt2                            like(RcaDtAtt)
037800100419     D DsDtLet2                            like(RcaDtLet)
037900100419     D DsNrLet2                            like(RcaNrLet)
038000100419     D DsProgr2                            like(RcaProgr)
038100090330     D*-------------
038200090331     D* Stringa per SQLs1
038300090330     D SQ              DS          4000
038400090331     D* Stringa per SQLs2
038500090331     D SQ2             DS          4000
038600090330      *----------------------------------------------------*
038700090330     D $A              s             79
038800090330     D $B              s             79
038900041222      *----------------------------------------------------*
039000041222      * DS Esterna gestione messaggi
039100041222     D XMSGDS        E DS
039200041222     D  STS          E                     EXTFLD(MSGSTS)
039300041222     D                                     DIM(333)
039400041222     D  JBA          E                     EXTFLD(MSGJBA)
039500041222     D                                     DIM(502)
039600041222      *----------------------------------------------------*
039700051115      * campi di lavoro
039800041222     D XMsgInz         s              1a   inz('1')
039900051115     D ZdateIso        s               D
040000100419     D* data scadenza selezionata
040100100419     D Seldtsca        s               D
040200100419     D SelImportd      s                   like(Rcaimportd)
040300100419     D SelSegno        s                   like(RcaSegno)
040400100419     D SelMdPag        s                   like(Rcamdpag)
040500100419     D SelMdRd         s                   like(RcaMdRD)
040600090331     D Wdate           s               D
040700090406     D WDare           s             19  3
040800090406     D WAvere          s             19  3
040900090415     D Wmsg1           s                   like(Txtmsg)
041000090415     D Wmsg2           s                   like(Txtmsg)
041100090416     D Wmsg3           s                   like(Txtmsg)
041200090416     D Wmsg4           s                   like(Txtmsg)
041300090416     D WUnita          s             35
041400100513     D Wrkattr         s              4
041500100524     D UltFaseAp       s              1
041600130913C2226D* ds nota di chiusura fase
041700130913  "  D d5nota          DS           400
041800130913  "  D d5nota1                 1     36
041900130913  "  D d5nota2                37     88
042000130913  "  D d5nota3                89    140
042100130913  "  D d5nota4               141    192
042200130913  "  D d5nota5               193    244
042300130913  "  D d5nota6               245    296
042400130913  "  D d5nota7               297    348
042500130913C2226D d5nota8               349    400
042600041222      *-------------------------------------------------------*
042700090415     D* schiera costanti
042800090415     D MSG             S             27    DIM(10) CTDATA PERRCD(1)             messaggi possibili
042900051116      *----------
043000041213      ******************************************************
043100041213      *                MAIN LINE PROGRAM
043200041213      ******************************************************
043300041213      *
043400041213      * inizializzazione variabili
043500041213     C                   EXSR      INZVAR
043600041213      *
043700041213     C     $FINE         DOWEQ     *OFF
043800090401     C     $GEST         CASEQ     'D1'          GESD1
043900041213     C     $GEST         CASEQ     'S1'          GESS1
044000041217     C     $GEST         CASEQ     'S2'          GESS2
044100041213     C                   ENDCS
044200041213     C                   ENDDO
044300041213      * fine programma
044400041213     C                   EXSR      ENDPGM
044500041213      *
044600041213      ************************************************************
044700041213      * FINE PROGRAMMA
044800041213      ************************************************************
044900041213     C     ENDPGM        BEGSR
045000060103      *
045100060103      * passaggio dati al pgm chiamante
045200090331     C                   MOVEL     WOPR          OPRL77
045300090331     C                   MOVEL     NDCL77DS      KPJBU
045400041213      *
045500050126      * Se pgm Guida non è stato richiamato posso chiudere in LR
045600090331     C     OPZL77        IFEQ      *BLANK
045700050126     C                   SETON                                            LR
045800050126      **altrimenti chiudo in RT
045900050126     C                   ELSE
046000050126     C                   SETON                                            RT
046100050126     C                   ENDIF
046200041213     C                   RETURN
046300041213      *
046400041213     C     XEndPgm       ENDSR
046500041213      ************************************************************
046600041213      * RICERCHE
046700041213      ************************************************************
046800041213     C     SEARCH        BEGSR
046900041213      *
047000041213      * determino Riga/Colonna del cursore
047100041213     C                   MOVE      NRG           R$$
047200041213     C                   MOVE      NCL           C$$
047300041213     C                   Z-ADD     RIRI          CURR
047400041213     C                   Z-ADD     COCO          CURC
047500041213      *
047600041213     C                   SELECT
047700041213      * ricerca non abilitata in questa posizione
047800041213     C                   OTHER
047900041213     C                   SETON                                        9899
048000041213      *
048100041213     C                   ENDSL
048200041213      *
048300041213      * imposto pos. del cursore
048400041213     C                   Z-ADD     CURR          H1RIGA
048500041213     C                   Z-ADD     CURC          H1COLO
048600041213      *
048700041213     C                   MOVE      *HIVAL        $LASTV
048800041213      *
048900041213     C     XSearch       ENDSR
049000041213      ************************************************************
049100041213      * RIEMPIMENTO OPZIONI
049200041213      ************************************************************
049300090331     C     RIEOPZs1      BEGSR
049400041213      *
049500041213      * Input :
049600041213      * - $TX = schiera con tutti i testi disponibili
049700041213      * - $FL = schiere con i flag di validità associati ai testi
049800041213      * - WCOZ = variabile contenente l'indice dell'ultimo testo
049900041213      *          opzione usato
050000041213      * - WMXO = variabile contenente il numero massimo di testi
050100041213      *          opzione usabili
050200041213      * - $L01 = Lunghezza prima riga
050300041213      * - $L02 = Lunghezza seconda riga
050400041213      * Output :
050500041213      * - WOPZ1 = variabile contenente la prima riga opzioni
050600041213      * - WOPZ2 = variabile contenente la seconda riga opzioni
050700041213      * - FF23  = flag che indica se c'è bisogno del tasto F23
050800041213      *
050900041213     C                   CALLB     'X04OPMSG'
051000041213      * input
051100041213     C                   PARM                    $TX
051200041213     C                   PARM                    $FL
051300041213     C                   PARM                    WCOZ              3 0
051400041213     C                   PARM                    WMXO              3 0
051500041213      * output
051600041213     C                   PARM                    WOPZ1            76
051700041213     C                   PARM                    WOPZ2            76
051800041213     C                   PARM                    FF23              1
051900041213      * input
052000041213     C                   PARM                    $L01              3 0
052100041213     C                   PARM                    $L02              3 0
052200041213      *
052300090331     C                   PARM                    $ALLOPT         960
052400090331     C*
052500090331     C     XRieOpzS1     ENDSR
052600090331      ************************************************************
052700090331      * RIEMPIMENTO OPZIONI
052800090331      ************************************************************
052900090331     C     RIEOPZs2      BEGSR
053000090331      *
053100090331      * Input :
053200090331      * - $TX = schiera con tutti i testi disponibili
053300090331      * - $FL = schiere con i flag di validità associati ai testi
053400090331      * - WCOZ = variabile contenente l'indice dell'ultimo testo
053500090331      *          opzione usato
053600090331      * - WMXO = variabile contenente il numero massimo di testi
053700090331      *          opzione usabili
053800090331      * - $L01 = Lunghezza prima riga
053900090331      * - $L02 = Lunghezza seconda riga
054000090331      * Output :
054100090331      * - WOPZ1 = variabile contenente la prima riga opzioni
054200090331      * - WOPZ2 = variabile contenente la seconda riga opzioni
054300090331      * - FF23  = flag che indica se c'è bisogno del tasto F23
054400090331      *
054500090331     C                   CALLB     'X04OPMSG'
054600090331      * input
054700090331     C                   PARM                    $TX
054800090331     C                   PARM                    $FL
054900090331     C                   PARM                    WCOZ              3 0
055000090331     C                   PARM                    WMXO              3 0
055100090331      * output
055200090331     C                   PARM                    WOPZ1            76
055300090331     C                   PARM                    WOPZ2            76
055400090331     C                   PARM                    FF23              1
055500090331      * input
055600090331     C                   PARM                    $L01              3 0
055700090331     C                   PARM                    $L02              3 0
055800090331      *
055900090331     C                   PARM                    $ALLOPT         960
056000090331     C*
056100090331     C     XRieOpzs2     ENDSR
056200041213      ************************************************************
056300041213      * RIEMPIMENTO TASTI FUNZIONE
056400041213      ************************************************************
056500041213     C     RIEKEY        BEGSR
056600041213      *
056700041213      * Input :
056800041213      * - $FX = schiera con tutti i testi disponibili
056900041213      * - $FC = schiere con i flag di validità associati ai testi
057000041213      * - $FR = schiere con i flag di REVERSE IMAGE
057100041213      * - $ULK = ultimo testo caricato
057200041213      * - $L01 = Lunghezza prima riga
057300041213      * - $L02 = Lunghezza seconda riga
057400041213      * Output :
057500041213      * - $ULK
057600041213      * - $KEY1= 1° riga di tasti funzione
057700041213      * - $KEY2= 2° riga di tasti funzione
057800041213      *
057900041213     C                   CALLB     'XKEYMSG'
058000041213      * input
058100041213     C                   PARM                    $FX
058200041213     C                   PARM                    $FC
058300041213     C                   PARM                    $FR
058400041213     C                   PARM                    $ULK              3 0
058500041213      * output
058600041213     C                   PARM                    $KEY1            79
058700041213     C                   PARM                    $KEY2            79
058800041213      * input
058900041213     C                   PARM                    $L01              3 0
059000041213     C                   PARM                    $L02              3 0
059100041213      *
059200090331     C                   PARM                    $ALLFUNCT       480
059300090331     C*
059400041213     C     XRieKey       ENDSR
059500041213      ************************************************************
059600041213      * GESTIONE EMISSIONE TASTI FUNZIONE
059700041213      ************************************************************
059800041213     C     GESPIE        BEGSR
059900041213      *
060000041213     C                   SELECT
060100041213      * Parzializzazioni
060200090401     C     $GEST         WHENEQ    'D1'
060300090401     C     $GEST         oreq      'S1'
060400090331     C     $GEST         oreq      'S2'
060500090331     C     $ULKD1        IFEQ      0
060600041213      * riempimento opzioni
060700090331     C                   EXSR      F24D1
060800041213     C                   ELSE
060900041213     C                   WRITE     FMTZ1
061000041213     C                   ENDIF
061100041213     C                   ENDSL
061200041213      *
061300041213     C     XGesPie       ENDSR
061400090401      ************************************************************
061500090401      * GESTIONE PARZIALIZZAZIONI
061600090401      ************************************************************
061700090401     C     GESD1         BEGSR
061800090401      * inizializzazione videata
061900090401     C     $INZD1        IFEQ      *ON
062000090401     C                   EXSR      INZD1
062100090401     C                   MOVE      *OFF          $INZD1
062200090401     C                   ENDIF
062300090401      * emissione piede videata se proveniente da altra
062400090401      * salvataggio valore formato di provenienza
062500090401     C     $LASTV        IFNE      'D1'
062600090401      * salvo stato F17 per sapere cosa fare in caso di F12
062700090401     C                   MOVE      *INKR         WINKR
062800090401     C                   EXSR      GESPIE
062900090401     C                   MOVE      $LASTV        $LASTG
063000090401     C                   MOVE      'D1'          $LASTV
063100090401     C                   ENDIF
063200090401      *
063300090401      * se esistono errori sulla videata
063400090401      * emetto la write del formato a indicatori spenti per vedere
063500090401      * le eventuali decodifiche
063600090401     C     *IN99         IFEQ      *ON
063700090401     C                   MOVEA     *IN           WIN
063800090401     C                   MOVE      *ALL'0'       IN5098           49
063900090401     C                   MOVEA     IN5098        *IN(50)
064000090401     C                   WRITE     FMTD1
064100090401     C                   MOVEA     WIN           *IN
064200090401     C                   ENDIF
064300090408      * Emissione sfl
064400090408      * S1
064500090408     c                   write     FmtC1
064600090408     C                   if        WMaxS1 = 0
064700090408     C                   write     FmtD3
064800090408     C                   endif
064900090408      * S2
065000090408     C                   write     FmtC2
065100090408     C                   if        WMaxS2 = 0
065200090408     C                   write     FmtD4
065300090408     C                   endif
065400090401      *
065500090401     C                   exfmt     FMTD1
065600090401      *
065700090401      * azzero la posizione del cursore
065800090401     C                   Z-ADD     0             H1RIGA
065900090401     C                   Z-ADD     0             H1COLO
066000090401      *
066100090401      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
066200090401      * INPUT
066300090401      * $FC1 = Abilitazioni videata D1
066400090401      * OUTPUT
066500090401      * *IN97 + *IN99 Errori
066600090401     C                   MOVEA     $FC1          $FC
066700090401     C                   EXSR      CTRKEY
066800090401      *
066900090401     C     *IN99         IFEQ      *OFF
067000090401      *
067100090401     C                   SELECT
067200090401      * F3=Fine
067300090401     C     $TASTO        WHENEQ    F03
067400090401     C                   EXSR      F03D1
067500090401      * F8=Successivo
067600090401     C     $TASTO        WHENEQ    F08
067700090401     C                   EXSR      F08D1
067800100416      * F10=Gestione fasi scadenze
067900090402     C     $TASTO        WHENEQ    F10
068000090402     C                   EXSR      F10D1
068100090401      * F15=Interrogazione conto
068200090401     C     $TASTO        WHENEQ    F15
068300090401     C                   EXSR      F15D1
068400090401      * F12=Ritorno
068500090401     C     $TASTO        WHENEQ    F12
068600090401     C                   EXSR      F12D1
068700090401      * F24=Altri tasti
068800090401     C     $TASTO        WHENEQ    F24
068900090401     C                   EXSR      F24D1
069000090401      *
069100090401     C                   OTHER
069200090401     C                   EXSR      CTRD1
069300090401      *
069400090401      * determino Riga/Colonna del cursore
069500090401     C                   MOVE      NRG           R$$
069600090401     C                   MOVE      NCL           C$$
069700090401     C                   Z-ADD     RIRI          CURR
069800090401     C                   Z-ADD     COCO          CURC
069900090401     C                   Z-ADD     CURR          H1RIGA
070000090401     C                   Z-ADD     CURC          H1COLO
070100090401      *
070200090401     c* se la riga del cursore è superiore a 11, gestione sfl
070300090401     c                   IF        H1riga >=11
070400090401     C                   if        H1Colo < 67
070500090401     c                   eval      $gest='S1'
070600090401     C                   else
070700090401     c                   eval      $gest='S2'
070800090401     C                   endif
070900090401     C                   endif
071000090401      *
071100090401     C                   ENDSL
071200090401      *
071300090401     C                   ENDIF
071400090401      *
071500090401     C     XGesD1        ENDSR
071600041213      ************************************************************
071700041213      * INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
071800041213      ************************************************************
071900041213     C     INZD1         BEGSR
072000041213      *
072100041213     C                   CLEAR                   FMTD1
072200090401     c                   eval      D1Divisa = xscdiv
072300051115     c*
072400041213      * Se pgm  richiamato con parametri forniti , li imposto nelle
072500041213      *    parzializzazioni
072600041213     C                   SELECT
072700041213      * il pgm è richiamato e
072800041213      * sono stati passati dei valori di parzializzazione ma non
072900041213      * la chiave univoca
073000090331     C     OPZL77        WHENNE    *BLANK
073100090331     C* imposto i valori passati sui campi a video
073200090331    >C                   MOVE      KccL77        D1kcc
073300090331    >C                   MOVE      KScL77        D1Ksc
073400041213      *
073500041213      * il pgm non è richiamato
073600090331     C     OPZL77        WHENEQ    *BLANK
073700090331     C* imposto dei valori di default
073800050110      *
073900041213     C                   ENDSL
074000090407     C* Conto
074100090415     c* dati recupero credito (vanno per primi per reperire la fase)
074200090415     C                   exsr      DatiRecCred
074300090407     c* dati anagrafici
074400090407     C                   exsr      DatiAnagr
074500090407     c* dati scadenzario
074600090407     C                   exsr      DatiScad
074700140603      *
074800041213      * Attributi video
074900041213     C                   exsr      AtrD1
075000041213      *
075100041213     C     XInzD1        endsr
075200090407     C*----------------------------------------------------------
075300090407     C* Dati anagrafici conto
075400090407     C*----------------------------------------------------------
075500090407     C     DatiAnagr     BEGSR
075600090407     C*
075700090407     C* ANAGRAFICI
075800090407     C                   CLEAR                   D1Descr
075900090407     C                   CLEAR                   D1Daticli
076000090407     c*
076100090407     c                   if        d1kcc=*blanks
076200090407     c                   eval      d1kcc='C'
076300090407     c                   endif
076400090407     C     D1kcc         IFNE      'C'
076500090407     C     D1kcc         ANDNE     'F'
076600090407     C                   CALL      'XALLINEA'
076700090407     C                   PARM                    D1kcc
076800090407     C                   PARM      6             LEN               5 0
076900090407     C                   PARM                    XSCINP
077000090407     C                   ENDIF
077100090407     C*
077200090407     C                   CALL      'XALLINEA'
077300090407     C                   PARM                    D1KSC
077400090407     C                   PARM      8             LEN               5 0
077500090407     C                   PARM                    XSCINP
077600090407     C*
077700090407     C                   MOVE      D1KCC         $kcc
077800090407     C                   MOVE      D1KSC         $ksc
077900090407     C*
078000090407     C* reperisco capoconto se cliente/fornitore
078100090407     C     D1KCC         IFEQ      'C       '
078200090407     C     D1KCC         OREQ      'F       '
078300090407     C                   MOVE      XSCSOC        XCAPSOCIET        3
078400090407     C                   MOVE      D1KCC         XCAPKCC           6
078500090407     C                   MOVE      D1KSC         XCAPKSC           8
078600090407     C                   CALLB     'XCAPCLIFOR'
078700090407     C                   PARM                    XCAPSOCIET
078800090407     C                   PARM                    XCAPKCC
078900090407     C                   PARM                    XCAPKSC
079000090407     C                   MOVE      XCAPKCC       D1KCC
079100090407     C                   MOVE      XCAPKCC       $kcc
079200090407     C                   MOVE      XCAPKsC       $ksc
079300090407     C                   ENDIF
079400090407     C*
079500090407     C* Richiamo modulo controllo conto
079600090407     C                   EXSR      CALLMVC002
079700090407     C* Errore
079800090416B1   C     $Esito        IFne      *ON
079900090407     C                   MOVEL     Descr002      D1Descr
080000100513     c                   if        PosAttr057 <> *blanks
080100100513     c                   movel     Categorie     DsCateg
080200100513     c                   eval      WrkAttr  = SkCateg(Posiz)
080300100513     c                   endif
080400090407     c* indirizzo
080500090407     C                   MOVE      '1'           DVARIC
080600090407     C                   MOVE      xscsoc        DVASOCIETA
080700090414     C                   MOVE      UnitaL77      DVAUnita
080800090407     C                   MOVE      'C'           DVASNATURA
080900090407     C                   MOVE      tpind002      DVATPIND
081000090407     C                   MOVE      cdind002      DVACDIND
081100090407     C                   MOVE      kscL77        DVAcodice
081200090407     C                   MOVEL     'DVACLI'      DVAstrutt
081300090407     C                   EVAL      DVALenOut = %Size(DVACLI)
081400090407     C*
081500090407     C                   CALLB     'NDDVASOG'    PDVASOG
081600090407     C*
081700090416 b2  C                   if        DVAerrore<>*ON
081800090407     C                   eval      %subst(DVACLI:1:DVALENOUT)
081900090407     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
082000090407     C                   eval      d1indiriz=dvcindriz
082100090407     C                   move      dvccap        d1cap
082200090407     C                   eval      D1localit=dvclocalit
082300090407     C                   eval      D1prv =dvcprov
082301140603      * recupero nome commerciale
082302140603brt  c                   exsr      repcommerciale
082400140314      *
082500140603     c                   eval      d1daticli=%trim(d1descr)+ '  ' +
082600140603     c                             %trim(d1indiriz)        + ' ' +
082700140603     c                             %trim(d1localit)        + ' ' +
082800140603     c                             %trim(d1cap) + ' ' + d1prv +
082801140603     c                             '  Comm: ' + nome_comm
082802140603     c* telefono
083000090415     c                   clear                   Wmsg1
083100090415     c                   eval      Idmsg= Msg(1)
083200090415     c                   exsr      PrendiMsg
083300090415     c                   eval      wmsg1=txtmsg
083400100513     c* attributo
083500140314brt  c                   clear                   Wmsg2
083600140314brt  c***                eval      Idmsg= Msg(2)
083700140314brt  c***                exsr      PrendiMsg
083800140314brt  c***                eval      wmsg2=txtmsg
083900100513     c* decodifica attributo contabile
084000100513     c                   if        Posattr057<> *blanks and
084100100513     c                             wrkattr  <> *blanks
084200100513     C                   MOVE      *ALL'0'       XTBKEY
084300100513     C                   MOVEL     PosAttr057    WRK6              6
084400100513     C                   MOVE      wrkattr       WRK6
084500100513    >C                   MOVE      wrk6          XTBKEY
084600100513     C                   MOVE      '0'           XTBALC
084700100513     C                   MOVE      '1'           XTBRIC
084800100513     C                   MOVE      '0A1'         XTBCOD
084900100513     C                   CALLB     'XATB'
085000100513     C                   PARM                    XATBDS
085100100513     C     XTBERR        IFEQ      '0'
085200100513     C                   MOVEL     XTBUNI        Ang0a1ds
085300100513     C                   MOVEL     DES0a1        wrkattrd         20
085400100513     c                   endif
085500100513     c                   endif
085600140314      * recupero descrizione cond. pag.
085700140314brt  c                   exsr      repcondpag
085701140603     c*
085900090415     c                   eval      d1daticli2=%trim(wmsg1)     +'  ' +
086000140314     c                                        %trim(dvctelefon) +
086100140314     c                             '      Condiz. Pagam. ' +
086200140314     c                             dvccondpag + ' ' + %trim(cdpdesbrev) +
086300140530     c                             '  Blocco Amm. ' + bkamm002  +
086400140530     c                             '  Stato Cred. ' + ctgan03002 +
086401140530     c                             '  Class.ABC ' + dvcclasabc
086500140314brt  c***                                     %trim(wmsg2)     +'  ' +
086600140314brt  c***                                     %trim(wrkattr)   +'  ' +
086700140314brt  c***                                     %trim(wrkattrD)  +'  '
086800090415 e2  c                   endif
086900090407     C*
087000090407 e1  C                   ENDIF
087100090416     C* dati unità
087200090416     c                   clear                   WUnita
087300090416     c                   clear                   Wmsg3
087400090416     c                   eval      Idmsg= Msg(3)
087500090416     c                   exsr      PrendiMsg
087600090416     c                   eval      wmsg3=txtmsg
087700090416     c                   clear                   Desunita         20
087800090416     C                   If        UnitaL77 <> *blanks
087900090416     C                   MOVE      xscsoc        UNISOCieta
088000090416     C                   MOVE      UnitaL77      UNIUNIta
088100090416     C     K02Uni01      CHAIN     ANUNI01L
088200090416     c                   if        %Found(Anuni01L)
088300090416     c                   eval      DesUnita=UniDesBrev
088400090416     c                   endif
088500090416     c                   else
088600090416     c* tutte le unità
088700090416     c                   clear                   Wmsg4
088800090416     c                   eval      Idmsg= Msg(4)
088900090416     c                   exsr      PrendiMsg
089000090416     c                   eval      wmsg4=txtmsg
089100090416     c                   eval      DesUnita=wmsg4
089200090416     c                   endif
089300090416     c*
089400090416     C                   eval      Wunita = %trim(wmsg3) +' '+ %trim(unital77)+
089500090416     c                             ' ' + DesUnita
089600090416     c
089700140314brt  c******             move      WUnita        D1DatiCli2
089800090407     C*
089900090407     c                   ENDSR
089901140603     C************************************************************
089902140603     C* Recupero dati cond. di pagamento
089903140603     C************************************************************
089904140603     C     RepCommercialeBEGSR
089905140603      *
089906140603     c                   clear                   nome_comm        25
089907140603     c                   move      kscL77        wrk_ksc           7 0
089908140603     c                   move      kccL77        wrk_kcc           4 0
089909140603      *
089910140603     C/EXEC SQL
089911140603     c+  SELECT cmmdes   into :nome_comm
089912140603     c+  from cnclp00f  join azcmm00f on clpage =cmmcod
089913140603     c+   where
089914140603     c+   1 = clpkut and clpkcc = :wrk_kcc
089915140603     c+  and clpksc= :wrk_ksc and clpage<>0
089916140603     C/END-EXEC
089917140603      *
089918140603     C                   endsr
090000140314     C************************************************************
090100140314     C* Recupero dati cond. di pagamento
090200140314     C************************************************************
090300140314     C     RepCondPag    BEGSR
090400140314     C*
090500140314     C                   Reset                   DVCDPDS
090600140314     C*
090700140314     C                   Eval      DCPRIC      = '1'                            chain
090800140314     C                   Eval      DCPSOC      = XScSoc
090900140314     C                   Eval      DCPUNI      = unital77
091000140314     C                   Eval      DCPVIS      = '1'                            lf principale
091100140314     C                   Eval      DCPALC      = *Off                           no allocazione
091200140314     C                   Eval      DCPNRK      = 1                              numero chiavi
091300140314     C                   Eval      DCPLIN      = XScLin
091400140314     C                   Eval      DCPCOD      = DvcCondPag
091500140314     C                   Eval      DCPERR      = *Off
091600140314     C*
091700140314     C                   CALL      'DVCDP'
091800140314     C                   PARM                    DVCDPDS
091900140314     C                   PARM                    DVOCDPDS
092000140314     C*
092100140314     C                   if        DCPErr = *On
092200140314     C                   Eval      CDPDesBrev = 'ERRORE Descrizione'
092300140314     C                   endif
092400140314     C*
092500140314     C                   ENDSR
092600090407     C*----------------------------------------------------------
092700090407     C* Dati recupero credito
092800090407     C*----------------------------------------------------------
092900090407     C     DatiRecCred   BEGSR
093000090407     C*
093100090416     C* dati recupero credito
093200090407     c                   reset                   NDCL99DS
093300090407     c                   eval      societal99=xscsoc
093400090414     c                   eval      UnitaL99 =Unital77
093500090407     c                   eval      ctbl99='CG'
093600090407     c                   eval      kccl99=KccL77
093700090407     c                   eval      kscl99=kscL77
093800090421     c                   eval      SolleciL99=*on
093900090407     c                   eval      ImpegniL99=*on
094000090407     c                   eval      ContenzL99=*on
094100090407     c                   eval      RatingL99 =*on
094200090407     c                   eval      BlocchiL99=*off
094300090407     c                   eval      UltimoSL99=*on
094400090415     c                   eval      GiorniL99 =*on
094500090415     c                   eval      DtRifL99=zdateiso
094600090407     c                   eval      kpjbu=ndcl99ds
094700090407     C*
094800090407     c                   CALL      'NDCL99R'
094900090407     C                   parm                    kpjba
095000090407     c                   eval      ndcl99ds = kpjbu
095100090407     c                   if        UltimoSL99=*on
095200090407     c     *jobrun       move      DtUlSolL99    d1DPs
095300090407     c                   z-add     NrUlSolL99    d1NPs
095400090407     c                   move      GravsolL99    D1Fase
095500090407     c                   z-add     ImpsolL99     D1ImpSoll
095600090409     c                   endif
095700090407     c                   move      UltRatL99     D1Rating
095800090407     c                   movel     UltRatDL99    D1RatingD
095900090407     c                   move      KccL99        KccContenz        6
096000090407     c                   z-add     ImpImpL99     D1TotImp
096100090407     c                   z-add     ImpContL99    D1TotCont
096200090415     c                   z-add     GGConL99      D1GGCon
096300090415     c                   z-add     GGRitL99      D1GGRit
096400090512     c* indicatori per visualizzare o meno le costanti relative
096500090512     c                   if        d1Rating <> *blanks
096600090512     c                   seton                                        11
096700090512     c                   endif
096800090512     c                   if        d1GGcon  <> 0  or D1GgRit <> 0
096900090512     c                   seton                                        12
097000090512     c                   endif
097100090512     c                   if        d1TotImp <> 0
097200090512     c                   seton                                        13
097300090512     c                   endif
097400090512     c                   if        d1TotCont<> 0
097500090512     c                   seton                                        14
097600090512     c                   endif
097700090407     c*
097800090407     C                   ENDSR
097801140603     C*----------------------------------------------------------
097802140603     C* Rep Prom
097803140603     C*----------------------------------------------------------
097804140603     C     RepProm       BEGSR
097805140603      *
097807140603     c                   clear                   wrk19            19 3
097808140603      *
097811140603     C/EXEC SQL
097812140603     C+   SELECT sum(SCDIMPRMC) into :wrk19
097813140603     C+   FROM ndscd00f
097814150126     C+   WHERE scdtitoli <> '' and
097815140603     C+   SCDCAURISC=' ' and SCDKCC = :kccl77
097816140603     C+   and SCDKSC = :kscl77 and SCDSOCIETA = :xscsoc
097817150126     C+   and SCDTPSCAD ='0' and scdctb = 'CG'
097818140603     C/END-EXEC
097819140603      *
097820140603     C                   eval       d1totprom = wrk19
097821140603     c                   if        d1TotProm<> 0
097822140603     c                   seton                                        15
097823140603     c                   endif
097824140603      *
097825140603     C                   ENDSR
097826140603     C*----------------------------------------------------------
098000090407     C* Dati scadenzario
098100090407     C*----------------------------------------------------------
098200090407     C     DatiScad      BEGSR
098300090408     c* dati tabella periodi
098400090408     C                   Reset                   ANG0CfDS
098500090408     C                   Move      XScSoc        XTbAzi
098600090408     C                   Move      XScLin        XTbLin
098700090408     C                   Move      '0CF'         XTbCod
098800090408     C                   Move      *Zeros        XTbKey
098900090408     C                   Move      TabAtt601     XTbKey
099000090408     C                   Move      '1'           XTbRic
099100090408     C                   Move      '0'           XTbAlc
099200090408     C                   Callb     'XATB'
099300090408     C                   Parm                    XAtbDS
099400090408     C                   If        XTbErr = '0'
099500090408     C                   Movel     XTbUni        ANG0CFDS
099600090408     c                   z-add     p1a0cf        d1gg1
099700090408     c                   z-add     p2a0cf        d1gg2
099800090408     c                   z-add     p3a0cf        d1gg3
099900090408     c                   z-add     p4a0cf        d1gg4
100000090408     c                   z-add     p5a0cf        d1gg5
100100090408     c                   z-add     p6a0cf        d1gg6
100200090408     c                   z-add     p7a0cf        d1gg7
100300090408     c                   z-add     p8a0cf        d1gg8
100400090408     c                   endif
100500090408     c*
100600090407     c                   clear                   WDare
100700090407     c                   clear                   WAvere
100800090407     C*
100900090407     C                   RESET                   x20scadds
101000090407     C                   RESET                   x20scadXds
101100090407     C                   eval      x20societa = SocietaL77
101200090416     C                   eval      x20Unita   = UnitaL77
101300090407     C                   eval      x20ctb     = 'CG'
101400090407    >C                   eval      x20def     = *on
101500090407    >C                   eval      x20prov    = *on
101600090407    >C                   eval      x20gest    = *on
101700090407     C                   eval      x20dtregmd = ZDateIso
101800090407     C                   eval      x20dtregmp = *hival
101900090407     C                   eval      x20dtregmg = ZDateIso
102000090407     C                   eval      x20kcc     =  kccL77
102100090407     C                   eval      x20ksc     =  kscL77
102200090407     C                   eval      x20snatura = 'C'
102300090407     C                   eval      x20parsca  = 'S'
102400090407     C                   eval      X20ParScd  = *blank
102500090407     C                   eval      x20elasca  = 'R'
102600090407     C                   eval      x20DtSca   = ZdateIso
102700090407     C                   eval      x20ppa     = 'S'
102800090407     C                   eval      x20lrt     = 'R'
102900090407     c                   eval      x20tabCF   = TabAtt601
102901140604brt  c                   if        wrtx20l77 = *on
102902140604brt  c                   eval      x20write ='R'
102903140604brt  c                   else
102904140604brt  c                   eval      x20write = *blanks
102905140604brt  c                   endif
102906140604brt  c                   eval      x20feod = *blanks
102907140604     C                   CALL      'X20SCADR'
103100090407     C                   PARM                    X20SCADDS
103200090407     C                   PARM                    X20SCADXDS
103300090407     c* 8 periodi
103400090407     c                   z-add     X20Prd01      D1Scad1
103500090407     c                   z-add     X20Prd02      D1Scad2
103600090407     c                   z-add     X20Prd03      D1Scad3
103700090407     c                   z-add     X20Prd04      D1Scad4
103800090407     c                   z-add     X20Prd05      D1ASca1
103900090407     c                   z-add     X20Prd06      D1ASca2
104000090407     c                   z-add     X20Prd07      D1ASca3
104100090407     c                   z-add     X20Prd08      D1ASca4
104200090407     c* rischio
104300090407     c                   z-add     x20Risc       D1Rischio
104400140313     c* tot.scadu
104500140313     c                   z-add     x20lScadu     D1totscad
104600140320     c* tot.a scad.
104700140320     c                   z-add     x20laScad     D1totAscad
104800090407     c* saldo
104900090407     c                   add       x20tDlD       WDare
105000090407     c                   add       x20tDlP       WDare
105100090407     c                   add       x20tDlG       WDare
105200090407     c                   add       x20tAlD       WAvere
105300090407     c                   add       x20tAlP       WAvere
105400090407     c                   add       x20TAlG       WAvere
105500090407     C*
105600090407     c* TROVO SALDO
105700090407     c* mc
105800090407     c     WDare         sub       WAvere        d1Saldo
105801140603     c*
105803140603     c* dati prom
105804140603     C                   exsr      RepProm
105805140603     c*
106000090407     C                   ENDSR
106100090407     C*----------------------------------------------------------
106200090407     C* Chiamata NDMVC002
106300090407     C*----------------------------------------------------------
106400090407     C     CallMvc002    BEGSR
106500090407     C*
106600090407     C* Controllo conto
106700090407     C                   Clear                   ND002DS
106800090407     C                   EVAL      $LenOut = %Size(ND002DS)
106900090407     C                   CallB     'NDMVC002'
107000090407     C                   PARM                    xscsoc
107100090407     C                   PARM                    $kcc
107200090407     C                   PARM                    $ksc
107300090407     C                   PARM                    WDATE
107400090407     C                   PARM      *OFF          $GesMsg
107500090407     C                   PARM      *OFF          $Esito
107600090407     C                   PARM      'ND002DS'     $Struttura
107700090407     C                   PARM                    ND002DS
107800090407     C                   PARM                    $LenOut
107900090407     C*
108000090407     C                   ENDSR
108100041213      ************************************************************
108200041213      * ATTRIBUTI D1
108300041213      ************************************************************
108400041213     C     AtrD1         Begsr
108500041213      *
108600041213     C     XAtrD1        Endsr
108700090401      ************************************************************
108800090401      * GESTIONE F03 VIDEO D1
108900090401      ************************************************************
109000090401     C     F03D1         BEGSR
109100090401      *
109200090401     C                   MOVE      *ON           $FINE
109300090401     C                   MOVE      '1'           RETL77
109400090401      * fine programma
109500090401     C                   EXSR      ENDPGM
109600090401      *
109700090401     C     XF03D1        ENDSR
109800090401      ************************************************************
109900090401      * GESTIONE F08 VIDEO D1
110000090401      ************************************************************
110100090401     C     F08D1         BEGSR
110200090401      *
110300090401     C                   MOVE      *ON           $FINE
110400090401     C                   MOVE      '0'           RETL77
110500090401      *
110600090401     C     XF08D1        ENDSR
110700090402      ************************************************************
110800090409      * GESTIONE F10 VIDEO C1/C2
110900090402      ************************************************************
111000090402     C     F10D1         BEGSR
111100100421     c* controllo autorizzazione
111200100421     c                   if        immatt = *off
111300100421     C                   SETON                                        6299
111400100421     C                   LEAVESR
111500100421     c                   endif
111600100416     C*
111700100416      * controllo che ci sia la data scadenza
111800100419     c                   if        Seldtsca=*loval
111900100416     c                   seton                                        6099
112000100416     C                   LEAVESR
112100100416     c                   endif
112200100419      * controllo che l'ultima fase sia chiusa
112300100419     c                   if        UltStato =*blanks
112400100419     c                   seton                                        6199
112500100419     C                   LEAVESR
112600100419     c                   endif
112700100416     C*
112800100416     c                   reset                   ndcl69ds
112900100416     c                   eval      SocietaL69=societaL77
113000100416     c                   eval      posizL69  =posattr057
113100100416     c                   eval      kccl69    =kccl77
113200100416     c                   eval      kscl69    =kscl77
113300100416     c                   eval      descrL69  =d1descr
113400100416     c                   eval      DtParL69  =DtParL77
113500100416     c                   eval      NrParL69  =NrParL77
113600100416     c                   eval      SerParL69 =SerParL77
113700100419     c                   eval      DtScadL69 =SelDtSca
113800100416     c                   eval      wdwl69=wrkwdwL69
113900100416     c                   eval      kpjbu=ndcl69ds
114000100416     C                   call      'NDCL69R'
114100100416     c                   parm                    kpjba
114200100416     c                   eval      ndcl69ds=kpjbu
114300100416     c                   move      *off          wrkwdwl69         1
114400100416     C* 2 = F12
114500100416    >C                   if        RetL69='2' and
114600100416     c                             OprL69 <> *on
114700100416     C                   move      *ON           $ESCI                          sospese READC
114800100416     c                   endif
114900100416     c* aggiornamento effettuato
115000100416     c                   if        OprL69=*on
115100100416     C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
115200100416     C                   move      *ON           $INZS2                         iniz.sfl fine ciclo
115300100416     c                   endif
115400090402      *
115500100415     c     XF10D1        ENDSR
115600090401      ************************************************************
115700090401      * GESTIONE F12 VIDEO D1
115800090401      ************************************************************
115900090401     C     F12D1         BEGSR
116000090401      *
116100090401     C                   MOVE      *ON           $FINE
116200090401     C                   MOVE      '2'           RETL77
116300090401      *
116400090401     C     XF12D1        ENDSR
116500090401      ************************************************************
116600090401      * GESTIONE F15 VIDEO D1
116700090401      ************************************************************
116800090401     C     F15D1         BEGSR
116900090401      *
117000090401     C                   reset                   ana399ds
117100090401     C                   eval      A39societa = XSCSOC
117200090401     C                   eval      A39unita = UnitaL77
117300090401     C                   eval      A39ctb = 'CG'
117400090401     C                   eval      A39kcc = kccL77
117500090401     C                   eval      A39ksc = kscl77
117600090401     C                   movel     ana399ds      kpjbu
117700090401     C                   CALL      'ANA399R'                            21
117800090401     C                   PARM                    KPJBA
117900090401     C     A39ERRORI     IFEQ      '0'
118000090401     C                   endif
118100090401     C                   MOVE      *HIVAL        $LASTV
118200090414     c* rireperisco dati recupero credito perchè potrebbero essere cambiati
118300090414     C                   exsr      DatiRecCred
118400090401      *
118500090401     C     XF15D1        ENDSR
118600140604      ************************************************************
118601140604      * GESTIONE F16 VIDEO D1
118602140604      ************************************************************
118603140604     C     F16D1         BEGSR
118604140604      *
118605140604     C                   reset                   tnta12ds
118606140604     C                   eval      TA12RIC = 'C'
118607140604     C                   eval      TA12APL = 'C'
118608140604     C                   eval      TA12KSC = %dec(KSCL77:7:0)
118609140604     C                   eval      TA12POT = 0
118610140604     C                   eval      TA12NRV = 0
118611140604     C                   eval      TA12RAG = cdpdesbrev
118612140604     C                   eval      TA12TNT = '50'
118613140604     C                   eval      TA12NOTE = *blanks
118614140604     C                   eval      TA12GCLI = '0'
118615140604     C                   eval      TA12ICLI = '0'
118616140604     C                   eval      TA12CMM = 0
118617140604      *
118618140604     C                   Call      'TNTA12R'
118619140604     C                   Parm                    kpjba
118620140604     C                   Parm                    tnta12ds
118621140604      *
118622140604     C                   MOVE      *HIVAL        $LASTV
118634140604      *
118637140604     C     XF16D1        ENDSR
118638140604      ************************************************************
118700041213      * GESTIONE F24 VIDEO D1
118800041213      * input
118900041213      * $ULKD1 = Ultimo tasto funzione emesso per video D1
119000041213      * $FX1 =Testi tasti funzione
119100041213      * $FC1 = Abilitazioni
119200041213      * $FR1 = Inversioni di fondo
119300041213      ************************************************************
119400041213     C     F24D1         BEGSR
119500041213      *
119600041213     C                   MOVEA     $FX1          $FX
119700041213     C                   MOVEA     $FC1          $FC
119800041213     C                   MOVEA     $FR1          $FR
119900041213     C                   Z-ADD     $ULKD1        $ULK
120000041213     C                   Z-ADD     $D1L01        $L01
120100041213     C                   Z-ADD     $D1L02        $L02
120200041213     C                   EXSR      RIEKEY
120300041213     C                   Z-ADD     $ULK          $ULKD1            3 0
120400041213     C                   MOVEA     $FC           $FC1
120500090331     C                   MOVEL     $KEY1         Z1KE
120600090331     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
120700041213     C                   MOVEL     $KEY2         Z1KE2
120800041213     C                   WRITE     FMTZ1
120900090331     c* riga unica tasti
121000090331     c                   eval      $A = %triml(z1ke)
121100090331     c                   eval      $B = %triml(z1ke2)
121200090331     c     $A            cat(P)    $B:0          z1ke1
121300090331     C                   WRITE     FMTZ1
121400041213      *
121500041213      *
121600041213     C     XF24D1        ENDSR
121700041213      ************************************************************
121800041213      * CONTROLLO VIDEATA PARZIALIZZAZIONI
121900041213      * non usare i seguenti indicatori (perchè relativi a D1MSG):
122000041213      * 93 94 96 97 98
122100041213      ************************************************************
122200041213     C     CTRD1         BEGSR
122300041213      *
122400041213     C                   SETOFF                                       99
122500090511     C                   Endsr
122600051115     C/EJECT
122700041213      ************************************************************
122800041213      * DEFINIZIONE DELL'ORDINAMENTO
122900041213      ************************************************************
123000041213     C     DEFORD        BEGSR
123100041213      *
123200041213     C                   Z-ADD     1             $ORD
123300041213      *
123400041213     C     XDefOrd       ENDSR
123500041213      ************************************************************
123600041213      * GESTIONE LISTA
123700100416     C************************************************************
123800041213     C     GESS1         BEGSR
123900041213      * inizializzazione videata
124000041213     C     $INZS1        IFEQ      *ON
124100041213     C                   EXSR      INZS1
124200041213     C                   MOVE      *OFF          $INZS1
124300041213     C                   ENDIF
124400041217     C     $INZS2        IFEQ      *ON
124500041217     C                   EXSR      INZS2
124600041217     C                   MOVE      *OFF          $INZS2
124700041217     C                   ENDIF
124800041213      * emissione piede videata se proveniente da altra
124900041213      * salvataggio valore formato di provenienza
125000041217     C                   if        $LastV <> 'S1' and
125100041217     C                             $LastV <> 'S2'
125200041213     C                   WRITE     FMTT1
125300090331     c                   Write     FMTD1
125400041213     C                   EXSR      GESPIE
125500041213     C                   MOVE      $LASTV        $LASTG
125600041213     C                   MOVE      'S1'          $LASTV
125700041213     C                   ENDIF
125800041213      *
125900041217      * S2
126000090401     C                   write     FmtC2
126100041217     C                   if        WMaxS2 = 0
126200041217     C                   write     FmtD4
126300041217     C                   endif
126400041217      * S1
126500041217     C                   if        WMaxS1 = 0
126600090401     c                   write     FmtC1
126700041217     C                   write     FmtD3
126800041217     C                   endif
126900041213      * Inizializzazione indice tasti funzione personalizzati
127000041213     C                   Eval      PosFun = 25
127100041217      *
127200100416     c* apertura windows
127300100416     C                   Eval      wrkwdwl69=*on
127400041213     C                   EXFMT     FMTC1
127500041213      *
127600041213      * azzero la posizione del cursore
127700041213     C                   Z-ADD     0             H1RIGA
127800041213     C                   Z-ADD     0             H1COLO
127900041213      *
128000041213     C     C1NRR         IFNE      0
128100041217     C                   Z-ADD     C1NRR         WSflS1
128200041213     C                   ENDIF
128300041213     C                   IF        SFLNRR > 0
128400041213     C                   Z-ADD     SFLNRR        C1RCD
128500041213     C                   ENDIF
128600041213      * inizializzazione indice opzioni personalizzate
128700041213     C                   Eval      PosOpz = 0
128800041213      * CONTROLLO ABILITAZIONE TASTI
128900041213      * INPUT
129000041213      * $FC2 = Abilitazioni videata S1
129100041213      * OUTPUT
129200041213      * *IN97 + *IN99 Errori
129300090331     C                   MOVEA     $FC1          $FC
129400041213     C                   EXSR      CTRKEY
129500041213      *
129600041213     C     *IN99         IFEQ      *OFF
129700041213      *
129800041213     C                   SELECT
129900041213      * F3=Fine
130000041213     C     $TASTO        WHENEQ    F03
130100041213     C                   EXSR      F03S1
130200041213      * F8=Successivo
130300041213     C     $TASTO        WHENEQ    F08
130400041213     C                   EXSR      F08S1
130500090511      * F9=Recupero credito
130600090511     C     $TASTO        WHENEQ    F09
130700090511     C                   EXSR      F09S1
130800100415      * F10=Gestione attività
130900090402     C     $TASTO        WHENEQ    F10
131000090402     C                   EXSR      F10D1
131100130916C2226 * F11=Gestione impostazione attività manuali
131200130916  "  C     $TASTO        WHENEQ    F11
131300130916C2226C                   EXSR      F11S1
131400041213      * F12=Ritorno
131500041213     C     $TASTO        WHENEQ    F12
131600041213     C                   EXSR      F12S1
131700090401      * F15=Interrogazione conto
131800090401     C     $TASTO        WHENEQ    F15
131900090401     C                   EXSR      F15D1
132000140604      * F16=Contatto
132001140604     C     $TASTO        WHENEQ    F16
132002140604     C                   EXSR      F16D1
132003140604      * F24=Altri tasti
132100090331     C     $TASTO        WHENEQ    F24
132200090331     C                   EXSR      F24D1
132300041213      * ROLL-UP
132400041213     C     $TASTO        WHENEQ    ROLLUP
132500041213     C                   EXSR      ROLS1
132600041213      * Personalizzata
132700041213     C     $PGF2(PosFun) Whenne    *BLANK
132800041213     C                   EXSR      FperS1
132900041213      *
133000041213     C                   OTHER
133100041221      *
133200041221      * CONTROLLI su entrambi i sfl
133300041222      *   il prossimo sfl da gestire sarà quello in errore
133400041222      *   oppure quello sul quale è il cursore
133500041221     C                   eval      WGest = $Gest
133600041221     C                   exsr      CtrS
133700041221     C                   eval      $Gest = WGest
133800041221      *
133900041221     C                   ENDSL
134000041221      *
134100041213     C                   ENDIF
134200041213      *
134300041213     C     XGesS1        ENDSR
134400041213      ************************************************************
134500041213      * INIZIALIZZAZIONE LISTA
134600041213      ************************************************************
134700041213     C     INZS1         BEGSR
134800041213      * pulizia SFL
134900041213     C                   SETOFF                                         3031
135000041213     C                   WRITE     FMTC1
135100041213     C                   SETON                                          31
135200041213      *
135300050104      * Ctl
135400050104     C                   exsr      RieC1
135500041213      *
135600041213      * opzioni
135700041217     C                   exsr      InzS1_RieOpz
135800041213      *
135900041213      * CARICAMENTO SFL A PAGINE
136000041217     C                   Z-ADD     0             S1Nrr
136100041217     C                   Z-ADD     0             WMaxS1
136200041213      *
136300041213      * Posizionamento su file pilota
136400041217     C                   clear                   C1Pos
136500041217     C                   exsr      SetAnaS1
136600041213      * ripristino i valori del posizionamento
136700041217     C                   MOVE      W1Pos         C1POS
136800041213      * Lettura file pilota
136900041217     C                   EXSR      RedAnaS1
137000041213      *
137100041213      * se arrivo dalla parzializzazione devo caricare solo la prima pagina
137200041217     C                   if        $LastV = 'D1'
137300041217     C                   z-add     SflPagS1      WPagS1
137400041217     C                   endif
137500041217     C                   exsr      RolS1
137600041213      * imposto il caricamento sfl del numero massimo di righe per pagina
137700041217     C                   z-add     SflPagS1      WPagS1
137800041213      *
137900041213     C     XInzS1        ENDSR
138000050104      ************************************************************
138100050104      * Caricamento C1
138200050104      ************************************************************
138300050104     C     RieC1         Begsr
138400050104      *
138500100409     c     *jobrun       move      DtParL77      c1Dtpar
138600100409     c                   z-add     NrParL77      C1NrPar
138700100409     c                   move      SerParL77     C1SerPar
138800100419     c                   move      DivisaL77     C1Divisa
138900050104      *
139000050104     C     XRieC1        Endsr
139100041217      ************************************************************
139200041217      * Caricamento opzioni S1
139300041217      ************************************************************
139400041217     C     InzS1_RieOpz  Begsr
139500041217      *
139600041217     C                   MOVEA     $TX1          $TX
139700041217     C                   MOVEA     $FL1          $FL
139800041217     C                   Z-ADD     0             WCOZ
139900041217     C                   Z-ADD     0             WMXO
140000041217      *  equivale al num.di elementi della schiera $TX1
140100100419     C                   Z-add     OpzLen1       WMXO
140200041217     C                   Z-ADD     $C1Op1        $L01
140300090331     C                   Z-ADD     0             $L02
140400100419     c                   clear                   Wopz1
140500090331     C                   EXSR      RIEOPZS1
140600041217     C                   Z-ADD     23            X
140700090331     C                   MOVE      FF23          $FC1(X)
140800041217     C                   MOVEL     WOPZ1         C1OPZ1
140900090331     C                   MOVEL     $ALLOPT       H1ALLOPT
141000041217     C                   Z-ADD     WCOZ          C1COZ
141100041217      *
141200041217     C     XInzS1_RieOpz Endsr
141300041213      ************************************************************
141400041213      * CARICAMENTO PAGINA LISTA
141500041213      ************************************************************
141600041213     C     ROLS1         BEGSR
141700041213      *
141800041213      *SflNxtChg inizializzato
141900041213      *(ripristino l'attributo di 'riga cambiata')
142000041213     C                   SETOFF                                       32
142100041213     C                   Z-ADD     0             Y
142200041213      *mi posiziono sull'ultima riga caricata
142300041217     C                   Z-ADD     WMaxS1        S1Nrr
142400041213      *
142500090331      * Carico il sfl
142600090331     C     $EndFileS1    DOWEQ     *OFF
142700041217     C     Y             ANDLT     WPagS1
142800041213      *
142900041217     C                   exsr      RieS1
143000041213      *
143100041213     C                   ADD       1             S1NRR
143200041213     C                   ADD       1             Y
143300041213      *
143400041213     C                   WRITE     FMTS1
143500041213      *
143600041217     C                   EXSR      RedAnaS1
143700041213      *
143800041213     C                   ENDDO
143900041213      *
144000041213      *memorizzo quante righe ho caricato
144100041217     C                   Z-ADD     S1NRR         WMaxS1               30
144200041213      *
144300041213      *
144400041217      * Posizionamemto al 1° RCD della pagina
144500041217     C                   Exsr      PosPag1S1
144600041213      *
144700041217     C     XRolS1        Endsr
144800041217      ************************************************************
144900041217      * Posizionamemto al 1° RCD della pagina S1
145000041217      ************************************************************
145100041217     C     PosPag1S1     Begsr
145200041217      *
145300041217     C     S1Nrr         DIV       SflPagS1      PAGINE            4 0
145400041217     C                   MVR                     RESTO             3 0
145500041217     C     PAGINE        MULT      SflPagS1      C1RCD
145600041217     C     RESTO         IFGT      0
145700041217     C                   ADD       1             C1RCD
145800041217     C                   ELSE
145900041217     C                   SUB       SflPagS1      C1RCD
146000041217     C                   ADD       1             C1RCD
146100041217     C                   ENDIF
146200041217      *
146300041217     C     XPosPag1S1    Endsr
146400041217      ************************************************************
146500041217      * Chiamata programma con tasto personalizzato
146600041217      ************************************************************
146700041217     C     FPerS1        BEGSR
146800041217      *
146900041217      * salvo la DS di ingresso
147000090331     C                   MOVEL     NDCL77DS      $SaveDS
147100041217      *
147200090331     C                   MOVEL     NDCL77DS      KPJBU
147300041217     C                   CALL      $PGF2(PosFun)                        22
147400041217     C                   PARM                    KPJBA
147500041217     C                   PARM                    NOMPGM
147600041217      *
147700041217      * ripristino la DS di ingresso
147800090331     C                   MOVEL     $SaveDS       NDCL77DS
147900041217      *
148000041217     C     XFPerS1       ENDSR
148100041213      ************************************************************
148200041213      * POSIZIONAMENTO RCD ARCHIVIO PILOTA
148300041213      ************************************************************
148400041217     C     SetAnaS1      BEGSR
148500041213      *
148600090331     c                   exsr      InzSqlS1
148700041213      *
148800041217     C     XSetAnaS1     ENDSR
148900090331     C************************************************************
149000090331     C* Composizione stringa sql
149100090331     C************************************************************
149200090331     C     InzSqlS1      BEGSR
149300100408     c*
149400100408     c                   clear                   SQ
149500090331     c*
149600100415     C                   EVAL      SQ='Select distinct RcaSocieta, RcaPosiz, +
149700100413     C                                 Rcakcc, rcaksc,                       +
149800100413     c                                 RcaDtPar, RcaNrPar, RcaSerPar         +
149900100413     c                                 ,RcaDtSca                             +
150000100413     c                                 from NdRca00F'
150100090331     c*
150200090331     C                   Cat       'Where'     :1Sq
150300090331     c* Societa
150400100408     C                   Cat       'Rcasocieta':1Sq
150500090331     C                   Cat       '=':0         Sq
150600090331     C                   Cat       '''':0        Sq
150700090331     C                   Cat       xscsoc      :0Sq
150800090331     C                   Cat       '''':0        Sq
150900100408     c* posizione attributo = a quella del parametro mod. solleciti
151000100408     C                   Cat       'and'       :1Sq
151100100408     C                   Cat       'RcaPosiz'  :1Sq
151200100408     C                   Cat       '='         :1Sq
151300100408     C                   Cat       ''''        :0Sq
151400100408     C                   Cat       PosAttr057  :0Sq
151500100408     C                   Cat       ''''        :0Sq
151600100408     c* parzializzazione kcc
151700100408     C                   Cat       'and'       :1Sq
151800100408     C                   Cat       'RcaKcc'    :1Sq
151900100408     C                   Cat       '='         :1Sq
152000100408     C                   Cat       '''':0        Sq
152100100408     C                   Cat       kccl77      :0Sq
152200100408     C                   Cat       '''':0        Sq
152300090409     c* parzializzazione ksc
152400090409     C                   Cat       'and'       :1Sq
152500100408     C                   Cat       'Rcaksc'    :1Sq
152600090409     C                   Cat       '='         :1Sq
152700090409     C                   Cat       '''':0        Sq
152800090409     C                   Cat       kscl77      :0Sq
152900090409     C                   Cat       '''':0        Sq
153000090331     c* parzializzazione unita
153001151022     c* felletti ottobre 2015
153002151022     c* non serve in quanto c'è già il filtro sul sottoconto
153100151022     c**                 if        UnitaL77<>*blanks
153200151022     C**                 Cat       'and'       :1Sq
153300151022     C**                 Cat       'RcaUnita'  :1Sq
153400151022     C**                 Cat       '='         :1Sq
153500151022     C**                 Cat       '''':0        Sq
153600151022     C**                 Cat       UnitaL77    :0Sq
153700151022     C**                 Cat       '''':0        Sq
153800151022     c**                 endif
153900100408     c* parzializzazione partita
154000100408     c* data
154100100408     c                   move      DtParL77      DtPartL77A       10
154200100408     C                   Cat       'and'       :1Sq
154300100408     C                   Cat       'RcaDtPar'  :1Sq
154400100408     C                   Cat       '='         :1Sq
154500100408     C                   Cat       '''':0        Sq
154600100408     C                   Cat       DtPartL77A  :0Sq
154700100408     C                   Cat       '''':0        Sq
154800100408     c* numero
154900100408     c                   move      NrParL77      NrPartL77A        9
155000100408     C                   Cat       'and'       :1Sq
155100100408     C                   Cat       'RcaNrPar'  :1Sq
155200100408     C                   Cat       '='         :1Sq
155300100408     C                   Cat       NrParTL77A  :0Sq
155400100408     c* serie
155500100408     C                   Cat       'and'       :1Sq
155600100408     C                   Cat       'RcaSerPar' :1Sq
155700100408     C                   Cat       '='         :1Sq
155800100408     C                   Cat       '''':0        Sq
155900100408     C                   Cat       SerParL77   :0Sq
156000100408     C                   Cat       '''':0        Sq
156100100421     c* parzializzazione partite
156200140331brt  c****               if        statoL77 <> *blanks
156300140314     C                   Cat       'and'       :1Sq
156400140314     C                   Cat       'RcaStato'  :1Sq
156500140331      * aperte/chiuse
156600140331     c                   if        statoL77 = '2'
156700140331     C                   Cat       '='         :1Sq
156800100421     C                   Cat       ''''        :0Sq
156900100421     C                   Cat       '2'         :0Sq
157000100421     C                   Cat       ''''        :0Sq
157100100421     c                   else
157200140331     C                   Cat       '<>'        :1Sq
157300100421     C                   Cat       ''''        :0Sq
157400100421     C                   Cat       '2'         :0Sq
157500100421     C                   Cat       ''''        :0Sq
157600100421     c                   endif
157700140331brt  c***                endif
157800090331     C* ORDINAMENTO
157900090331     c                   cat       'Order by'  :1Sq
158000100415     c                   cat       'RcaDtSca'  :1Sq
158100090331     C*
158200090331     C* Esecuzione SQL
158300090331     C                   Exsr      ExeSQLS1
158400090331     C*
158500090331     C                   ENDSR
158600090331     C******************************************************
158700090331     C* ESECUZIONE SQL
158800090331     C******************************************************
158900090331     C     EXESQLS1      BEGSR
159000090331     C*
159100090331     C                   EXSR      CLOCURS1
159200090331     C*
159300090331     C/EXEC SQL
159400090331     C+     PREPARE S1 FROM :SQ
159500090331     C/END-EXEC
159600090331     C*
159700090331     C/EXEC SQL
159800090331     C+     DECLARE A1 CURSOR FOR S1
159900090331     C/END-EXEC
160000090331     C*
160100090331     C/EXEC SQL
160200090331     C+     OPEN A1
160300090331     C/END-EXEC
160400090331     c*
160500090331     C                   ENDSR
160600090331     C*
160700090331     C************************************************************
160800090331     C* CHIUSURA CURSORE
160900090331     C************************************************************
161000090331     C     CLOCURS1      BEGSR
161100090331     C*
161200090331     C/EXEC SQL
161300090331     C+     CLOSE A1
161400090331     C/END-EXEC
161500090331     C*
161600090331     C                   ENDSR
161700090331     C/EJECT
161800090331     C************************************************************
161900090331     C* fetch
162000090331     C************************************************************
162100090331     C     FetchSqlS1    BEGSR
162200090331     C*
162300090331     C/EXEC SQL
162400090331     C+     FETCH A1 INTO :dsFetchS1
162500090331     C/END-EXEC
162600090331     c*
162700090331     C                   ENDSR
162800090331     C*
162900041213      ************************************************************
163000041213      * LETTURA RCD ARCHIVIO PILOTA
163100041213      ************************************************************
163200041217     C     RedAnaS1      BEGSR
163300041213      *
163400041213      *
163500090331     C                   movel     *off          $EndFileS1
163600041213     C                   movel     *off          $RcdOk
163700041213      *
163800090331     C                   dou       $EndFileS1= *on or $RcdOk = *on
163900041213      *
164000090331     c                   exsr      FetchSqlS1
164100090331     C                   if        SqlCod<0 or SqlCod=100
164200090331     C                   MOVE      *ON           *IN21
164300090331     C                   ELSE
164400090331     C                   MOVE      *OFF          *IN21
164500090331     C                   END
164600090331     C                   MOVE      *IN21         $EndFileS1
164700041213      *
164800041213      * SELEZIONE
164900090331     C                   if        $EndFileS1= *off
165000041217     C                   exsr      SelAnaS1
165100041213     C                   endif
165200041213      *
165300041213     C                   enddo
165400041213      *
165500090331     C                   move      $EndFileS1    *IN33
165600041213      *
165700041217     C     XRedAnaS1     ENDSR
165800041213      ************************************************************
165900041213      * SELEZIONI SULLA RIGA LETTA
166000041213      ************************************************************
166100041217     C     SelAnaS1      Begsr
166200041213      *
166300041213     C                   movel     *on           $RcdOk
166400090409      *
166500041213      *
166600041217     C     XSelAnaS1     Endsr
166700041213      ************************************************************
166800041213      * RIEMPIMENTO RIGA LISTA
166900041213      ************************************************************
167000041213     C     RIES1         BEGSR
167100041213      *
167200041213     C                   clear                   FMTS1
167300041213      *
167400100415     c* aggancio ultima fase (aperta o chiusa)
167500100415     c                   eval      RcaSocieta=SocietaL77
167600100426     c                   eval      RcaPosiz=PosAttr057
167700100415     c                   eval      RcaKcc    =dsKcc
167800100415     c                   eval      RcaKsc    =dsKsc
167900100415     c                   eval      Rcadtpar  =DsdtPar
168000100415     c                   eval      RcaNrpar  =DSNrPar
168100100415     c                   eval      RcaSerPar =DsSerPar
168200100415     c                   eval      RcaDtSca  =DSDtSca
168300131031     c**                 eval      RcaDtAtt  = *hival
168400131031     c**   k08Rca04      setll     NdRca04L
168500131031     c**   k07Rca04      reade(n)  NdRca04L
168600131031     c                   eval      RcaDtAtt  = *hival
168700131031     c     k08Rca01      setgt     NdRca01L
168800140313     c     k08Rca01      readpe    NdRca01L
168900090331     C* riempio i campi del sfl, compresi gli hidden con i valori
169000090331     C* del rcd letto
169100100419     C                   move      Rcamdrd       H1mdrd
169200100419     C                   move      Rcamdpag      H1mdpag
169300100408     c                   if        DsDtsca    <> *loval
169400100408     c     *jobrun       move      DsDtSca       s1dtSca
169500090331     c                   endif
169600100415     c                   eval      s1Fase    = RcaFase
169700100419     c                   z-add     Rcaprogr      S1Progr
169800100419     c                   if        RcaStato='1' or RcaStato='2'
169900100419     c                   move      'C'           S1Stato
170000100419     c                   endif
170100100415     c                   eval      s1Azione  = RcaAzione
170200100412     C                   MOVE      *ALL'0'       XTBKEY
170300100412    >C                   MOVE      s1Azione      XTBKEY
170400100412     C                   MOVE      '0'           XTBALC
170500100412     C                   MOVE      '1'           XTBRIC
170600100412     C                   MOVE      '047'         XTBCOD
170700100412     C                   CALLB     'XATB'
170800100412     C                   PARM                    XATBDS
170900100412     C     XTBERR        IFEQ      '0'
171000100412     C                   MOVEL     XTBUNI        Ang047ds
171100100419     c                   movel     Sigla047      s1AzioneD
171200100412     c                   endif
171300131105     c**                 z-add     RcaImportD    s1Importd
171400131105     c**                 move      RcaSegno      s1Segno
171500100412      * primo destinatario del gruppo azione
171600100412     C                   MOVE      *ALL'0'       XTBKEY
171700100415    >C                   MOVE      RcaGrute      XTBKEY
171800100412     C                   MOVE      '0'           XTBALC
171900100412     C                   MOVE      '1'           XTBRIC
172000100412     C                   MOVE      '048'         XTBCOD
172100100412     C                   CALLB     'XATB'
172200100412     C                   PARM                    XATBDS
172300100412     C     XTBERR        IFEQ      '0'
172400100412     C                   MOVEL     XTBUNI        Ang048ds
172500100412     c                   movel     usr1048       s1destinat
172600100412     c                   endif
172700140313      * rep. caus. da ndscd
172800140313     c                   exsr      repcausale
172900100413     c*
173000100415     c                   if        RcaNota<> *blanks
173100100413     c                   move      *on           H1Note
173200100413     c                   endif
173300041213      * attributi sfl
173400041213     C                   exsr      AtrS1
173500041213      *
173600041213     C     XRieS1        Endsr
173700140313     C************************************************************
173800140313     C* Reperimento causale da ndscd
173900140313     C************************************************************
174000140313     C     RepCausale    BEGSR
174100140313     C*
174200140314     c                   clear                   s1causale
174300140314     C*
174400140313     c                   eval      ScdTpScad ='0'
174500140313     c                   eval      ScdSocieta= SocietaL77
174600140313     c                   eval      ScdCtb    ='CG'
174700140313     c                   eval      Scdkcc    = dskcc
174800140313     c                   eval      Scdksc    = dsksc
174900140313     c                   eval      ScdDtPar = dtparL77
175000140313     c                   eval      ScdNrPar = nrparL77
175100140313     c                   eval      SCdSerPar= serparL77
175200140313     c                   eval      SCdDtSca = DSdtsca
175300140314     c     K09Scd01      setll     ndscd01l
175400140314     c                   do        *hival
175500140314     c     K09Scd01      reade     ndscd01l
175600140317     c                   if        %eof(ndscd01L)
175700140314     c                   leave
175800140314     c                   endif
175900140314     c                   if        SCDIMPRMC > 0
176000140313     c                   eval      s1causale = ScdCaus2
176100140314     c                   leave
176200140314     c                   endif
176300140314     c                   eval      s1causale = ScdCaus2
176400140314     c                   enddo
176500140314      *
176600140313     c                   Endsr
176700140313      ************************************************************
176800041213      * ATTRIBUTI DELLA RIGA DEL SFL
176900041213      ************************************************************
177000041213     C     AtrS1         Begsr
177100100413      *Se ci sono i note metto segnalino
177200100413     C                   if        H1Note    =*on
177300090402     c                   seton                                        43
177400090402     c                   else
177500090402     c                   setoff                                       43
177600090402     c                   endif
177700041213      *
177800041213     C     XAtrS1        Endsr
177900041213      ************************************************************
178000041213      * CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
178100041213      ************************************************************
178200041217     C     ClcPagS1      Begsr
178300041213      *
178400041213      * Input :
178500041213      * - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
178600041213      *          cursore
178700041213      * - SFLPAG = numero rcd per pagina sfl
178800041213      * Output :
178900041213      * - WPAG = pagina fino a cui deve essere ricaricato il sfl
179000041213      *
179100041217     C     WSflS1        div       SflPagS1      Pagine            4 0
179200041213     C                   mvr                     Resto             3 0
179300041213     C                   if        Resto > 0
179400041213     C                   add       1             Pagine
179500041213     C                   endif
179600041217     C     Pagine        mult      SflPagS1      WPagS1
179700041213      *
179800041217     C     XClcPagS1     Endsr
179900041213      ************************************************************
180000041213      * GESTIONE F08 VIDEO S1
180100041213      ************************************************************
180200041213     C     F08S1         BEGSR
180300041213      *
180400041213     C                   MOVE      *ON           $FINE
180500090331     C                   MOVE      '0'           RETL77
180600041213      *
180700041213     C     XF08S1        ENDSR
180800090511      ************************************************************
180900090511      * GESTIONE F09 VIDEO S1
181000090511      ************************************************************
181100090511     C     F09S1         BEGSR
181200090511     C*
181300090511     C*controllo chiamata ricorsiva
181400090511     c                   Reset                   X48chPgmds
181500090511     C                   Eval      X48pgm = 'NDC988R'
181600090511     C                   Eval      KPJBU = X48chpgmds
181700090511     C                   CALL      'X48CHPGM'
181800090511     C                   PARM                    KPJBA
181900090511     C                   Movel     KPJBU         X48chpgmds
182000090511     C                   If        X48errore = *off
182100090511     C                   eval      Societa988=SocietaL77
182200090511     C                   eval      Unita988  =UnitaL77
182300090511     C                   eval      opz988='02'
182400090511     C                   eval      KCc988    =KccL77
182500090511     C                   eval      Ksc988    =KscL77
182600090511     C                   eval      dtrif988  =zdateiso
182700100426c2156c                   eval      dtpar988=dtparl77
182800100426  "  c                   eval      nrpar988=nrparl77
182900100426c2156c                   eval      serpar988=serparL77
183000090511     C                   Movel(p)  NDC988DS      KPJBU
183100090511     C                   CALL      'NDC988R'
183200090511     C                   PARM                    KPJBA
183300090511     C                   Movel     KPJBU         ndc988ds
183400090511     c                   eval      RetL77=Ret988
183500090511     c* se modificato, faccio riemettere videata con importo aggiornato
183600090511     c                   if        Opr988=*on
183700100426c2156C                   MOVE      OPR988        Wopr
183800100426c2156c                   move      Ret988        REtl77
183900090511     c                   eval      $inzd1=*on
184000100426c2156c                   eval      $inzs1=*on
184100100426c2156c                   eval      $inzs2=*on
184200090511     c                   endif
184300090511     C                   EndIf
184400090511     c*
184500090511     C                   endsr
184600041213      ************************************************************
184700041213      * GESTIONE F03 VIDEO S1
184800041213      ************************************************************
184900041213     C     F03S1         BEGSR
185000041213      *
185100041213     C                   MOVE      *ON           $FINE
185200090331     C                   MOVE      '1'           RETL77
185300041213      * fine programma
185400041213     C                   EXSR      ENDPGM
185500041213      *
185600041213     C     XF03S1        ENDSR
185700130916C2226 ************************************************************
185800130916  "   * GESTIONE F11 VIDEO S1
185900130916  "   ************************************************************
186000130916C2226C     F11S1         BEGSR
186100130916     C*
186200130916     C*controllo chiamata ricorsiva
186300130916     c                   Reset                   X48chPgmds
186400130916     C                   Eval      X48pgm = 'NDC980R'
186500130916     C                   Eval      KPJBU = X48chpgmds
186600130916     C                   CALL      'X48CHPGM'
186700130916     C                   PARM                    KPJBA
186800130916     C                   Movel     KPJBU         X48chpgmds
186900130916     C                   If        X48errore = *off
187000130916     C                   eval      Societa980=SocietaL77
187100130916     C                   eval      Unita980  =UnitaL77
187200130916     C                   eval      opz980='02'
187300130916     C                   eval      KCc980    =KccL77
187400130916     C                   eval      Ksc980    =KscL77
187500130916     C                   eval      dtrif980  =zdateiso
187600130916     c                   eval      dtpar980=dtparl77
187700130916     C                   eval      nrpar980=nrparl77
187800130916     C                   eval      serpar980=serparL77
187900130916     C                   Movel(p)  NDC980DS      KPJBU
188000130916     C                   CALL      'NDC980R'
188100130916     C                   PARM                    KPJBA
188200130916     C                   Movel     KPJBU         ndc980ds
188300130916     C                   eval      RetL77=Ret980
188400130916     C* se modificato, faccio riemettere videata con importo aggiornato
188500130916     C                   if        Opr980=*on
188600130916     C                   MOVE      OPR980        Wopr
188700130916     C                   move      Ret980        REtl77
188800130916     C**                 eval      $inzd1=*on
188900131001BRT  C                   eval      $inzs1=*on
189000131001BRT  C                   eval      $inzs2=*on
189100130916     C                   endif
189200130916     C                   EndIf
189300130916     C*
189400130916C2226C                   Endsr
189500041213      ************************************************************
189600041213      * GESTIONE F12 VIDEO S1
189700041213      ************************************************************
189800041213     C     F12S1         BEGSR
189900041213      *
190000041213      *  se la 1a videata sono le parzializzazioni, per F12 torno a queste
190100041213     C     $LOP          IFEQ      1
190200041213     C                   MOVE      'D1'          $GEST
190300041213      *  se la 1a videata è questa, per F12 torno a menu
190400041213     C                   ELSE
190500041213     C                   MOVE      *ON           $FINE
190600090331     C                   MOVE      '2'           RETL77
190700041213     C                   ENDIF
190800051116     C*
190900041213      *
191000041213     C     XF12S1        ENDSR
191100041213      ************************************************************
191200041213      * CONTROLLO CAMPI I/O RIGA LISTA
191300041213      ************************************************************
191400041213     C     RECS1         BEGSR
191500041213      *
191600041213      * reset indicatori DSPATR
191700050103      *   se il campo è rimasto in RI per un errore precedente
191800050103      *   la routine di controllo spegne questo RI poi si dovrà fare
191900050103      *   la write del relativo sfl
192000050427     C                   MOVE      *ALL'0'       In4245            4
192100050427     C                   MOVEA     In4245        *IN(42)
192200041213      *
192300041213      * Controllo se Utente Autorizzato alla scelta
192400041213     C     S1OPZ         IFNE      0
192500041213     C                   Z-ADD     1             X
192600041213     C     S1OPZ         LOOKUP    $OP1(X)                                21
192700041213      * mi salvo l'indice dell' opzione
192800041213     C   21              Z-add     X             PosOpz            2 0
192900041213     C     *IN21         IFEQ      *OFF
193000041213     C     $FL1(X)       OREQ      'N'
193100041213     C                   MOVE      *ON           $ESCI
193200050427     C                   SETON                                        429199
193300041213     C                   ENDIF
193400041213     C                   ENDIF
193500041213      *
193600041213     C     XRecS1        ENDSR
193700041213      ************************************************************
193800041213      * GESTIONE OPZIONI LISTA
193900041213      ************************************************************
194000041213     C     OPZS1         BEGSR
194100041213      *
194200041213     C                   Z-ADD     RIRI          CURR
194300041213     C                   Z-ADD     COCO          CURC
194400041213      *
194500041213     C                   SELECT
194600051115     C*
194700100413     C* se opz=scelta
194800100413     C     S1OPZ         WHENEQ    01
194900100419     C     *jobrun       move      S1DtSca       Seldtsca
195000100419     c                   z-add     s1importd     Selimportd
195100100419     c                   move      s1segno       SelSegno
195200100419     c                   move      h1mdrd        SelmdRd
195300100419     c                   move      h1mdpag       SelmdPag
195400100413     C                   MOVE      *ON           $ESCI
195500051115     C                   MOVE      *HIVAL        $LASTV
195600100413     c                   move      *on           $inzs2
195700100413     c                   eval      Wgest='S2'
195800130729D2799C                   eval      SavOpzS1 = S1Opz
195900051115     C*
196000041213      * opzione personalizzata
196100041213     C                   when      $PGO1(PosOpz) <> *blank
196200041213     C                   exsr      Opzs1_Per
196300041213      *
196400041213      * se altre opzioni
196500041213     C                   OTHER
196600041213      *
196700041213     C                   ENDSL
196800041213      *
196900041213     C     XOpzS1        ENDSR
197000051115     C/EJECT
197100041213      ************************************************************
197200041213      * opzione personalizzata
197300041213      ************************************************************
197400041213     C     Opzs1_Per     Begsr
197500041213      *
197600090331     C                   movel     NDCL77DS      $SaveDS
197700041213      *
197800090331     C                   RESET                   NDCL77DS
197900090331     C                   MOVE      xscsoc        SocietaL77
198000090331     C                   MOVEL     S1OPZ         OPZL77
198100090331      * passare nei campi appositi della DS NDCL77DS il valore
198200041213      * della chiave della VL univoca
198300090331     C                   movel     xscsoc        SocietaL77
198400090331     C**                 z-add     H1Sys         SysL77
198500090331     C**                 z-add     H1NrAsReg     NrAsRegL77
198600041213      *
198700090331     C                   MOVEL     NDCL77DS      KPJBU
198800041213     C                   CALL      $PGO1(PosOpz)                        22
198900041213     C                   PARM                    KPJBA
199000041213     C                   PARM                    NOMPGM
199100041213      *
199200090331     C                   MOVEL     KPJBU         NDCL77DS
199300041213      * ritorno da PGM
199400041213     C                   exsr      Opzs1_RtnPer
199500041213      * ripristino la DS di ingresso
199600090331     C                   MOVEL     $SaveDS       NDCL77DS
199700041213      *
199800041213     C     XOpzS1_Per    Endsr
199900041213      ************************************************************
200000041213      * ritorno da opzione personalizzata
200100041213      ************************************************************
200200041213     C     Opzs1_RtnPer  Begsr
200300041213      *----------------------------------------------------------------
200400041213      * modo di ritorno
200500041213     C                   select
200600041213      * 1 = F3
200700090331     C                   when      RetL77 = '1'
200800041213     C                   move      *on           $Esci
200900041213     C                   move      *on           $Fine
201000041213      * 2 = F12
201100041213      * mi fermo sulla riga del sfl in oggetto senza fare nulla,
201200041213      * se non sono stati fatti aggiornamenti
201300041213      * (devo ricaricare il sfl se sono stati fatti aggiornamenti)
201400090331     C                   when      RetL77 = '2' and OprL77 <> '1'
201500041213     C                   move      *on           $Esci
201600041213     C                   endsl
201700041213      *----------------------------------------------------------------
201800041213      * operazione eseguite dal pgm chiamato
201900041213     C                   select
202000041213      * 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
202100090331     C                   when      OprL77 = '1'
202200090331     C                   move      OprL77        WOpr
202300041213      *
202400041213      * se l'opzione con cui è stato chiamato non è la modifica
202500041213      * memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
202600090331     C                   if        OpzL77 <> '02'
202700041213     C                   move      *on           WInzS1
202800041213      * se l'opzione con cui è stato chiamato è la modifica
202900041213      * aggiorno subito la riga di sfl e non richiedo il ripristino sfl
203000041213     C                   else
203100041213     C                   exsr      AggS1
203200041213     C                   endif
203300041213     C                   endsl
203400041213      *----------------------------------------------------------------
203500041213      * funzione non eseguibile per errore :
203600041213     C                   select
203700041213      * 1 = funzione richiamata chiusa in errore
203800041213      *   PRO0007-(F1) La funzione richiamata si è chiusa in errore
203900090331     C                   when      ErrL77 = '1'
204000041213     C                   move      *on           $Esci
204100041213     C                   eval      *in93 = *on
204200041213     C                   eval      *in99 = *on
204300041213      * 2 = funzione richiamata senza la dovuta autorizzazione
204400090331     C                   when      ErrL77 = '2'
204500041213     C                   move      *on           $Esci
204600041213     C                   eval      *in94 = *on
204700041213     C                   eval      *in99 = *on
204800041213     C                   endsl
204900041213      *
205000041213     C     XOpzs1_RtnPer Endsr
205100041213      ************************************************************
205200041213      * AGGIORNAMENTO RIGA LISTA
205300041213      ************************************************************
205400041213     C     AGGS1         BEGSR
205500041213      *
205600041213      * riempio i campi del sfl che sono stati ripassati dal pgm
205700041213      * di manutenzione dopo una opzione 02
205800041213      *
205900041213     C     XAggS1        ENDSR
206000041217      ************************************************************
206100041217      * GESTIONE LISTA 2
206200041217      ************************************************************
206300041217     C     GESS2         BEGSR
206400041217      * inizializzazione videata
206500041217     C     $INZS2        IFEQ      *ON
206600041217     C                   EXSR      INZS2
206700041217     C                   MOVE      *OFF          $INZS2
206800041217     C                   ENDIF
206900041217      * inizializzazione videata
207000041217     C     $INZS1        IFEQ      *ON
207100041217     C                   EXSR      INZS1
207200041217     C                   MOVE      *OFF          $INZS1
207300041217     C                   ENDIF
207400041217      * emissione piede videata se proveniente da altra
207500041217      * salvataggio valore formato di provenienza
207600041217     C                   if        $LastV <> 'S2' and
207700041217     C                             $LastV <> 'S1'
207800041217     C                   WRITE     FMTT1
207900090331     c                   Write     FMTD1
208000041217     C                   EXSR      GESPIE
208100041217     C                   MOVE      $LASTV        $LASTG
208200041217     C                   MOVE      'S2'          $LASTV
208300041217     C                   ENDIF
208400041217      *
208500041217      * S1
208600090401     C                   write     FmtC1
208700041217     C                   if        WMaxS1 = 0
208800041217     C                   write     FmtD3
208900041217     C                   endif
209000041217      *
209100041217      * S2
209200041217     C                   if        WMaxS2 = 0
209300090401     C                   write     FmtC2
209400041217     C                   write     FmtD4
209500041217     C                   endif
209600041217      * Inizializzazione indice tasti funzione personalizzati
209700041217     C                   eval      PosFun = 25
209800041217      *
209900100416     c* apertura windows
210000100416     C                   Eval      wrkwdwl69=*on
210100100416     C                   Eval      wrkwdwl75=*on
210200100507     C                   Eval      wrkwdwmsg=*on
210300100416     C                   EXFMT     FMTC2
210400041217      *
210500041217      * azzero la posizione del cursore
210600041217     C                   Z-ADD     0             H1RIGA
210700041217     C                   Z-ADD     0             H1COLO
210800041217      *
210900041217     C     C2NRR         IFNE      0
211000041217     C                   Z-ADD     C2NRR         WSflS2
211100041222     C                   Z-ADD     C2NRR         C2RCD
211200041217     C                   ENDIF
211300041222     C**                 IF        SFLNRR > 0
211400041222     C**                 Z-ADD     SFLNRR        C2RCD
211500041222     C**                 ENDIF
211600041217      * inizializzazione indice opzioni personalizzate
211700041217     C                   Eval      PosOpz = 0
211800041217      * CONTROLLO ABILITAZIONE TASTI
211900041217      * INPUT
212000090331      * $Fc1 = Abilitazioni videata S1
212100041217      * OUTPUT
212200041217      * *IN97 + *IN99 Errori
212300090331     C                   MOVEA     $FC1          $FC
212400041217     C                   EXSR      CTRKEY
212500041217      *
212600041217     C     *IN99         IFEQ      *OFF
212700041217      *
212800041217     C                   SELECT
212900041217      * F3=Fine
213000041217     C     $TASTO        WHENEQ    F03
213100041217     C                   EXSR      F03S1
213200041217      * F8=Successivo
213300041217     C     $TASTO        WHENEQ    F08
213400041217     C                   EXSR      F08S1
213500090511      * F9=Recupero credito
213600090511     C     $TASTO        WHENEQ    F09
213700090511     C                   EXSR      F09S1
213800090402      * F10=Espandi cronologia
213900090402     C     $TASTO        WHENEQ    F10
214000090402     C                   EXSR      F10D1
214100130916C2226 * F11=Attività manuali
214200130916  "  C     $TASTO        WHENEQ    F11
214300130916C2226C                   EXSR      F11S1
214400041217      * F12=Ritorno
214500041217     C     $TASTO        WHENEQ    F12
214600041217     C                   EXSR      F12S1
214700090401      * F15=Interrogazione conto
214800090401     C     $TASTO        WHENEQ    F15
214900090401     C                   EXSR      F15D1
214901140610      * F16=Contatto
214902140610     C     $TASTO        WHENEQ    F16
214903140610     C                   EXSR      F16D1
215000041217      * F24=Altri tasti
215100041217     C     $TASTO        WHENEQ    F24
215200090331     C                   EXSR      F24D1
215300041217      * ROLL-UP
215400041217     C     $TASTO        WHENEQ    ROLLUP
215500041220     C                   EXSR      ROLS2
215600041217      * Personalizzata
215700041217     C     $PGF2(PosFun) Whenne    *BLANK
215800041217     C                   EXSR      FperS1
215900041217      *
216000041217     C                   OTHER
216100041221      *
216200041221      * CONTROLLI su entrambi i sfl
216300041222      *   il prossimo sfl da gestire sarà quello in errore
216400041222      *   oppure quello sul quale è il cursore
216500041221     C                   eval      WGest = $Gest
216600041221     C                   exsr      CtrS
216700041221     C                   eval      $Gest = WGest
216800041217      *
216900041217     C                   ENDSL
217000041217      *
217100041217     C                   ENDIF
217200041217      *
217300041217     C     XGesS2        ENDSR
217400090331      ************************************************************
217500090331      * Controlli sfl S1 e S2
217600090331      ************************************************************
217700090331     C     CtrS          Begsr
217800090331      *
217900090331      * determino Riga/Colonna del cursore
218000090331     C                   MOVE      NRG           R$$
218100090331     C                   MOVE      NCL           C$$
218200090331     C                   Z-ADD     RIRI          CURR
218300090331     C                   Z-ADD     COCO          CURC
218400090331      *
218500090331      * imposto pos. del cursore
218600090331     C                   Z-ADD     CURR          H1RIGA
218700090331     C                   Z-ADD     CURC          H1COLO
218800090331      *
218900090331     C                   eval      $SflErr = *blank
219000090401      *
219100090401     c* se la riga del cursore è inferiore a 11, gestione videata D1
219200090401     c                   IF        H1riga <11
219300090401     c                   eval      WGest='D1'
219400090401     C
219500090401     c                   ELSE
219600090331      *
219700090401      * CONTROLLO OPZIONI sfl
219800090331      *
219900090331      * Inizio a fare i controlli dal sfl sul quale è il cursore
220000090401     C                   if        H1Colo < 67
220100090331      *
220200090331     C                   exsr      CtrS_S1_I
220300090331      *
220400090331     C                   else
220500090331      *
220600090331     C                   exsr      CtrS_S2_I
220700090331      *
220800090331     C                   endif
220900090331      *
221000090401      * RISOLVO OPZIONI
221100090331      *
221200090401      *  se opzioni S1 o S2 ok, 'risolvo' opzioni <> 1 sui sfl
221300090331     C                   if        *in99 = *off
221400090331      *
221500090331      *   Inizio a fare i controlli dal sfl sul quale è il cursore
221600090401     C                   if        H1Colo < 67
221700090331      *
221800090331     C                   exsr      CtrS_S1_II
221900090331      *
222000090331     C                   else
222100090331      *
222200090331     C                   exsr      CtrS_S2_II
222300090331      *
222400090331     C                   endif
222500090331     C                   endif
222600090331      *
222700090331      * -------------------------------------------------------
222800090331      * Se non si è verificato un errore riferito a solo uno dei 2 sfl
222900090331      * passo a gestire il sfl sul quale è posizionato il cursore
223000090331     C                   if        $SflErr <> *blank
223100090331     C                   eval      WGest = $SflErr
223200090331     C                   else
223300090401     C                   if        H1colo < 67
223400130729D2799C                             and SavOpzS1 <> 01
223500090331     C                   eval      WGest = 'S1'
223600090331     C                   else
223700090331     C                   eval      WGest = 'S2'
223800130729D2799C                   eval      SavOpzS1 = S1OPz
223900090331     C                   endif
224000090331     C                   endif
224100090331      *
224200090401     C                   ENDIF
224300090401      *
224400090331     C     XCtrS         Endsr
224500041221      ************************************************************
224600041221      * Controllo opzioni sfl S1
224700041221      ************************************************************
224800041221     C     CtrS_S1_I     Begsr
224900041221      *
225000041221     C                   eval      *in99 = *off
225100041221     C                   eval      $Esci = *off
225200041221     C                   eval      S1opz = 0
225300041221      *
225400041221      * Leggo il sfl solo se ci sono rcd
225500041221     C                   if        WMaxS1 > 0
225600041221     C                   readc     FmtS1                                  21
225700041221      *
225800041221      * esce se fine sfl o errore che richiede l'uscita
225900041221     C                   dow       *in21 = *off  and  $Esci = *off
226000041221      *
226100041221      *  ctrl opzioni su riga
226200041221     C                   exsr      RecS1
226300041221      *
226400041221      *  la riga è da rileggere se errore o opz valida
226500041221     C                   if        *in99 = *on  or  S1Opz <> 0
226600041221     C                   move      *on           *in32
226700041221     C                   endif
226800041221      *  se errore, passerò a gestire il sfl in errore
226900041221      *  nella pagina della riga appena letta
227000041221     C                   if        *in99 = *on
227100050103     C                   eval      $SflErr = 'S1'
227200041221     C                   z-add     S1Nrr         C1Rcd
227300041221     C                   endif
227400041221      *  attributi sfl
227500041221     C                   exsr      AtrS1
227600041221      *  aggiorna sfl
227700041221     C                   update    FmtS1
227800041221     C                   move      *off          *in32
227900041221      *
228000041221      *  leggo prossimo rcd dal sfl se no errori
228100041221     C                   if        $Esci = *off
228200041221     C                   readc     FmtS1                                  21
228300041221     C                   endif
228400041221     C                   enddo
228500041221      *
228600041221     C                   endif
228700041221      *
228800041221     C     XCtrS_S1_I    Endsr
228900041221      ************************************************************
229000041221      * Controllo opzioni sfl S2
229100041221      ************************************************************
229200041221     C     CtrS_S2_I     Begsr
229300041221      *
229400041221     C                   eval      *in99 = *off
229500041221     C                   eval      $Esci = *off
229600041221     C                   eval      S2opz = 0
229700041221      *
229800041221      * Leggo il sfl solo se ci sono rcd
229900041221     C                   if        WMaxS2 > 0
230000041221     C                   readc     FmtS2                                  21
230100041221      *
230200041221      * esce se fine sfl o errore che richiede l'uscita
230300041221     C                   dow       *in21 = *off  and  $Esci = *off
230400041221      *
230500041221      *  ctrl opzioni su riga
230600041221     C                   exsr      RecS2
230700041221      *
230800041221      *  la riga è da rileggere se errore o opz valida
230900041221     C                   if        *in99 = *on  or  S2Opz <> 0
231000041221     C                   move      *on           *in37
231100041221     C                   endif
231200041221      *  se errore, passerò a gestire il sfl in errore
231300041221      *  nella pagina della riga appena letta
231400041221     C                   if        *in99 = *on
231500050103     C                   eval      $SflErr = 'S2'
231600041221     C                   z-add     S2Nrr         C2Rcd
231700041221     C                   endif
231800041221      *
231900041221      *  attributi sfl
232000041221     C                   exsr      AtrS2
232100041221      *  aggiorna sfl
232200041221     C                   update    FmtS2
232300041221     C                   move      *off          *in37
232400041221      *
232500041221      *  leggo prossimo rcd dal sfl se no errori
232600041221     C                   if        $Esci = *off
232700041221     C                   readc     FmtS2                                  21
232800041221     C                   endif
232900041221     C                   enddo
233000041221      *
233100041221     C                   endif
233200041221      *
233300041221     C     XCtrS_S2_I    Endsr
233400041221      ************************************************************
233500090401      * 'Risolvo' opzioni sfl S1
233600041221      ************************************************************
233700041221     C     CtrS_S1_II    Begsr
233800041221      *
233900041221     C                   eval      *in99 = *off
234000041221     C                   eval      $Esci = *off
234100041221     C                   eval      S1opz = 0
234200041221      *
234300041221      * Leggo il sfl solo se ci sono rcd
234400041221     C                   if        WMaxS1 > 0
234500041221     C                   readc     FmtS1                                  21
234600041221      *
234700041221      * esce se fine sfl o errore che richiede l'uscita
234800041221     C                   dow       *in21 = *off  and  $Esci = *off
234900041221     C                   z-add     S1Nrr         C1Rcd
235000041221      *  risolvo opzioni su riga (eccetto l'1 di scelta)
235100041221     C                   exsr      OpzS1
235200100413      *  attivo sflnxtchg se rilevato errore o se devo uscire
235300090331     C                   if        *in99 = *on
235400041221     C                   move      *on           *in32
235500041221     C                   endif
235600041221      *  passerò a gestire il sfl in errore
235700041221     C                   if        *in99 = *on
235800050103     C                   eval      $SflErr = 'S1'
235900041221     C                   endif
236000041221      *  attributi sfl
236100041221     C                   exsr      AtrS1
236200041221      *  aggiorna sfl
236300041221     C                   z-add     0             S1Opz
236400041221     C                   update    FmtS1
236500041221     C                   move      *off          *in32
236600041221      *
236700041221      *  leggo prossimo rcd dal sfl se no errori
236800041221     C                   if        $Esci = *off
236900041221     C                   readc     FmtS1                                  21
237000041221      *   a fine ciclo ripristino il flag richiesta iniz.sfl
237100041221     C                   if        *in21 = *on
237200041221     C                   move      WInzS1        $InzS1
237300041221     C                   move      *off          WInzS1
237400041221      *   se è avvenuta almeno una modifica
237500041221     C                   if        $InzS1 = *on
237600041221      *   calcolo pagina a cui deve posizionarsi
237700041221     C                   if        WSflS1 > 0
237800041221     C                   exsr      ClcPagS1
237900041221     C                   else
238000041221     C                   z-add     SflPagS1      WPagS1
238100041221     C                   endif
238200041221     C                   endif
238300041221     C                   endif
238400041221     C                   endif
238500041221      *
238600041221     C                   enddo
238700041221      *
238800041221     C                   endif
238900041221      *
239000041221     C     XCtrS_S1_II   Endsr
239100041221      ************************************************************
239200090331      * 'Risolvo' opzioni sfl S2
239300041221      ************************************************************
239400041221     C     CtrS_S2_II    Begsr
239500041221      *
239600041221     C                   eval      *in99 = *off
239700041221     C                   eval      $Esci = *off
239800041221     C                   eval      S2opz = 0
239900041221      *
240000041221      * Leggo il sfl solo se ci sono rcd
240100041221     C                   if        WMaxS2 > 0
240200041221     C                   readc     FmtS2                                  21
240300041221      *
240400041221      * esce se fine sfl o errore che richiede l'uscita
240500041221     C                   dow       *in21 = *off  and  $Esci = *off
240600041221     C                   z-add     S2Nrr         C2Rcd
240700041221      *  risolvo opzioni su riga (eccetto l'1 di scelta)
240800041221     C                   exsr      OpzS2
240900041221      *  attivo sflnxtchg se rilevato errore o se devo ancora
241000041221      *  analizzare l'opz. 1
241100090331     C                   if        *in99 = *on
241200041221     C                   move      *on           *in37
241300041221     C                   endif
241400041221      *  passerò a gestire il sfl in errore
241500041221     C                   if        *in99 = *on
241600050103     C                   eval      $SflErr = 'S2'
241700041221     C                   endif
241800041221      *  attributi sfl
241900041221     C                   exsr      AtrS2
242000041221      *  aggiorna sfl
242100041221     C                   z-add     0             S2Opz
242200041221     C                   update    FmtS2
242300041221     C                   move      *off          *in37
242400041221      *
242500041221      *  leggo prossimo rcd dal sfl se no errori
242600041221     C                   if        $Esci = *off
242700041221     C                   readc     FmtS2                                  21
242800041221      *   a fine ciclo ripristino il flag richiesta iniz.sfl
242900041221     C                   if        *in21 = *on
243000041221     C                   move      WInzS2        $InzS2
243100041221     C                   move      *off          WInzS2
243200041221      *   se è avvenuta almeno una modifica
243300041221     C                   if        $InzS2 = *on
243400041221      *   calcolo pagina a cui deve posizionarsi
243500041221     C                   if        WSflS2 > 0
243600041221     C                   exsr      ClcPagS2
243700041221     C                   else
243800041221     C                   z-add     SflPagS2      WPagS2
243900041221     C                   endif
244000041221     C                   endif
244100041221     C                   endif
244200041221     C                   endif
244300041221      *
244400041221     C                   enddo
244500041221      *
244600041221     C                   endif
244700041221      *
244800041221     C     XCtrS_S2_II   Endsr
244900041217      ************************************************************
245000041217      * INIZIALIZZAZIONE LISTA
245100041217      ************************************************************
245200041217     C     INZS2         BEGSR
245300041217      * pulizia SFL
245400041217     C                   SETOFF                                         3536
245500041217     C                   WRITE     FMTC2
245600041217     C                   SETON                                          36
245700100413     c* data scadenza selezionata
245800100419     c                   if        Seldtsca <> *loval
245900100419     c     *jobrun       move      Seldtsca      c2dtsca
246000100413     c                   endif
246100100419     c                   z-add     SelImportd    c2importd
246200100419     c                   move      SelSegno      C2Segno
246300100419     c* mod. pagamento
246400100419     c                   if        selmdpag <> *blanks
246500100419     c                   move      selmdpag      c2mdpag
246600100419     C                   EVAL      XtbRic = '1'
246700100419     C                   EVAL      XtbAzi = SocietaL77
246800100419     C                   EVAL      XtbKey = *zero
246900100419     C                   EVAL      XtbCod = 'G57'
247000100419     C                   MOVE      SelMdPag      XtbKey
247100100419     C                   CALLB     'XATB'
247200100419     C                   PARM                    XAtbDS
247300100419     c                   if        xtbErr = *off
247400100419     C                   EVAL      ANGg57DS = XtbUni
247500100419     c                   eval      c2mdpagd=DesG57
247600100419     c                   endif
247700100419     c*
247800100419     c                   else
247900100419     c* rimessa diretta
248000100419     c                   move      selmdrd       c2mdpag
248100100419     c                   reset                   XatBds
248200100419     C                   EVAL      XtbRic = '1'
248300100419     C                   EVAL      XtbAzi = SocietaL77
248400100419     C                   EVAL      XtbKey = *zero
248500100419     C                   EVAL      XtbCod = 'G56'
248600100419     C                   MOVE      SelMdRd       XtbKey
248700100419     C                   CALLB     'XATB'
248800100419     C                   PARM                    XAtbDS
248900100419     c                   if        xtbErr = *off
249000100419     C                   EVAL      ANGg56DS = XtbUni
249100100419     c                   eval      c2mdpagD=DesG56
249200100419     c                   endif
249300100419     c                   endif
249400041217      *
249500041217      * opzioni
249600090331     C                   exsr      InzS2_RieOpz
249700041217      *
249800041217      * CARICAMENTO SFL A PAGINE
249900041217     C                   Z-ADD     0             S2NRR
250000041217     C                   Z-ADD     0             WMaxS2
250100041217      *
250200041217     C                   EXSR      SetAnaS2
250300041217      *
250400041217      * Lettura file pilota
250500041217     C                   EXSR      RedAnaS2
250600041217      *
250700041217      * se arrivo dalla parzializzazione devo caricare solo la prima pagina
250800041217     C                   IF        $LastV = 'D1'
250900041217     C                   Z-ADD     SflPagS2      WPagS2
251000041217     C                   ENDIF
251100041217     C                   EXSR      RolS2
251200041217      * imposto il caricamento sfl del numero massimo di righe per pagina
251300041217     C                   Z-ADD     SflPagS2      WPagS2
251400041217      *
251500041217     C     XInzS2        ENDSR
251600090331      ************************************************************
251700100419      * Caricamento opzioni S2
251800090331      ************************************************************
251900090331     C     InzS2_RieOpz  Begsr
252000090331      *
252100090331     C                   MOVEA     $TX2          $TX
252200090331     C                   MOVEA     $FL2          $FL
252300090331     C                   Z-ADD     0             WCOZ
252400090331     C                   Z-ADD     0             WMXO
252500090331      *  equivale al num.di elementi della schiera $TX2
252600100419     C                   Z-add     OpzLen2       WMXO
252700090331     C                   Z-ADD     $C2Op1        $L01
252800090331     C                   Z-ADD     0             $L02
252900100419     c                   clear                   Wopz1
253000090331     C                   EXSR      RIEOPZs2
253100090331     C                   Z-ADD     23            X
253200090331     C                   MOVE      FF23          $FC1(X)
253300090331     C                   MOVEL     WOPZ1         C2OPZ1
253400090331     C                   MOVEL     $ALLOPT       H1ALLOPT
253500090331     C                   Z-ADD     WCOZ          C1COZ
253600090331      *
253700090331     C     XInzS2_RieOpz Endsr
253800041217      ************************************************************
253900041217      * CARICAMENTO PAGINA LISTA s2
254000041217      ************************************************************
254100041217     C     ROLS2         BEGSR
254200041217      *
254300041217      *SflNxtChg inizializzato
254400041217      *(ripristino l'attributo di 'riga cambiata')
254500041217     C                   SETOFF                                       37
254600041217     C                   Z-ADD     0             Y
254700041217      *mi posiziono sull'ultima riga caricata
254800041217     C                   Z-ADD     WMaxS2        S2Nrr
254900041217      *
255000041217      * Leggo dal file anagrafico per caricare la lista
255100041217      *
255200090331     C     $EndFileS2    DOWEQ     *OFF
255300041217     C     Y             ANDLT     WPagS2
255400041217      *
255500041217     C                   exsr      RieS2
255600041217      *
255700041217     C                   ADD       1             S2NRR
255800041217     C                   ADD       1             Y
255900041217      *
256000041217     C                   WRITE     FMTS2
256100041217      *
256200041217     C                   EXSR      REDANAS2
256300041217      *
256400041217     C                   ENDDO
256500041217      *
256600041217      *memorizzo quante righe ho caricato
256700041220     C                   Z-ADD     S2Nrr         WMaxS2               35
256800041217      *
256900041217      *
257000041217      * Posizionamemto al 1° RCD della pagina
257100041220     C                   Exsr      PosPag1S2
257200041217      *
257300041217     C     XRolS2        ENDSR
257400041217      ************************************************************
257500041217      * Posizionamemto al 1° RCD della pagina S2
257600041217      ************************************************************
257700041217     C     PosPag1S2     Begsr
257800041217      *
257900041217     C     S2NRR         DIV       SflPagS2      PAGINE            4 0
258000041217     C                   MVR                     RESTO             3 0
258100041217     C     PAGINE        MULT      SflPagS2      C2RCD
258200041217     C     RESTO         IFGT      0
258300041217     C                   ADD       1             C2RCD
258400041217     C                   ELSE
258500041217     C                   SUB       SflPagS2      C2RCD
258600041217     C                   ADD       1             C2RCD
258700041217     C                   ENDIF
258800041217     C     XPosPag1S2    Endsr
258900041217      ************************************************************
259000041217      * POSIZIONAMENTO RCD ARCHIVIO PILOTA x caricare S2
259100041217      ************************************************************
259200041217     C     SETANAS2      BEGSR
259300041217      *
259400090331     c                   exsr      InzSqlS2
259500041217      *
259600041217     C     XSetAnaS2     ENDSR
259700090331     C************************************************************
259800090331     C* Composizione stringa sql SFL2
259900090331     C************************************************************
260000090331     C     InzSqlS2      BEGSR
260100100408     c*
260200100408     c                   clear                   SQ2
260300090331     c*
260400100413     C                   EVAL      SQ2='Select RcaSocieta, RcaPosiz, RcaGrute,+
260500100413     C                                 Rcakcc, rcaksc, RcaFase, RcaAzione,   +
260600100413     c                                 RcaDtPar, RcaNrPar, RcaSerPar, Rcadtsca,+
260700100413     c                                 RcadtChiu,  RcaUteChiu,               +
260800100419     c                                 RcaStato, RcaNota , RcadTatt,         +
260900100419     c                                 RcaDtLet, RcaNrLet, RcaProgr          +
261000100413     c                                 from NdRca00F'
261100090331     c*
261200100419     C                   Cat       'Where'     :1Sq2
261300100408     c* Societa
261400100413     C                   Cat       'Rcasocieta':1Sq2
261500100413     C                   Cat       '=':0         Sq2
261600100413     C                   Cat       '''':0        Sq2
261700100413     C                   Cat       xscsoc      :0Sq2
261800100413     C                   Cat       '''':0        Sq2
261900100413     c* posizione attributo = a quella del parametro mod. solleciti
262000100413     C                   Cat       'and'       :1Sq2
262100100413     C                   Cat       'RcaPosiz'  :1Sq2
262200100413     C                   Cat       '='         :1Sq2
262300100413     C                   Cat       ''''        :0Sq2
262400100413     C                   Cat       PosAttr057  :0Sq2
262500100413     C                   Cat       ''''        :0Sq2
262600100408     c* parzializzazione kcc
262700100413     C                   Cat       'and'       :1Sq2
262800100413     C                   Cat       'RcaKcc'    :1Sq2
262900100413     C                   Cat       '='         :1Sq2
263000100413     C                   Cat       '''':0        Sq2
263100100413     C                   Cat       kccl77      :0Sq2
263200100413     C                   Cat       '''':0        Sq2
263300100408     c* parzializzazione ksc
263400100413     C                   Cat       'and'       :1Sq2
263500100413     C                   Cat       'Rcaksc'    :1Sq2
263600100413     C                   Cat       '='         :1Sq2
263700100413     C                   Cat       '''':0        Sq2
263800100413     C                   Cat       kscl77      :0Sq2
263900100413     C                   Cat       '''':0        Sq2
264000100408     c* parzializzazione unita
264001151022     c* felletti ottobre 2015
264002151022     c* non serve in quanto c'è già il filtro sul sottoconto
264003151022     c***                if        UnitaL77<>*blanks
264200151022     C**                 Cat       'and'       :1Sq2
264300151022     C**                 Cat       'RcaUnita'  :1Sq2
264400151022     C**                 Cat       '='         :1Sq2
264500151022     C**                 Cat       '''':0        Sq2
264600151022     C**                 Cat       UnitaL77    :0Sq2
264700151022     C**                 Cat       '''':0        Sq2
264800151022     c**                 endif
264900100408     c* parzializzazione partita
265000100408     c* data
265100100408     c                   move      DtParL77      DtPartL77A       10
265200100413     C                   Cat       'and'       :1Sq2
265300100413     C                   Cat       'RcaDtPar'  :1Sq2
265400100413     C                   Cat       '='         :1Sq2
265500100413     C                   Cat       '''':0        Sq2
265600100413     C                   Cat       DtPartL77A  :0Sq2
265700100413     C                   Cat       '''':0        Sq2
265800100408     c* numero
265900100408     c                   move      NrParL77      nrPartL77A        9
266000100413     C                   Cat       'and'       :1Sq2
266100100413     C                   Cat       'RcaNrPar'  :1Sq2
266200100413     C                   Cat       '='         :1Sq2
266300100413     C                   Cat       NrPartL77A  :0Sq2
266400100408     c* serie
266500100413     C                   Cat       'and'       :1Sq2
266600100413     C                   Cat       'RcaSerPar' :1Sq2
266700100413     C                   Cat       '='         :1Sq2
266800100413     C                   Cat       '''':0        Sq2
266900100413     C                   Cat       SerParL77   :0Sq2
267000100413     C                   Cat       '''':0        Sq2
267100100413     c* data scadenza selezionata
267200100419     c                   move      SelDtsca      SeldtscaA        10
267300100413     C                   Cat       'and'       :1Sq2
267400100413     C                   Cat       'RcaDtSca'  :1Sq2
267500100413     C                   Cat       '='         :1Sq2
267600100413     C                   Cat       '''':0        Sq2
267700100419     C                   Cat       SelDtScaA   :0Sq2
267800100413     C                   Cat       '''':0        Sq2
267900100408     C* ORDINAMENTO
268000100413     c                   cat       'Order by'  :1Sq2
268100100413     c                   cat       'RcaDtSca,' :1Sq2
268200100419     c                   cat       'RcaFase,'  :1Sq2
268300100419     c                   cat       'RcaProgr'  :1Sq2
268400090331     C*
268500090331     C* Esecuzione SQL
268600090331     C                   Exsr      ExeSQLS2
268700090331     C*
268800090331     C                   ENDSR
268900090331     C******************************************************
269000090331     C* ESECUZIONE SQL
269100090331     C******************************************************
269200090331     C     EXESQLS2      BEGSR
269300090331     C*
269400090331     C                   EXSR      CLOCUR2
269500090331     C*
269600090331     C/EXEC SQL
269700090331     C+     PREPARE S2 FROM :SQ2
269800090331     C/END-EXEC
269900090331     C*
270000090331     C/EXEC SQL
270100090331     C+     DECLARE A2 CURSOR FOR S2
270200090331     C/END-EXEC
270300090331     C*
270400090331     C/EXEC SQL
270500090331     C+     OPEN A2
270600090331     C/END-EXEC
270700090331     c*
270800090331     C                   ENDSR
270900090331     C*
271000090331     C************************************************************
271100090331     C* CHIUSURA CURSORE
271200090331     C************************************************************
271300090331     C     CLOCUR2       BEGSR
271400090331     C*
271500090331     C/EXEC SQL
271600090331     C+     CLOSE A2
271700090331     C/END-EXEC
271800090331     C*
271900090331     C                   ENDSR
272000090331     C/EJECT
272100090331     C************************************************************
272200090331     C* fetch
272300090331     C************************************************************
272400090331     C     FetchSqlS2    BEGSR
272500090331     C*
272600090331     C/EXEC SQL
272700090331     C+     FETCH A2 INTO :dsFetchS2
272800090331     C/END-EXEC
272900090331     c*
273000090331     C                   ENDSR
273100090331     C*
273200041217      ************************************************************
273300041217      * LETTURA RCD ARCHIVIO PILOTA x caricare S2
273400041217      ************************************************************
273500041217     C     RedAnaS2      BEGSR
273600041217      *
273700041217      *
273800090331     C                   movel     *off          $EndFileS2
273900041217     C                   movel     *off          $RcdOk
274000041217      *
274100090331     C                   dou       $EndFileS2= *on or $RcdOk = *on
274200090331     c*
274300090331     c                   exsr      FetchSqlS2
274400090331     C                   if        SqlCod<0 or SqlCod=100
274500090331     C                   MOVE      *ON           *IN21
274600090331     C                   ELSE
274700090331     C                   MOVE      *OFF          *IN21
274800090331     C                   END
274900090331     C                   MOVE      *IN21         $EndFileS2
275000041217      *
275100041217      *
275200041217      * SELEZIONE
275300090331     C                   if        $EndFileS2= *off
275400041217     C                   exsr      SelAnaS2
275500041217     C                   endif
275600041217      *
275700041217     C                   enddo
275800041217      *
275900090331     C                   move      $EndFileS2    *IN38
276000041217      *
276100041217     C     XRedAnaS2     ENDSR
276200041217      ************************************************************
276300041217      * SELEZIONI SULLA RIGA LETTA s2
276400041217      ************************************************************
276500041217     C     SelAnaS2      Begsr
276600041217      *
276700041217     C                   movel     *on           $RcdOk
276800041217      *
276900041217     C     XSelAnaS2     Endsr
277000041217      ************************************************************
277100041217      * RIEMPIMENTO RIGA LISTA S2
277200041217      ************************************************************
277300041217     C     RieS2         BEGSR
277400041217      *
277500041217     C                   clear                   FMTS2
277600090331     C* riempio i campi del sfl, compresi gli hidden con i valori
277700090331     C* del rcd letto
277800100416     c                   eval      h2dtatt   =DsDtAtt2
277900100413     c                   eval      s2Fase    = DsFase2
278000100419     c                   z-add     Dsprogr2      s2progr
278100100413     c                   eval      s2Azione  = DsAzione2
278200100413     C                   MOVE      *ALL'0'       XTBKEY
278300100413    >C                   MOVE      s2Azione      XTBKEY
278400100413     C                   MOVE      '0'           XTBALC
278500100413     C                   MOVE      '1'           XTBRIC
278600100413     C                   MOVE      '047'         XTBCOD
278700100413     C                   CALLB     'XATB'
278800100413     C                   PARM                    XATBDS
278900100413     C     XTBERR        IFEQ      '0'
279000100413     C                   MOVEL     XTBUNI        Ang047ds
279100100419     C                   movel     des047        H2AzioneD
279200100419     c                   movel     Sigla047      s2AzioneD
279300100419     c                   move      TipAtt047     H2TpAtt
279400100413     c                   endif
279500100419      * utente chiusura
279600100419     c                   move      DsUtechiu2    s2utente
279700100419     c* stato
279800100419     c                   if        DsStato2='1' or DsStato2='2'
279900100419     c                   move      'C'           S2Stato
280000100419     c                   endif
280100100419     c                   move      S2Stato       UltStato          1
280200100413     c* data chiusura
280300100413     c                   if        dsDtChiu2 <> *loval
280400100415     c     *jobrun       move      DsDtChiu2     S2DtChiu
280500100413     c                   endif
280600100419     c* data lettera
280700100419     c                   if        dsDtLet2  <> *loval
280800100419     c     *jobrun       move      DsDtLet2      S2DtLet
280900100419     c                   endif
281000100419     c                   z-add     dsNrLet2      s2NrLet
281100100419     c*
281200100413     c                   if        DsNote2<> *blanks
281300100413     c                   move      *on           H2Note
281400100413     c                   endif
281500041217      *
281600041217      * attributi sfl
281700041217     C                   exsr      AtrS2
281800041217      *
281900041217     C     XRieS2        Endsr
282000041217      ************************************************************
282100041217      * ATTRIBUTI DELLA RIGA DEL SFL S2
282200041217      ************************************************************
282300041217     C     AtrS2         Begsr
282400100413      *Se ci sono i note metto segnalino
282500100413     C                   if        H2Note    =*on
282600100413     c                   seton                                        49
282700100413     c                   else
282800100413     c                   setoff                                       49
282900100413     c                   endif
283000041217      *
283100041217     C     XAtrS2        Endsr
283200041217      ************************************************************
283300041217      * CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL s2
283400041217      ************************************************************
283500041217     C     ClcPagS2      Begsr
283600041217      *
283700041217      * Input :
283800041217      * - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
283900041217      *          cursore
284000041217      * - SFLPAG = numero rcd per pagina sfl
284100041217      * Output :
284200041217      * - WPAG = pagina fino a cui deve essere ricaricato il sfl
284300041217      *
284400041217     C     WSflS2        div       SflPagS2      Pagine            4 0
284500041217     C                   mvr                     Resto             3 0
284600041217     C                   if        Resto > 0
284700041217     C                   add       1             Pagine
284800041217     C                   endif
284900041217     C     Pagine        mult      SflPagS2      WPagS2
285000041217      *
285100041217     C     XClcPagS2     Endsr
285200041217      ************************************************************
285300041217      * CONTROLLO CAMPI I/O RIGA LISTA S2
285400041217      ************************************************************
285500041217     C     RECS2         BEGSR
285600041217      *
285700041217      * reset indicatori DSPATR
285800050103      *   se il campo è rimasto in RI per un errore precedente
285900050103      *   la routine di controllo spegne questo RI poi si dovrà fare
286000050103      *   la write del relativo sfl
286100050427     C                   MOVE      *ALL'0'       In4649            4
286200050427     C                   MOVEA     In4649        *IN(46)
286300041217      *
286400041217      * Controllo se Utente Autorizzato alla scelta
286500041217     C     S2OPZ         IFNE      0
286600041217     C                   Z-ADD     1             X
286700100415     C     S2OPZ         LOOKUP    $OP2(X)                                21
286800041217      * mi salvo l'indice dell' opzione
286900041217     C   21              Z-add     X             PosOpz            2 0
287000041217     C     *IN21         IFEQ      *OFF
287100041217     C     $FL1(X)       OREQ      'N'
287200041217     C                   MOVE      *ON           $ESCI
287300090401     C                   SETON                                        468199
287400041217     C                   ENDIF
287500041217     C                   ENDIF
287600100416      *
287700100420     c* UTENTE non autorizzato
287800100420     c* chiusura
287900100420     c                   if        s2opz  =15 and
288000100420     c                             chiatt = *off
288100100420     C                   MOVE      *ON           $ESCI
288200100420     C                   SETON                                        466299
288300100420     c                   endif
288400100420     c* cancellazione
288500100420     c                   if        s2opz  =04 and
288600100420     c                             annatt = *off
288700100420     C                   MOVE      *ON           $ESCI
288800100420     C                   SETON                                        466299
288900100420     c                   endif
289000100524     c* se opzione chiusura, controllo che sia l'ultima fase
289100100524     c* perchè nel caso di impegni ci possono essere + fasi aperte
289200100524     c* di tipo impegno
289300100524     c                   if        s2opz=15
289400100524     c                   exsr      CtrUltima
289500100524     c                   if        UltFaseAP=*off
289600100524     C                   MOVE      *ON           $ESCI
289700100524     C                   SETON                                        468099
289800100524     c                   endif
289900100524     c                   endif
290000100416     c* se fase già chiusa non si può chiudere
290100100416     c                   if        s2opz  =15 and
290200100416     c                             s2stato<> *blanks
290300100416     C                   MOVE      *ON           $ESCI
290400100416     C                   SETON                                        468599
290500100416     c                   endif
290600100524     c* si possono cancellare solo fasi non chiuse e non di tipo
290700100524     c* automatico notturno
290800100419     c                   if        s2opz  =04 and
290900100419     c                             (s2stato <>*blanks or
291000100524     c                             h2tpAtt = '1')
291100100419     C                   MOVE      *ON           $ESCI
291200100419     C                   SETON                                        468699
291300100419     c                   endif
291400041217      *
291500041217     C     XRecS2        ENDSR
291600100524      ************************************************************
291700100524      * Controllo se la fase che si vuole chiudere è l'ultima
291800100524      * aperta, altrimenti NON si può fare
291900100524      * mi posiziono su quella e poi vado all'indietro a leggere
292000100524      ************************************************************
292100100524     C     CtrUltima     BEGSR
292200100524      *
292300100524     c                   move      *on           UltFaseAp
292400100524      *
292500100524     c                   eval      RcaSocieta=SocietaL77
292600100524     c                   eval      RcaPosiz=PosAttr057
292700100524     c                   eval      RcaKcc    =Kccl77
292800100524     c                   eval      RcaKsc    =KscL77
292900100524     c                   eval      Rcadtpar  =dtParL77
293000100524     c                   eval      RcaNrpar  =NrParL77
293100100524     c                   eval      RcaSerPar =SerParL77
293200100524     c     *jobrun       move      c2dtsca       Rcadtsca
293300100524     c                   eval      RcaFase   =s2Fase
293400100524     c                   eval      RcaProgr  =s2Progr
293500100524     c     k10Rca01      setll     NdRca01L
293600100524     c                   do        *hival
293700100524     c     k09Rca01      readpe    NdRca01L
293800100524     c                   if        %eof(NDRCA01L)
293900100524     c                   leave
294000100524     c                   endif
294100100524      *
294200100524     c                   if        RcaStato =' '
294300100524     c                   move      *off          UltFaseAp
294400100524     c                   endif
294500100524      *
294600100524     c                   enddo
294700100524     c                   endsr
294800041217      ************************************************************
294900041217      * GESTIONE OPZIONI LISTA  S2
295000041217      ************************************************************
295100041217     C     OPZS2         BEGSR
295200041217      *
295300041217     C                   Z-ADD     RIRI          CURR
295400041217     C                   Z-ADD     COCO          CURC
295500041217      *
295600041217     C                   SELECT
295700100415     C* se opz=note
295800100415     C     S2OPZ         WHENEQ    14
295900100415     c                   reset                   NDCL75DS
296000100416     c                   move      '02'          opzl75
296100100416     c                   eval      SocietaL75=societaL77
296200100416     c                   eval      kccl75    =kccl77
296300100416     c                   eval      kscl75    =kscl77
296400100416     c                   eval      descrL75  =d1descr
296500100416     c                   eval      DtParL75  =DtParL77
296600100416     c                   eval      NrParL75  =NrParL77
296700100416     c                   eval      SerParL75 =SerParL77
296800100419     c                   eval      DtScadL75 =SelDtSca
296900100416     c                   eval      DtAttL75  =h2dtatt
297000100416     c                   move      s2fase        Fasel75
297100100419     c                   z-add     s2progr       ProgrL75
297200100419     c                   eval      azioneDl75=h2azioned
297300100416     c                   eval      Azionel75=S2Azione
297400100416     c                   eval      wdwl75=wrkwdwL75
297500100415     c                   eval      kpjbu=ndcl75ds
297600100415     C                   call      'NDCL75R'
297700100415     c                   parm                    kpjba
297800100416     c                   eval      ndcl75ds=kpjbu
297900100416     c                   move      *off          wrkwdwl75         1
298000100416     C* 2 = F12
298100100416    >C                   if        RetL75='2' and
298200100416     c                             OprL75 <> *on
298300100416     C                   move      *ON           $ESCI                          sospese READC
298400100416     c                   endif
298500100416     c* aggiornamento effettuato
298600100416     c                   if        OprL75=*on
298700100416     C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
298800100416     C                   move      *ON           WINZS2                         iniz.sfl fine ciclo
298900100512     c                   move      *on           Wopr
299000100416     c                   endif
299100100419     C* se opz=Cancellazione
299200100419     C     S2OPZ         WHENEQ    04
299300100419     c                   exsr      DelAna
299400100416     C* se opz=Chiusura fase
299500100416     C     S2OPZ         WHENEQ    15
299600100416     c                   exsr      AggAna
299700041217      *
299800041217      * opzione personalizzata
299900041217     C                   when      $PGO1(PosOpz) <> *blank
300000041217     C                   exsr      Opzs2_Per
300100041217      *
300200041217      * se altre opzioni
300300100408     C                   OTHER
300400041217      *
300500041217     C                   ENDSL
300600041217      *
300700041217     C     XOpzS2        ENDSR
300800090402     C/EJECT
300900100419      ************************************************************
301000100419      * cancellazione fase
301100100419      ************************************************************
301200100419     C     Delana        Begsr
301300100419     C                   eval      Rcasocieta = societaL77
301400100419     C                   eval      Rcakcc     = kccL77
301500100419     C                   eval      rcaksc     = kscl77
301600100419     C                   eval      rcadtpar   = dtparl77
301700100419     C                   eval      rcanrpar   = nrparl77
301800100419     C                   eval      rcaserpar  = serparl77
301900100419     C                   eval      rcadtsca   = Seldtsca
302000100419     C                   eval      rcadtatt   = h2dtatt
302100100419     c                   move      s2fase        RcaFase
302200100419     c                   z-add     s2progr       RcaProgr
302300100419     c*
3024001004192    C     *IN22         DOUEQ     *OFF
302500100419     C     MSGRTN        ORNE      'R'
302600100524    >C     K11Rca04      CHAIN     NDRCA04L                           2122
302700100419     C*
302800100419     C* Gestione RCD allocato
3029001004193    C     *IN22         IFEQ      *ON
303000100419    >C                   MOVEL     'NDRCA00F'    MSGFIL
303100100419     C                   EXSR      MSGPGM
3032001004193-   C                   ENDIF
3033001004192-   C                   ENDDO
303400100419     c*
303500100419     C                   if        *in21=*off
303600100419     c                   delete    NdRCA000
303700130916C2226C*
303800130916  "  C* richiama pgm User Exit
303900130916  "  c*
304000130916  "  C     USRERR        IFEQ      *BLANK
304100130916  "  C* passa opzione ricevuta a user exit
304200130916  "  C                   MOVEL     '04'          XUKOPZ
304300130916  "  C* imposta chiave univoca  per user exit
304400130916  "  C                   MOVE      XSCSOC        XUKSOC
304500130916  "  C                   MOVE      XSCDUN        XUKDUN
304600130916  "  C                   MOVEL     DSPGM         XUKPGM
304700130916  "  C                   MOVEL (P) RRNRCA        XUKFL1
304800130916  "  C                   MOVEL     XUKDS         KPJBU
304900130916  "  C     DSPGM         CAT       'U':0         USREXT
305000130916  "  C                   CALL      USREXT                               21
305100130916  "  C                   PARM                    KPJBA
305200130916  "  C     *IN21         IFEQ      *ON
305300130916  "  C                   MOVE      *ON           USRERR
305400130916  "  C                   ENDIF
305500130916C2226C                   ENDIF
305600100524     c* se azione impegno, cancello anche impegno
305700100524     c                   if        RcaAzione='98' or
305800100524     c                             RcaAzione='99'
305900100524     C                   EVAL      IMPSocieta = rcasocieta
306000100524     C                   EVAL      IMPKcc =   rcakcc
306100100524     C                   EVAL      IMPKsc =   rcaksc
306200100524     C                   EVAL      IMPDtPar =  rcaDtpar
306300100524     C                   EVAL      IMPNrPar =  rcanrpar
306400100524     C                   EVAL      IMPSerPar = rcaserpar
306500100524     C                   EVAL      IMPDtSca =  rcadtsca
306600100524     c                   eval      ImpProgr =  rcaProgrIm
306700100524     C     K08IMP01      delete    NDIMP01L                             23
306800100524     c                   endif
306900100419     C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
307000100419     c                   move      *on           Winzs2
307100100512     c                   move      *on           Wopr
307200100419     c                   ENDIF
307300100419     c*
307400100419     c                   endsr
307500100416      ************************************************************
307600100416      * Aggiornamento x chiusura fase
307700100416      ************************************************************
307800100416     C     Aggana        Begsr
307900130913C2226C* Reperisco le note
308000130913  "  C                   eval      Rcasocieta = societaL77
308100130913  "  C                   eval      Rcakcc     = kccL77
308200130913  "  C                   eval      rcaksc     = kscl77
308300130913  "  C                   eval      rcadtpar   = dtparl77
308400130913  "  C                   eval      rcanrpar   = nrparl77
308500130913  "  C                   eval      rcaserpar  = serparl77
308600130913  "  C                   eval      rcadtsca   = Seldtsca
308700130913  "  C                   eval      rcadtatt   = h2dtatt
308800130913  "  c                   move      s2fase        RcaFase
308900130913  "  c                   z-add     s2progr       RcaProgr
309000130913  "  C     K11Rca04      CHAIN(N)  NDRCA04L                           21
309100130913C2226C                   eval      D5Nota = RcaNota
309200110407D2680C* emetto richiesta data chiusura
309300110407  "  c                   eval      D5DtChiu   =udate
309400110408  "  c                   clear                   Wmsg1
309500110408  "  c                   eval      Idmsg= Msg(5)
309600110408  "  c                   exsr      PrendiMsg
309700110408  "  c                   eval      Z5Key=txtmsg
309800110407  "  c     emetti        tag
309900110407  "  c                   exfmt     Fmtd5
310000110408  "  c                   if        $tasto=F12
310100110408  "  c                   leavesr
310200110408  "  c                   endif
310300110407  "  C                   if        D5DtChiu <>   *Zeros
310400110407  "  C     *JobRun       Test(d)                 D5DtChiu               21
310500110407  " >C     *In21         ifeq      *On
310600110407  "  c* non corretta
310700110407  " >C                   SetOn                                        5099
310800110407  "  c                   goto      emetti
310900110407  " >C                   endif
311000110407  "  c                   else
311100110407  "  c* non immessa
311200110407  " >C                   SetOn                                        5099
311300110407  "  c                   goto      emetti
311400110407D2680C                   endif
311500110407     c
311600130913C2226X**                 eval      Rcasocieta = societaL77
311700130913  "  X**                 eval      Rcakcc     = kccL77
311800130913  "  X**                 eval      rcaksc     = kscl77
311900130913  "  X**                 eval      rcadtpar   = dtparl77
312000130913  "  X**                 eval      rcanrpar   = nrparl77
312100130913  "  X**                 eval      rcaserpar  = serparl77
312200130913  "  X**                 eval      rcadtsca   = Seldtsca
312300130913  "  X**                 eval      rcadtatt   = h2dtatt
312400130913  "  X**                 move      s2fase        RcaFase
312500130913C2226X**                 z-add     s2progr       RcaProgr
312600100416     c*
3127001004162    C     *IN22         DOUEQ     *OFF
312800100416     C     MSGRTN        ORNE      'R'
312900100524    >C     K11Rca04      CHAIN     NDRCA04L                           2122
313000100416     C*
313100100416     C* Gestione RCD allocato
3132001004163    C     *IN22         IFEQ      *ON
313300100416    >C                   MOVEL     'NDRCA00F'    MSGFIL
313400100416     C                   EXSR      MSGPGM
3135001004163-   C                   ENDIF
3136001004162-   C                   ENDDO
313700100416     c*
313800100416     C                   if        *in21=*off
313900110407D2680x*************      eval      RcaDtChiu=Zdateiso
314000110407D2680c     *jobrun       move      D5DtChiu      RcaDtChiu
314100100416     c                   eval      RcaUteChiu=sdsusr
314200100416     c                   eval      RcaStato='1'
314300130913C2226C                   eval      RcaNota = D5Nota
314400100416     c                   update    NdRCA000
314500100416     C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
314600100416     c                   move      *on           Winzs2
314700100512     c                   move      *on           Wopr
314800130916C2226C*
314900130916  "  C* richiama pgm User Exit
315000130916  "  c*
315100130916  "  C     USRERR        IFEQ      *BLANK
315200130916  "  C* passa opzione ricevuta a user exit
315300130916  "  C                   MOVEL     '02'          XUKOPZ
315400130916  "  C* imposta chiave univoca  per user exit
315500130916  "  C                   MOVE      XSCSOC        XUKSOC
315600130916  "  C                   MOVE      XSCDUN        XUKDUN
315700130916  "  C                   MOVEL     DSPGM         XUKPGM
315800130916  "  C                   MOVEL (P) RRNRCA        XUKFL1
315900130916  "  C                   MOVEL     XUKDS         KPJBU
316000130916  "  C     DSPGM         CAT       'U':0         USREXT           10
316100130916  "  C                   CALL      USREXT                               21
316200130916  "  C                   PARM                    KPJBA
316300130916  "  C     *IN21         IFEQ      *ON
316400130916  "  C                   MOVE      *ON           USRERR            1
316500130916  "  C                   ENDIF
316600130916C2226C                   ENDIF
316700130916     C*
316800100507     C* avviso se esistono impegni non scaduti
316900100507     c                   exsr      MSgImpegni
317000100416     c                   ENDIF
317100140317      *
317200140317     c                   exsr      aggimpegni
317300100416     c*
317400100416     c                   endsr
317500100507      ************************************************************
317600100525      * avviso se il relativo impegno non è scaduto
317700100507      ************************************************************
317800100507     C     MsgImpegni    Begsr
317900100507     C                   EVAL      IMPSocieta = rcasocieta
318000100507     C                   EVAL      IMPKcc =   rcakcc
318100100507     C                   EVAL      IMPKsc =   rcaksc
318200100507     C                   EVAL      IMPDtPar =  rcaDtpar
318300100507     C                   EVAL      IMPNrPar =  rcanrpar
318400100507     C                   EVAL      IMPSerPar = rcaserpar
318500100507     C                   EVAL      IMPDtSca =  rcadtsca
318600100525     c                   eval      ImpProgr =  rcaProgrIm
318700100525     C     K08IMP01      chain(n)  NDIMP01L
318800100525     c                   if        %found(ndimp01l)
318900100507     c* testo data scadenza impegno
319000100507     c* aggiungo giorni di tolleranza
319100100507     c                   adddur    GGtol057:*D   impdtimp
319200100507     c* se non ancora scaduto, mando messaggio
319300100507     c                   if        impdtimp > zdateiso
319400100507     C* si deve chiamare un 'babbo' vuoto a 132 altrimenti xmsgr
319500100507     c* viene a 80 e non tiene lo sfondo
319600100507     c                   call      'ANA970R3'
319700100507     c                   exsr      callxmsgr
319800100507     c                   endif
319900100507     c*
320000100525     c                   endif
320100100507     c                   endsr
320200140317      ************************************************************
320300140317      * aggiorno impegno se chiusura
320400140317      ************************************************************
320500140317     C     AggImpegni    Begsr
320600140317     C                   EVAL      IMPSocieta = rcasocieta
320700140317     C                   EVAL      IMPKcc =   rcakcc
320800140317     C                   EVAL      IMPKsc =   rcaksc
320900140317     C                   EVAL      IMPDtPar =  rcaDtpar
321000140317     C                   EVAL      IMPNrPar =  rcanrpar
321100140317     C                   EVAL      IMPSerPar = rcaserpar
321200140317     C                   EVAL      IMPDtSca =  rcadtsca
321300140317     c                   eval      ImpProgr =  rcaProgrIm
321400140317     C     K08IMP01      chain     NDIMP01L
321500140317     c                   if        %found(ndimp01l)
321600140317     c                   eval      impann = '1'
321700140317     c                   update    ndimp000
321800140317     c                   endif
321900140317      *
322000140317     c                   endsr
322100100507     C*----------------------------------------------------                     *
322200100507     C* messaggio di avvertimento di presenza impegno non scaduto               *
322300100507     C*----------------------------------------------------                     *
322400100507     C     CallXmsgr     BEGSR                                                  *
322500100507     C*
322600100507     c                   clear                   Xmsgds
322700100507     C                   Z-ADD     0             MSGMSG
322800100507     C                   MOVEL     'N'           MSGRSP
322900100507     C                   MOVEL     'S'           MSGEMV
323000100507     C                   Z-ADD     10            MSGRGP
323100100507     C                   MOVEL     'N'           MSGLCK
323200100507     C                   MOVEA     STATUS        STS
323300100507     C                   MOVEL     'N'           MSGCNL
323400100507     C                   MOVEL     'N'           MSGVID
323500100507     C                   MOVEL     'N'           MSGOPE
323600100507     C                   MOVEL     'N'           MSGSTP
323700100507     c                   eval      xmsgCd1 = 'COS2775'
323800100507     C*
323900100507     C                   CALL      'XMSGR'
324000100507     C                   PARM                    XMSGDS
324100100507     C                   PARM                    XMStxt          135
324200100507     C                   PARM      wrkwdwmsg     XMSGinz           1
324300100507     C                   PARM                    XMSGCd1           7
324400100507     C                   PARM                    XMSGCd2           7
324500100507     C                   PARM                    XMSGCd3           7
324600100507     C                   PARM                    XMSGSn1           7
324700100507     C                   PARM                    XMSGCdTxT        63
324800100507     C                   PARM                    MSGCdTit         14
324900100507     C                   PARM                    MSGClPrtf         1
325000100507     C                   PARM                    VarMsg1         100
325100100507     C                   PARM                    VarMsg2         100
325200100507     C                   PARM                    VarMsg3         100
325300100507     C*
325400100507     c                   move      *off          wrkwdwmsg         1
325500100507     C*
325600100507     C                   ENDSR
325700041217      ************************************************************
325800041217      * opzione personalizzata
325900041217      ************************************************************
326000041217     C     Opzs2_Per     Begsr
326100041217      *
326200090331     C                   movel     NDCL77DS      $SaveDS
326300041217      *
326400090331     C                   RESET                   NDCL77DS
326500090331     C                   MOVE      xscsoc        SocietaL77
326600090331     C                   MOVEL     S2OPZ         OPZL77
326700090331      * passare nei campi appositi della DS NDCL77DS il valore
326800041217      * della chiave della VL univoca
326900090331     C                   movel     xscsoc        SocietaL77
327000041217      *
327100090331     C                   MOVEL     NDCL77DS      KPJBU
327200041217     C                   CALL      $PGO1(PosOpz)                        22
327300041217     C                   PARM                    KPJBA
327400041217     C                   PARM                    NOMPGM
327500041217      *
327600090331     C                   MOVEL     KPJBU         NDCL77DS
327700041217      * ritorno da PGM
327800041217     C                   exsr      Opzs2_RtnPer
327900041217      * ripristino la DS di ingresso
328000090331     C                   MOVEL     $SaveDS       NDCL77DS
328100041217      *
328200041217     C     XOpzS2_Per    Endsr
328300041217      ************************************************************
328400041217      * ritorno da opzione personalizzata  S2
328500041217      ************************************************************
328600041217     C     Opzs2_RtnPer  Begsr
328700041217      *----------------------------------------------------------------
328800041217      * modo di ritorno
328900041217     C                   select
329000041217      * 1 = F3
329100090331     C                   when      RetL77 = '1'
329200041217     C                   move      *on           $Esci
329300041217     C                   move      *on           $Fine
329400041217      * 2 = F12
329500041217      * mi fermo sulla riga del sfl in oggetto senza fare nulla,
329600041217      * se non sono stati fatti aggiornamenti
329700041217      * (devo ricaricare il sfl se sono stati fatti aggiornamenti)
329800090331     C                   when      RetL77 = '2' and OprL77 <> '1'
329900041217     C                   move      *on           $Esci
330000041217     C                   endsl
330100041217      *----------------------------------------------------------------
330200041217      * operazione eseguite dal pgm chiamato
330300041217     C                   select
330400041217      * 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
330500090331     C                   when      OprL77 = '1'
330600090331     C                   move      OprL77        WOpr
330700041217      *
330800041217      * se l'opzione con cui è stato chiamato non è la modifica
330900041217      * memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
331000090331     C                   if        OpzL77 <> '02'
331100041217     C                   move      *on           WInzS2
331200041217      * se l'opzione con cui è stato chiamato è la modifica
331300041217      * aggiorno subito la riga di sfl e non richiedo il ripristino sfl
331400041217     C                   else
331500041217     C                   exsr      AggS2
331600041217     C                   endif
331700041217     C                   endsl
331800041217      *----------------------------------------------------------------
331900041217      * funzione non eseguibile per errore :
332000041217     C                   select
332100041217      * 1 = funzione richiamata chiusa in errore
332200041217      *   PRO0007-(F1) La funzione richiamata si è chiusa in errore
332300090331     C                   when      ErrL77 = '1'
332400041217     C                   move      *on           $Esci
332500041217     C                   eval      *in93 = *on
332600041217     C                   eval      *in99 = *on
332700041217      * 2 = funzione richiamata senza la dovuta autorizzazione
332800090331     C                   when      ErrL77 = '2'
332900041217     C                   move      *on           $Esci
333000041217     C                   eval      *in94 = *on
333100041217     C                   eval      *in99 = *on
333200041217     C                   endsl
333300041217      *
333400041217     C     XOpzs2_RtnPer Endsr
333500041217      ************************************************************
333600041217      * AGGIORNAMENTO RIGA LISTA
333700041217      ************************************************************
333800041217     C     AGGS2         BEGSR
333900041217      *
334000041217      * riempio i campi del sfl che sono stati ripassati dal pgm
334100041217      * di manutenzione dopo una opzione 02
334200041217      *
334300041217     C     XAggS2        ENDSR
334400041217*******S2 FINE****************************************************
334500041213      ************************************************************
334600041213      * CONTROLLO ABILITAZIONE TASTI
334700041213      * INPUT
334800041213      * $TASTO =Tasto funzione premuto
334900041213      * $FC = Tasti abilitati
335000041213      * OUTPUT
335100041213      * IN99 = ERrore
335200041213      ************************************************************
335300041213     C     CTRKEY        BEGSR
335400041213      *
335500041213     C                   SETOFF                                       99
335600041213      *
335700041213     C                   SELECT
335800041213     C     $TASTO        WHENEQ    F01
335900041213     C                   z-add     01            posfun
336000041213     C     $FC(1)        IFEQ      '0'
336100041213     C                   SETON                                        9997
336200041213     C                   ENDIF
336300041213     C     $TASTO        WHENEQ    F02
336400041213     C                   z-add     02            posfun
336500041213     C     $FC(2)        IFEQ      '0'
336600041213     C                   SETON                                        9997
336700041213     C                   ENDIF
336800041213     C     $TASTO        WHENEQ    F03
336900041213     C                   z-add     03            posfun
337000041213     C     $FC(3)        IFEQ      '0'
337100041213     C                   SETON                                        9997
337200041213     C                   ENDIF
337300041213     C     $TASTO        WHENEQ    F04
337400041213     C                   z-add     04            posfun
337500041213     C     $FC(4)        IFEQ      '0'
337600041213     C                   SETON                                        9997
337700041213     C                   ENDIF
337800041213     C     $TASTO        WHENEQ    F05
337900041213     C                   z-add     05            posfun
338000041213     C     $FC(5)        IFEQ      '0'
338100041213     C                   SETON                                        9997
338200041213     C                   ENDIF
338300041213     C     $TASTO        WHENEQ    F06
338400041213     C                   z-add     06            posfun
338500041213     C     $FC(6)        IFEQ      '0'
338600041213     C                   SETON                                        9997
338700041213     C                   ENDIF
338800041213     C     $TASTO        WHENEQ    F07
338900041213     C                   z-add     07            posfun
339000041213     C     $FC(7)        IFEQ      '0'
339100041213     C                   SETON                                        9997
339200041213     C                   ENDIF
339300041213     C     $TASTO        WHENEQ    F08
339400041213     C                   z-add     08            posfun
339500041213     C     $FC(8)        IFEQ      '0'
339600041213     C                   SETON                                        9997
339700041213     C                   ENDIF
339800041213     C     $TASTO        WHENEQ    F09
339900041213     C                   z-add     09            posfun
340000041213     C     $FC(09)       IFEQ      '0'
340100041213     C                   SETON                                        9997
340200041213     C                   ENDIF
340300041213     C     $TASTO        WHENEQ    F10
340400041213     C                   z-add     10            posfun
340500041213     C     $FC(10)       IFEQ      '0'
340600041213     C                   SETON                                        9997
340700041213     C                   ENDIF
340800041213     C     $TASTO        WHENEQ    F11
340900041213     C                   z-add     11            posfun
341000041213     C     $FC(11)       IFEQ      '0'
341100041213     C                   SETON                                        9997
341200041213     C                   ENDIF
341300041213     C     $TASTO        WHENEQ    F12
341400041213     C                   z-add     12            posfun
341500041213     C     $FC(12)       IFEQ      '0'
341600041213     C                   SETON                                        9997
341700041213     C                   ENDIF
341800041213     C     $TASTO        WHENEQ    F13
341900041213     C                   z-add     13            posfun
342000041213     C     $FC(13)       IFEQ      '0'
342100041213     C                   SETON                                        9997
342200041213     C                   ENDIF
342300041213     C     $TASTO        WHENEQ    F14
342400041213     C                   z-add     14            posfun
342500041213     C     $FC(14)       IFEQ      '0'
342600041213     C                   SETON                                        9997
342700041213     C                   ENDIF
342800041213     C     $TASTO        WHENEQ    F15
342900041213     C                   z-add     15            posfun
343000041213     C     $FC(15)       IFEQ      '0'
343100041213     C                   SETON                                        9997
343200041213     C                   ENDIF
343300041213     C     $TASTO        WHENEQ    F16
343400041213     C                   z-add     16            posfun
343500041213     C     $FC(16)       IFEQ      '0'
343600041213     C                   SETON                                        9997
343700041213     C                   ENDIF
343800041213     C     $TASTO        WHENEQ    F17
343900041213     C                   z-add     17            posfun
344000041213     C     $FC(17)       IFEQ      '0'
344100041213     C                   SETON                                        9997
344200041213     C                   ENDIF
344300041213     C     $TASTO        WHENEQ    F18
344400041213     C                   z-add     18            posfun
344500041213     C     $FC(18)       IFEQ      '0'
344600041213     C                   SETON                                        9997
344700041213     C                   ENDIF
344800041213     C     $TASTO        WHENEQ    F19
344900041213     C                   z-add     19            posfun
345000041213     C     $FC(19)       IFEQ      '0'
345100041213     C                   SETON                                        9997
345200041213     C                   ENDIF
345300041213     C     $TASTO        WHENEQ    F20
345400041213     C                   z-add     20            posfun
345500041213     C     $FC(20)       IFEQ      '0'
345600041213     C                   SETON                                        9997
345700041213     C                   ENDIF
345800041213     C     $TASTO        WHENEQ    F21
345900041213     C                   z-add     21            posfun
346000041213     C     $FC(21)       IFEQ      '0'
346100041213     C                   SETON                                        9997
346200041213     C                   ENDIF
346300041213     C     $TASTO        WHENEQ    F22
346400041213     C                   z-add     22            posfun
346500041213     C     $FC(22)       IFEQ      '0'
346600041213     C                   SETON                                        9997
346700041213     C                   ENDIF
346800041213     C     $TASTO        WHENEQ    F23
346900041213     C                   z-add     23            posfun
347000041213     C     $FC(23)       IFEQ      '0'
347100041213     C                   SETON                                        9997
347200041213     C                   ENDIF
347300041213     C     $TASTO        WHENEQ    F24
347400041213     C                   z-add     24            posfun
347500041213     C     $FC(24)       IFEQ      '0'
347600041213     C                   SETON                                        9997
347700041213     C                   ENDIF
347800041213     C                   ENDSL
347900041213      *
348000041213     C     XCtrKey       ENDSR
348100041213      ************************************************************
348200041213      * AGGIORNAMENTO VALORI ASSUNTI
348300041213      ************************************************************
348400041213     C     AGGMPE        BEGSR
348500041213      *
348600041213      * preparo i parametri di chiamata al driver
348700041213     C                   CLEAR                   XMPEDS
348800041213     C                   EVAL      TpChiamata = '03'
348900041213     C                   EVAL      NChiavi = 5
349000041213     C                   EVAL      VLogica = 2
349100041213      *
349200041213      * preparo le chiavi per la chiamata al driver
349300041213     C                   CLEAR                   XMPEDS1
349400041213     C                   MOVEL     XSCSOC        DSMPESOC
349500041213     C                   MOVEL     DSPGM         DSMPEPGM
349600041213     C                   MOVEL     KNMUS         DSMPEPRF
349700041213      *
349800041213      * Si memorizzano i dati di default a seconda delle modalità con
349900041213      * cui il pgm è usato
350000041213      * MPETIP = 'PX'  pgm richiamato come lista (CMD4)
350100041213      * MPETIP = 'PV'  pgm richiamato da menù
350200041213      * MPETIP = 'PB'  x lancio Batch
350300041213      *
350400041213     C                   SELECT
350500090331     C     OPZL77        WHENEQ    '01'
350600041213     C                   MOVEL     'PX'          DSMPETIP
350700041213     C                   OTHER
350800041213     C                   MOVEL     'PV'          DSMPETIP
350900041213     C                   ENDSL
351000041213     C                   MOVEL     DSMPE         DSMPEPAR
351100041213      *
351200041213     C                   CALLB     'XMPE'
351300041213     C                   PARM                    XMPEDS
351400051115     C                   PARM                    XMPEDS1
351500041213      *
351600041213     C     XAggMpe       ENDSR
351700041213      ************************************************************
351800041213      * OPERAZIONI INIZIALI
351900041213      ************************************************************
352000041213     C     *INZSR        BEGSR
352100051115     C     *DMY          Move      Udate         ZDateISO
352200051116     C     *JobRun       Move      ZDateISO      ZDate             6 0
352300041213      *
352400041213      * Reperimento parametri
352500041213     C     *ENTRY        PLIST
352600041213     C                   PARM                    KPJBA
352700041213     C                   PARM                    DSAUT
352800041213      *
352900041213      * testo se sono stati passati i dati di autorizzazione
353000041213     C     PARMS         IFLT      2
353100041213     C                   MOVE      *BLANK        AUTDS
353200041213     C                   ELSE
353300041213     C                   MOVEL     DSAUT         AUTDS
353400041213     C                   ENDIF
353500041213      *
353600041213      * Reperimento dati società
353700041213     C                   MOVEL     'SOC001'      TIPXSC
353800041213     C                   MOVEL     *BLANK        SOCXSC
353900041213     C                   EXSR      REPSOC
354000041213      *
354100041213      * Controlli dati società
354200041213     C     RTNXSC        IFNE      '1'
354300041213     C                   MOVEL     XSOCDS        SOC001
354400041213     C                   ELSE
354500041213     C                   MOVE      *ON           $FINE
354600090331     C                   MOVE      '1'           ErrL77
354700041213     C                   EXSR      ENDPGM
354800041213     C                   ENDIF
354900051116     C*
355000090406     C* Reperimento parametri gestione solleciti
355100051116     C*
355200051116     C                   MOVEL     *BLANK        XPARIC
355300051116     C                   MOVEL     XSCSOC        XPASOC
355400090331     C                   MOVEL     'NDCL1S  '    XPAPAR
355500051116     C                   MOVEL     *BLANK        XPAOUT
355600051116     C                   EXSR      REPPAR
355700051116     C     XPAERR        IFEQ      '0'
355800090331     C                   MOVEL     XPAOUT        ANP057DS
355900100513     c                   move      posattr057    posiz             2 0
356000051116     C                   ELSE
356100051116     C                   SETON                                        LR
356200051116     C                   RETURN
356300051116     C                   ENDIF
356400041213      *
356500090406     C*
356600090406     C* Reperimento parametri gestione scadenzario
356700090406     C*
356800090406     C                   MOVEL     *BLANK        XPARIC
356900090406     C                   MOVEL     XSCSOC        XPASOC
357000090406     C                   MOVEL     'SCABASE '    XPAPAR
357100090406     C                   MOVEL     *BLANK        XPAOUT
357200090406     C                   EXSR      REPPAR
357300090406     C     XPAERR        IFEQ      '0'
357400090406     C                   MOVEL     XPAOUT        NDC601DS
357500090406     C                   ELSE
357600090406     C                   SETON                                        LR
357700090406     C                   RETURN
357800090406     C                   ENDIF
357900041213      * reperisco le dimensioni delle variabili che conterranno i tasti
358000090330     C                   EVAL      $D1L01 = %Size(Z1Ke)
358100041213     C                   EVAL      $D1L02 = %Size(Z1Ke2)
358200041213     C                   EVAL      $C1Op1 = %Size(C1Opz1)
358300090331     C                   EVAL      $C2Op1 = %Size(C2Opz1)
358400041213      *
358500041213      * Reperimento opzioni
358600090331     C                   EXSR      REPOPZS1
358700090331     C                   EXSR      REPOPZS2
358800041213      *
358900041213      * Reperimento tasti di funzione
359000041213     C                   EXSR      REPKEY
359100041213      * Chiamata a routine per il reperimento delle opzioni
359200041213      * personalizzate
359300041213    > * se si vogliono abilitare i tasti personalizzati in ogni videata
359400041213    > * ripeterla passando le schiere dei tasti funzione
359500041213     C                   CAllb     'X46USREXT'
359600041213     C                   PARM                    $OP1
359700041213     C                   PARM                    $TX1
359800041213     C                   PARM                    $FL1
359900041213     C                   PARM                    $PGO1
360000041213     C                   PARM      $dimOp        NumOpz
360100090331     C                   PARM                    $FX1
360200090331     C                   PARM                    $FC1
360300041213     C                   PARM                    $PGF2
360400041213     C                   PARM                    DsPgm
360500041213     C                   PARM                    xsccut
360600041213     C                   PARM                    xscgrp
360700041213      *
360800100419     C                   Z-add     1             OPZLEN1
360900100419     C     *blank        Lookup    $TX1(opzlen1)                          21
361000041213     C                   if        *in21 = *on
361100100419     C                   sub       1             OPZLEN1
361200041213     C                   else
361300100419     C                   Z-add     $DimOp        OPZLEN1
361400041213     C                   endif
361500100419     c*
361600100419     C                   Z-add     1             OPZLEN2
361700100419     C     *blank        Lookup    $TX2(opzlen2)                          21
361800100419     C                   if        *in21 = *on
361900100419     C                   sub       1             OPZLEN2
362000100419     C                   else
362100100419     C                   Z-add     $DimOp        OPZLEN2
362200100419     C                   endif
362300041213      *
362400041213     C     XInzSr        ENDSR
362500041213      ************************************************************
362600041213      * INIZIALIZZAZIONE VARIABILI
362700041213      ************************************************************
362800041213     C     INZVAR        BEGSR
362900041213      *
363000041213      * Dati da PGM chiamante
363100090331     C                   MOVEL     KPJBU         NDCL77DS
363200041213      *
363300041213      * Se società diversa da quella impostata nella *inzsr, rieseguo
363400041213      * reperimento dati società
363500090331     C                   IF        SocietaL77 <> xscsoc  and
363600090331     C                             OpzL77 <>*blank
363700041213     C                   MOVEL     'SOC001'      TIPXSC
363800090331     C                   MOVEL     SocietaL77    SOCXSC
363900041213     C                   EXSR      REPSOC
364000041213     C     RTNXSC        IFNE      '1'
364100041213     C                   MOVEL     XSOCDS        SOC001
364200041213     C                   ELSE
364300041213     C                   MOVE      *ON           $FINE
364400090331     C                   MOVE      '1'           ErrL77
364500041213     C                   EXSR      ENDPGM
364600041213     C                   ENDIF
364700041213     C                   ENDIF
364800041213      *
364900041213      * Reperimento autorizzazioni
365000051114     C     AUTDS         IFEQ      *BLANK
365100051114     C                   EXSR      REPAUT
365200051114     C                   ELSE
365300100409     C                   MOVEL     XCADDS        CONRECDS
365400090331     C* Se non autorizzati alla funzione oppure                              ed esco
365500100409    >C     VisATT        IFNE      *ON
365600090331     C                   MOVE      XCAFNC        XNOFNC
365700090331     C                   MOVE      XCAPRF        XNOPRF
365800090331     C                   MOVE      XCACUT        XNOCUT
365900090331     C                   MOVE      XCATCK        XNOTCK
366000090331     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
366100090331     C                   CALL      'XNOAUTR'
366200090331     C                   PARM                    XNOADS
366300090331    >C                   MOVE      '2'           ErrL77
366400090331     C                   MOVE      *ON           $fine
366500090331     C                   EXSR      ENDPGM
366600090331     C                   ENDIF
366700051114     C                   ENDIF
366800050103      *
366900041213      * Reperimento valori default utente
367000041213     C                   EXSR      REPDFT
367100041213      *
367200041213      * Definizione delle opzioni utilizzabili a pgm
367300041213     C                   EXSR      DEFOPZ
367400041213      *
367500041213      * Variabili per gestione videate
367600041213     C                   MOVE      *OFF          $FINE
367700041213     C                   MOVE      *OFF          $INZD1
367800041213     C                   MOVE      *OFF          $INZS1
367900041217     C                   MOVE      *OFF          $INZS2
368000041213     C                   MOVE      *OFF          $INZD2
368100090331     C                   MOVE      *OFF          $EndFileS1
368200090331     C                   MOVE      *OFF          $EndFileS2
368300041213     C                   MOVE      *OFF          $ESCI
368400041213     C                   MOVE      *OFF          $RCDOK
368500041213     C                   MOVE      *HIVAL        $LASTV
368600041213     C                   Z-ADD     0             $ULKD1            3 0
368700041213     C                   Z-ADD     0             $ULKS1            3 0
368800041213     C                   Z-ADD     0             $ULKD2            3 0
368900041213      * azzero la posizione del cursore
369000041213     C                   Z-ADD     0             H1RIGA
369100041213     C                   Z-ADD     0             H1COLO
369200041213      *
369300041213     C                   MOVE      *ON           $INZD1
369400041213     C                   MOVE      *ON           $INZS1
369500041217     C                   MOVE      *ON           $INZS2
369600100419     C                   CLEAR                   SElDTSCA
369700041213      *
369800041213      * se pgm richiamato con chiave univoca impostata,
369900041213      * forzo la emissione della lista e imposto l'ordinamento
370000090331     C*    OpzL77        IFNE      *BLANK
370100090331     C*                  MOVE      *ON           $Unique
370200090331     C*                  Z-ADD     2             $LOP
370300090331     C*                  ELSE
370400090331     C*                  MOVE      *OFF          $Unique
370500090331     C*                  Z-ADD     1             $LOP
370600090331     C*                  ENDIF
370700041213      *
370800041213      * Variabili appoggio
370900041220     C                   Z-ADD     SflPagS1      WPagS1
371000041220     C                   Z-ADD     SflPagS2      WPagS2
371100041213     C                   MOVEL     *OFF          WOPR
371200041213     C                   CLEAR                   W1POS
371300041213      *
371400041213      * definisco qual'è la prima videata
371500041213      * (se la prima videata risulta la lista, devo comunque caricare
371600041213      *  la videata di parzializzazione e gestire la definizione
371700041213      *  dell'ordinamento, perchè, dai default oppure se sono stati
371800041213      *  passati dei dati, può variare il caricamento)
371900041213     C                   SELECT
372000041213     C     $LOP          WHENEQ    1
372100041213     C                   MOVE      'D1'          $GEST
372200041213     C     $LOP          WHENEQ    2
372300041213     C                   MOVE      'S1'          $GEST
372400041213     C                   EXSR      INZD1
372500090331     C                   EXSR      CTRD1
372600090401     C                   MOVE      *ALL'0'       IN5098           49
372700090401     C                   MOVEA     IN5098        *IN(50)
372800041213     C                   MOVE      *OFF          $INZD1
372900090331     C                   EXSR      DEFORD
373000041213     C                   ENDSL
373100041213      *
373200050105      * Campi testata
373300041213     C                   CLEAR                   FMTT1
373400041213     C                   MOVEL     KNSIF         NOMSIF
373500041213     C                   MOVEL     XSCDSI        NOMDIT
373600041213     C                   MOVEL     DSPGM         NOMPGM
373700050105      *
373800041213     C                   MOVEL     *BLANK        T1TIT
373900041213     C     SOTTIT        IFNE      *BLANK
374000041213     C                   MOVEL     SOTTIT        IDMSG
374100041213     C                   CALLB     'XRTVM'
374200041213     C                   PARM                    IDMSG            27
374300041213     C                   PARM                    TXTMSG          100
374400041213     C                   PARM                    ERRMSG            1
374500041213     C                   PARM      *ON           CTRMSG            1
374600041213     C                   PARM      30            LENMSG            3 0
374700041213     C     ERRMSG        IFNE      *ON
374800041213     C                   MOVEL     TXTMSG        T1TIT
374900041213     C                   ELSE
375000041213     C                   MOVEL     *ALL'?'       T1TIT
375100041213     C                   ENDIF
375200041213     C                   ENDIF
375300041213      *
375400041213     C     XInzVar       ENDSR
375500041213      ************************************************************
375600041213      * REPERIMENTO DATI SOCIETA'
375700041213      ************************************************************
375800041213     C     REPSOC        BEGSR
375900041213      *
376000041213     C                   CALLB     'XSOC'
376100041213     C                   PARM                    TIPXSC            6
376200041213     C                   PARM                    SOCXSC            3
376300041213     C                   PARM                    CDSXSC            9 0
376400090331     C                   PARM                    MODXSC            3
376500041213     C                   PARM      *BLANKS       RTNXSC            1
376600041213     C                   PARM                    XSOCDS
376700041213     C                   PARM                    KPJBA
376800041213      *
376900041213     C     XRepSoc       ENDSR
377000051116     C************************************************************
377100051116     C* REPERIMENTO PARAMETRI GESTIONE
377200051116     C************************************************************
377300051116     C     REPPAR        BEGSR
377400051116     C*
377500051116     C                   CALLB     'XPAR'
377600051116     C                   PARM                    XPASOC            3
377700051116     C                   PARM                    XPAPAR            8
377800051116     C                   PARM                    XPAOUT
377900051116     C                   PARM                    XPAERR            1
378000051116     C                   PARM                    XPARIC            1
378100051116     C*
378200051116     C                   ENDSR
378300051116     C/EJECT
378400051114      ************************************************************
378500051114      * REPERIMENTO AUTORIZZAZIONI UTENTE
378600051114      ************************************************************
378700051114     C     REPAUT        BEGSR
378800051114      *
378900051114     C                   CLEAR                   AUTDS
379000051114      *
379100051114     C                   MOVEL     XSCSOC        XCASOC
379200051114     C                   MOVEL     XSCDUN        XCAUNI
379300051114     C                   Z-ADD     0             XCAGRP
379400051114     C                   MOVEL     KNMUS         XCAPRF
379500100409     C                   MOVEL     'CONREC'      XCAFNC
379600051114     C                   MOVEL     *ZERO         XCAVFU
379700090511     C                   MOVE      'REC'         XCAVFU
379800090511     C                   MOVE      'CC'          XCATCK
379900051114     C                   MOVE      '1'           XCANAU
380000051114      *
380100051114     C                   CALLB     'XCHKAUT'
380200051114     C                   PARM                    AUTDS
380300051114      *
380400051114      * Errori/non abilitazione
380500051114     C     XCARTN        IFNE      0
380600090331     C                   MOVE      '2'           ErrL77
380700051114     C                   EXSR      ENDPGM
380800051114     C                   ELSE
380900100409     C                   MOVEL     XCADDS        CONRECDS
381000051114     C* Se non autorizzati alla funzione                                     ed esco
381100100409    >C     VisATT        IFNE      *ON
381200051114     C                   MOVE      XCAFNC        XNOFNC
381300051114     C                   MOVE      XCAPRF        XNOPRF
381400051114     C                   MOVE      XCACUT        XNOCUT
381500051114     C                   MOVE      XCATCK        XNOTCK
381600051114     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
381700051114     C                   CALL      'XNOAUTR'
381800051114     C                   PARM                    XNOADS
381900051114     C                   MOVE      *ON           $FINE
382000051114     C                   EXSR      ENDPGM
382100051114     C                   ENDIF
382200051114     C                   ENDIF
382300051114      *
382400051114     C     XRepAut       ENDSR
382500051114      ************************************************************
382600041213      ************************************************************
382700041213      * REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
382800041213      ************************************************************
382900041213     C     REPDFT        BEGSR
383000041213      *
383100041213      * preparo i parametri di chiamata al driver
383200041213     C                   CLEAR                   XMPEDS
383300041213     C                   EVAL      TpChiamata = '01'
383400041213     C                   EVAL      NChiavi = 5
383500041213     C                   EVAL      VLogica = 2
383600041213      *
383700041213      * preparo le chiavi per la chiamata al driver
383800041213     C                   CLEAR                   XMPEDS1
383900041213     C                   MOVEL     XSCSOC        DSMPESOC
384000041213     C                   MOVEL     DSPGM         DSMPEPGM
384100041213     C                   MOVEL     KNMUS         DSMPEPRF
384200041213      * Si reperiscono i dati di default a seconda delle modalità con
384300041213      * cui il pgm è usato
384400041213      * MPETIP = 'PX'  pgm richiamato come lista (CMD4)
384500041213      * MPETIP = 'PV'  pgm richiamato da menù
384600041213      * MPETIP = 'PB'  x lancio Batch
384700041213     C                   SELECT
384800090331     C     OPZL77        WHENEQ    '01'
384900041213     C                   MOVEL     'PX'          DSMPETIP
385000041213     C                   OTHER
385100041213     C                   MOVEL     'PV'          DSMPETIP
385200041213     C                   ENDSL
385300041213      *
385400041213     C                   CALLB     'XMPE'
385500041213     C                   PARM                    XMPEDS
385600041213     C                   PARM                    XMPEDS1
385700041213      *
385800041213     C     Risultato     IFEQ      '0'
385900041213     C                   MOVEL     DSMPEPAR      DSMPE
386000041213      * nel caso sia la prima volta che l'utente apre questa anagrafica
386100041213      * i suoi valori di default vengono decisi a pgm
386200041213     C                   ELSE
386300090331     C                   Z-ADD     2             $LOP
386400041213     C                   Z-ADD     1             $ORD
386500041213     C                   ENDIF
386600041213      *
386700041213     C     XRepDft       ENDSR
386800041213      ************************************************************
386900041213      * DEFINIZIONE OPZIONI UTILIZZABILI A PGM
387000041213      ************************************************************
387100041213     C     DEFOPZ        BEGSR
387200041213      *
387300041213      * Questa routine deve valorizzare la schiera $FL1
387400041213      * con S o N a seconda della utilizzabilità o meno
387500041213      * dell'opzione correlata
387600041213      *
387700041213      * Inizialmente si considerano tutte le opzioni come valide
387800041213     C     1             DO        $DIMOP        X
387900041213      * Imposta tutti i flg delle opzioni non a *blank, a 'S'
388000041213     C                   If        $TX1(X)  <> *Blank
388100041213     C                   MOVE      'S'           $FL1(X)
388200041213     C                   Else
388300041213     C                   Leave
388400041213     C                   EndIf
388500041213     C                   ENDDO
388600090331     c*
388700090331     C     1             DO        $DIMOP        X
388800090331      * Imposta tutti i flg delle opzioni non a *blank, a 'S'
388900090331     C                   If        $TX2(X)  <> *Blank
389000090331     C                   MOVE      'S'           $FL2(X)
389100090331     C                   Else
389200090331     C                   Leave
389300090331     C                   EndIf
389400090331     C                   ENDDO
389500050103      *
389600041213      *
389700090331      * Questa routine deve valorizzare la schiera $FC1/$FR1
389800041213      * con 1 o 0 a seconda della utilizzabilità o meno
389900041213      * del tasto funzione correlato
390000041213      *
390100041213      * Inizialmente ripristino le abilitazioni come da schiera a tempo
390200041213      * di compilazione
390300041213     C                   RESET                   $FC1
390400041213     C                   RESET                   $FR1
390500041213      *
390600041213      * Dopodichè, a seconda del richiamo e delle autorizzazioni
390700041213      * invalido alcune opzioni
390800041213     C                   SELECT
390900041213      * Se pgm richiamato in ricerca disattivo :
391000090331     C     OPZL77        WHENEQ    '01'
391100041213      * F3=Fine
391200041213     C                   Z-ADD     3             X
391300041213     C                   MOVE      '0'           $FC1(X)
391400041213      *
391500041213      * Se pgm richiamato in interrogazione disattivo :
391600090331     C     OPZL77        WHENEQ    '05'
391700041213      * F10=Immissione
391800041213     C                   Z-ADD     10            X
391900041213     C                   MOVE      '0'           $FC1(X)
392000041221      * 1=Selezione
392100041221     C                   Z-ADD     1             X
392200041221     C     01            LOOKUP    $OP1(X)                                21
392300041221     C     *IN21         IFEQ      *ON
392400041221     C                   MOVE      'N'           $FL1(X)
392500041221     C                   ENDIF
392600041213      *
392700090331     C     OPZL77        WHENEQ    *BLANKS
392800041213      * Se pgm non richiamato, disattivo subito :
392900041213      * F8=Selezione seguente
393000041213     C                   Z-ADD     8             X
393100041213     C                   MOVE      '0'           $FC1(X)
393200041221      **1=Selezione
393300041221     C***                Z-ADD     1             X
393400041221     C***  01            LOOKUP    $OP1(X)                                21
393500041221     C***  *IN21         IFEQ      *ON
393600041221     C***                MOVE      'N'           $FL1(X)
393700041221     C***                ENDIF
393800041213      *
393900041213      * In tutti gli altri casi, disattivo subito :
394000041213     C                   OTHER
394100041217      **1=Selezione
394200041217     C***                Z-ADD     1             X
394300041217     C***  01            LOOKUP    $OP1(X)                                21
394400041217     C***  *IN21         IFEQ      *ON
394500041217     C***                MOVE      'N'           $FL1(X)
394600041217     C***                ENDIF
394700041213      *
394800041213     C                   ENDSL
394900050103      *
395000050103      * poi, disattivo a seconda delle autorizzazioni :
395100041213      *
395200041213     C     XDefOpz       ENDSR
395300041213      ************************************************************
395400041213      * REPERIMENTO OPZIONI
395500041213      ************************************************************
395600090331     C     REPOPZS1      BEGSR
395700041213      *
395800041213     C     1             DO        $DIMOP        X
395900041213      *
396000041213     C     $MS1(X)       IFNE      *BLANK
396100041213      *
396200041213     C                   MOVEL     $MS1(X)       IDMSG
396300041213     C                   CALLB     'XRTVM'
396400041213     C                   PARM                    IDMSG            27
396500041213     C                   PARM                    TXTMSG          100
396600041213     C                   PARM                    ERRMSG            1
396700041213      *
396800041213     C     ERRMSG        IFNE      *ON
396900041213     C                   MOVEL     TXTMSG        $TX1(X)
397000041213     C                   ELSE
397100041213     C                   MOVEL     *ALL'?'       $TX1(X)
397200041213     C                   ENDIF
397300041213      *
397400041213     C                   ENDIF
397500041213      *
397600041213     C                   ENDDO
397700041213      *
397800090331     C     XRepOpzS1     ENDSR
397900090331      ************************************************************
398000090331      * REPERIMENTO OPZIONI
398100090331      ************************************************************
398200090331     C     REPOPZS2      BEGSR
398300090331      *
398400090331     C     1             DO        $DIMOP        X
398500090331      *
398600090331     C     $MS2(X)       IFNE      *BLANK
398700090331      *
398800090331     C                   MOVEL     $MS2(X)       IDMSG
398900090331     C                   CALLB     'XRTVM'
399000090331     C                   PARM                    IDMSG            27
399100090331     C                   PARM                    TXTMSG          100
399200090331     C                   PARM                    ERRMSG            1
399300090331      *
399400090331     C     ERRMSG        IFNE      *ON
399500090331     C                   MOVEL     TXTMSG        $TX2(X)
399600090331     C                   ELSE
399700090331     C                   MOVEL     *ALL'?'       $TX2(X)
399800090331     C                   ENDIF
399900090331      *
400000090331     C                   ENDIF
400100090331      *
400200090331     C                   ENDDO
400300090331      *
400400090331     C     XRepOpzs2     ENDSR
400500041213      ************************************************************
400600041213      * REPERIMENTO TASTI DI FUNZIONE
400700041213      ************************************************************
400800041213     C     REPKEY        BEGSR
400900041213     C                   MOVEA     $FM1          $FM
401000041213     C                   MOVEA     $FC1          $FC
401100041213     C                   EXSR      REPKKK
401200041213     C                   MOVEA     $FX           $FX1
401300041213      *
401400041213     C     XRepKey       ENDSR
401500041213      ************************************************************
401600041213      * REPERIMENTO TASTI DI FUNZIONE
401700041213      ************************************************************
401800041213     C     REPKKK        BEGSR
401900041213      *
402000041213     C                   CLEAR                   $FX
402100041213     C     1             DO        24            X
402200041213      *
402300041213     C     $FM(X)        IFNE      *BLANK
402400041213     C     $Fc(X)        andne     '0'
402500041213      *
402600041213     C                   MOVEL     $FM(X)        IDMSG
402700041213     C                   CALLB     'XRTVM'
402800041213     C                   PARM                    IDMSG            27
402900041213     C                   PARM                    TXTMSG          100
403000041213     C                   PARM                    ERRMSG            1
403100041213      *
403200041213     C     ERRMSG        IFNE      *ON
403300041213     C                   MOVEL     TXTMSG        $FX(X)
403400041213     C                   ELSE
403500041213     C                   MOVEL     *ALL' '       $FX(X)
403600041213     C                   ENDIF
403700041213      *
403800041213     C                   ENDIF
403900041213      *
404000041213     C                   ENDDO
404100041213      *
404200041213     C     XRepKKK       ENDSR
404300041213      ************************************************************
404400041213      * DEFINIZIONE KLIST
404500041213      ************************************************************
404600041213     C     DefKlist      BEGSR
404700041213      *
404800090331     C*
404900090401     C* PLIST per NDVASOG
405000090401     C     PDVASOG       PLIST
405100090401     C                   PARM                    DVARIC
405200090401     C                   PARM                    DVASOCIETA
405300090401     C                   PARM                    DVAUNITA
405400090401     C                   PARM                    DVASTRUTT
405500090401     C                   PARM                    DVADTRIF
405600090401     C                   PARM                    DVALENOUT
405700090401     C* Se si accede con il codice cli/for impostare i campi seguenti
405800090401     C* se si accede con il soggetto ometterli e impostare gli altri
405900090401     C                   PARM                    DVASNATURA
406000090401     C                   PARM                    DVACODICE
406100090401     C                   PARM                    DVALINEAV
406200090401     C                   PARM                    DVAFILIALE
406300090401     C                   PARM                    *omit
406400090401     C                   PARM                    DVATPIND
406500090401     C                   PARM                    DVACDIND
406600090401     C                   PARM                    DVAERRORE
406700090401     C                   PARM                    DVAOUTPUT
406800090409     C*
406900090416    >C* AnUni01L
407000090416    >C     K02Uni01      KLIST
407100090416    >C                   KFLD                    UNISocieta
407200090416    >C                   KFLD                    UNIUnita
407300140313     C* NDSCD01L
407400140313     C     K09Scd01      KLIST
407500140313     C                   KFLD                    ScdSocieta
407600140313     C                   KFLD                    ScdCtb
407700140313     C                   KFLD                    ScdTpScad
407800140313     C                   KFLD                    ScdKcc
407900140313     C                   KFLD                    SCdKsc
408000140313     C                   KFLD                    SCdDtPar
408100140313     C                   KFLD                    SCdNrPar
408200140313     C                   KFLD                    SCdSerPar
408300140313     C                   KFLD                    SCdDtSca
408400131031     C*
408500131031    >C* ndRCA01l
408600131031    >C     K08Rca01      KLIST
408700131031    >C                   KFLD                    RcaSocieta
408800131031    >C                   KFLD                    RcaPosiz
408900131031    >C                   KFLD                    RcaKCC
409000131031    >C                   KFLD                    RcaKSC
409100131031    >C                   KFLD                    RcaDtpar
409200131031    >C                   KFLD                    RcaNrpar
409300131031    >C                   KFLD                    RcaSerPar
409400131031    >C                   KFLD                    RcaDtSca
409500090407     C*
409600100524    >C* ndRCA01l
409700100524    >C     K10Rca01      KLIST
409800100524    >C                   KFLD                    RcaSocieta
409900100524    >C                   KFLD                    RcaPosiz
410000100524    >C                   KFLD                    RcaKCC
410100100524    >C                   KFLD                    RcaKSC
410200100524    >C                   KFLD                    RcaDtpar
410300100524    >C                   KFLD                    RcaNrpar
410400100524    >C                   KFLD                    RcaSerPar
410500100524    >C                   KFLD                    RcaDtSca
410600100524    >C                   KFLD                    RcaFase
410700100524    >C                   KFLD                    RcaProgr
410800100524    >C     K09Rca01      KLIST
410900100524    >C                   KFLD                    RcaSocieta
411000100524    >C                   KFLD                    RcaPosiz
411100100524    >C                   KFLD                    RcaKCC
411200100524    >C                   KFLD                    RcaKSC
411300100524    >C                   KFLD                    RcaDtpar
411400100524    >C                   KFLD                    RcaNrpar
411500100524    >C                   KFLD                    RcaSerPar
411600100524    >C                   KFLD                    RcaDtSca
411700100524    >C                   KFLD                    RcaFase
411800100524    >C* ndRCA04l
411900100524    >C     K11Rca04      KLIST
412000100416    >C                   KFLD                    RcaSocieta
412100100416    >C                   KFLD                    RcaPosiz
412200100416    >C                   KFLD                    RcaKCC
412300100416    >C                   KFLD                    RcaKSC
412400100416    >C                   KFLD                    RcaDtpar
412500100416    >C                   KFLD                    RcaNrpar
412600100416    >C                   KFLD                    RcaSerPar
412700100416    >C                   KFLD                    RcaDtSca
412800100416    >C                   KFLD                    RcaDtAtt
412900100416    >C                   KFLD                    RcaFase
413000100419    >C                   KFLD                    RcaProgr
413100100524    >C     K08Rca04      KLIST
413200100415    >C                   KFLD                    RcaSocieta
413300100415    >C                   KFLD                    RcaPosiz
413400100415    >C                   KFLD                    RcaKCC
413500100415    >C                   KFLD                    RcaKSC
413600100415    >C                   KFLD                    RcaDtpar
413700100415    >C                   KFLD                    RcaNrpar
413800100415    >C                   KFLD                    RcaSerPar
413900100415    >C                   KFLD                    RcaDtSca
414000100524    >C     K07Rca04      KLIST
414100100415    >C                   KFLD                    RcaSocieta
414200100415    >C                   KFLD                    RcaPosiz
414300100415    >C                   KFLD                    RcaKCC
414400100415    >C                   KFLD                    RcaKSC
414500100415    >C                   KFLD                    RcaDtpar
414600100415    >C                   KFLD                    RcaNrpar
414700100415    >C                   KFLD                    RcaSerPar
414800100507     C* NDIMP01L
414900100507     C*
415000100507     c     K08IMP01      KLIST
415100100507     C                   KFLD                    IMPSocieta
415200100507     C                   KFLD                    IMPKcc
415300100507     C                   KFLD                    IMPKsc
415400100507     C                   KFLD                    IMPDtPar
415500100507     C                   KFLD                    IMPNrPar
415600100507     C                   KFLD                    IMPSerPar
415700100507     C                   KFLD                    IMPDtSca
415800100507     C                   KFLD                    IMPProgr
415900041213      *
416000041213     C     XDefKlist     ENDSR
416100090415     C************************************************************
416200090415     C* Chiamata a xrtvm
416300090415     C************************************************************
416400090415     C     PrendiMsg     BEGSR
416500090415     C*
416600090415     C                   CALLB     'XRTVM'
416700090415     C                   PARM                    IDMSG            27
416800090415     C                   PARM                    TXTMSG          100
416900090415     C                   PARM                    ERRMSG            1
417000090415     C                   PARM      *off          CTRMSG            1            centratura
417100090415     C                   PARM                    LENMSG            3 0          lun output.
417200090415     C                   PARM                    MSgDta          100            lun output.
417300090415     C                   PARM                    TxtMsg30         30            lun output.
417400090415     C                   PARM                    TxtMsg3000     3000            lun output.
417500090415     C     ERRMSG        IFNE      *ON
417600090415     C                   MOVEL     TxTMsg        WWMSG           100            *
417700090415     C                   ELSE
417800090415     C                   MOVEL     *ALL'?'       WWMSG
417900090415     C                   ENDIF
418000090415     C*
418100090415     C                   ENDSR
418200100416     C******************************************************
418300100416     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
418400100416     C******************************************************
418500100416     C     MSGPGM        BEGSR
418600100416     C*
418700100416     C                   Z-ADD     2             MSGMSG
418800100416     C                   MOVEL     'S'           MSGRSP
418900100416     C                   MOVEL     'CR'          MSGTPR
419000100416     C                   MOVEL     'S'           MSGEMV
419100100416     C                   Z-ADD     10            MSGRGP
419200100416     C                   MOVEL     'S'           MSGLCK
419300100416     C                   MOVEA     STATUS        STS
419400100416     C                   MOVEL     'N'           MSGCNL
419500100416     C                   MOVEL     'N'           MSGVID
419600100416     C                   MOVEL     'N'           MSGOPE
419700100416     C                   MOVEL     'N'           MSGSTP
419800100416     C                   CALL      'XMSGR'
419900100416     C                   PARM                    XMSGDS
420000100416     C*
420100100416     C                   ENDSR
420200100416     C/EJECT
420300041213      ************************************************************
420400090331** $MS1 / $TX1 opzioni sfl1
420500100413PROMSG    *LIBL     OPZ0001
420600041213** $OP1 / $FL1
42070010041301S      <Scelta
420800090331** $MS2 / $TX2 opzioni sfl2
420900100419PROMSG    *LIBL     OPZ0004
421000100413PROMSG    *LIBL     OPZ0652
421100100413PROMSG    *LIBL     OPZ0458
421200090331** $OP2 / $FL2
42130010041904S      <Cancella
42140010041314S      <Note
42150010041515S      <Chiusura fase
421600041213** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
421700041213PROMSG    *LIBL     KEY0001  01
421800041213PROMSG    *LIBL     KEY0002  02
421900041213PROMSG    *LIBL     KEY0003  03
422000041213PROMSG    *LIBL     KEY0004  04
422100041213PROMSG    *LIBL     KEY0005  05
422200041213PROMSG    *LIBL     KEY0006  06
422300041213PROMSG    *LIBL     KEY0007  07
422400041213PROMSG    *LIBL     KEY0008  08
422500090511PROMSG    *LIBL     KEY0356  09
422600100415PROMSG    *LIBL     KEY0631  10
422700130916PROMSG    *LIBL     KEY0639  11                *C2226*
422800041213PROMSG    *LIBL     KEY0012  12
422900041213PROMSG    *LIBL     KEY0013  13
423000041213PROMSG    *LIBL     KEY0014  14
423100090331PROMSG    *LIBL     KEY0053  15
423200140604YBARMSG   *LIBL     KEY0116  16
423300041213PROMSG    *LIBL     KEY0017  17
423400140604PROMSG    *LIBL     KEY0018  18
423500041213PROMSG    *LIBL     KEY0019  19
423600041213PROMSG    *LIBL     KEY0020  20
423700041213PROMSG    *LIBL     KEY0021  21
423800041213PROMSG    *LIBL     KEY0022  22
423900041213PROMSG    *LIBL     KEY0023  23
424000041213PROMSG    *LIBL     KEY0024  24
424100041213**  ABILITAZIONE D1 : $FC1 / $FR1
4242000412130N  01
4243000903310N  02
4244000412131N  03
4245000903310N  04
4246000412130N  05
4247000412130N  06
4248000412130N  07
4249000412131N  08
4250000905111N  09
4251001403310N  10
4252001309161N  11                                 *C2226*
4253000412131N  12
4254000412130N  13
4255000412130N  14
4256000903311N  15
4257001406101N  16
4258000412130N  17
4259001406040N  18
4260000412130N  19
4261000412130N  20
4262000412130N  21
4263000412130N  22
4264000412130N  23   Impostare a '1' se si utilizzano opzioni
4265000412131N  24   Lasciare sempre impostata a '1' il valore della riga 24
426600090415** MSG
426700090415PROMSG    *LIBL     COS2762    Telefono
426800100513PROMSG    *LIBL     COS2778    Contatto
426900090416PROMSG    *LIBL     COS2764    Unità
427000090416PROMSG    *LIBL     COS0426    Tutte
427100110408PROMSG    *LIBL     KEY0012    F12=Ritorno                D2680
