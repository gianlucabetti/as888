000100070524     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200070524     H*PARMS BNDDIR(PJXBND)
000300070524     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000400070524     H*PARMS DYNUSRPRF(*OWNER) RDB(*NONE)
000500940211     H DECEDIT('0,') DATEDIT(*DMY.)
000600941115      *
000700090323    > *                Lista Attività Recupero credito
000800090323      *           -------------------------------------
000900090324     FAnUni01L  IF   E           K DISK
001000130923C2226FNdAtt01L  IF   E           K DISK
001100100413     FNdRca01L  IF   E           K DISK
001200101209c2176FNdRca04L  UF   E           K DISK
001300101209c2176F                                     RENAME(NDRCA000:NDRCA04)
001400940211     F*------------
001500090323    >FNDCL76D   CF   E             WORKSTN usropn
001600090323    >F                                     RENAME(CL76D1:FMTD1)
001700090323    >F                                     RENAME(CL76D2:FMTD2)
001800090323    >F                                     RENAME(CL76D3:FMTD3)
001900090323    >F                                     RENAME(CL76S1:FMTS1)
002000090323    >F                                     RENAME(CL76C1:FMTC1)
002100130911C2226F                                     RENAME(CL76C2:FMTC2)
002200130911C2226F                                     RENAME(CL76S2:FMTS2)
002300090323    >F                                     RENAME(CL76T1:FMTT1)
002400090323    >F                                     RENAME(CL76Z1:FMTZ1)
002500090323    >F                                     RENAME(CL76Z2:FMTZ2)
002600090323    >F                                     RENAME(CL76Z3:FMTZ3)
002700110412D2680F                                     RENAME(CL76D5:FMTD5)
002800940131     F                                     SFILE(FMTS1:S1NRR)
002900130911C2226F                                     SFILE(FMTS2:S2NRR)
003000940201     F                                     INFDS(DSFMT)
003100940128     D*----------------------------------------------------*
003200940302     D* Schiere per la gestione delle opzioni
003300940302     D*
003400960117     D* dimensione della schiera $MS1, da non cambiare
003500960117     D $DIMOP          C                   CONST(30)
003600950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003700950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
003800950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003900950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
004000130912C2226D $OP2            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004100130912C2226D $FL2            S              1    DIM($dimop) ALT($OP2)
004200960117     D* schiera contenente i nomi dei programma richiamati attraverso
004300960117     D* le opzioni personalizzate
004400960117     D $PGO1           S             10    DIM($dimop)
004500941214     D $TX             S             50    DIM(50)
004600941214     D $FL             S              1    DIM(50)
004700940909     D* Schiere generali per la gestione dei tasti di funzione
004800940506     D $FM             S             27    DIM(24)
004900940506     D $FX             S             50    DIM(24)
005000940506     D $FC             S              1    DIM(24)
005100940506     D $FR             S              1    DIM(24)
005200940909     D* Schiere per la gestione dei tasti di funzione
005300940506     D* Video D1
005400940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005500940506     D $FX1            S             50    DIM(24) ALT($FM1)
005600940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005700940506     D $FR1            S              1    DIM(24) ALT($FC1)
005800960202     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005900960202     D* funzione personalizzata S1
006000960202     D $PGF2           S             10    DIM(25)
006100940506     D* Schiere per la gestione dei tasti di funzione
006200940506     D* Video S1
006300940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006400940506     D $FX2            S             50    DIM(24) ALT($FM2)
006500940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006600940506     D $FR2            S              1    DIM(24) ALT($FC2)
006700940916     D* Schiere per la gestione dei tasti di funzione
006800940916     D* Video D2
006900940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
007000940916     D $FX3            S             50    DIM(24) ALT($FM3)
007100940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007200940916     D $FR3            S              1    DIM(24) ALT($FC3)
007300090325     D*-------------
007400090325     D* Video S1
007500090325     D WC1Rcd          S                   like(c1nrr) inz
007600090325     D $OpzOk          S              1
007700090325     D WOpz            S                   like(S1Opz) inz
007800090325     D $X              S                   like(c1nrr) inz
007900950726     D*
008000130911C2226D* Video S2
008100130911  "  D WC2Rcd          S                   like(c2nrr) inz
008200130911  "  D $OpzOkS2        S              1
008300130911  "  D WOpzS2          S                   like(S2Opz) inz
008400130911  "  D $XS2            S                   like(c2nrr) inz
008500130911C2226D*
008600950726     D $D1L01          S              3  0
008700950726     D $D1L02          S              3  0
008800950726     D $S1L01          S              3  0
008900950726     D $S1L02          S              3  0
009000130911C2226D $S2L01          S              3  0
009100130911C2226D $S2L02          S              3  0
009200950906     D $C1Op1          S              3  0
009300950906     D $C1Op2          S              3  0
009400950906     D $D2L01          S              3  0
009500950906     D $D2L02          S              3  0
009600130911C2226D $C2Op1          S              3  0
009700130911C2226D $C2Op2          S              3  0
009800940302     D*
009900960202     D*-------------
010000960202     D* Indice della schiera tasti funzione
010100960202     D PosFun          S              2  0
010200940211     D/EJECT
010300940128     D*----------------------------------------------------*
010400100402     DDsFetch          Ds
010500100407     D DsSocieta                           like(RcaSocieta)
010600100402     D DsPosiz                             like(RcaPosiz)
010700100407     D DsAttrib                            like(RcaAttrib)
010800100402     D DsKcc                               like(RcaKcc)
010900100402     D DsKsc                               like(RcaKsc)
011000100402     D DsDtPar                             like(RcaDtPar)
011100100402     D DsNrPar                             like(RcaNrPar)
011200100402     D DsSerPar                            like(RcaSerPar)
011300100419     D Dsdivisa                            like(RcaDivisa)
011400100407     D DskscAge                            like(RcakscAge)
011500130910C2226DDsFetchPre       Ds
011600130910  "  D Ds2Societa                          like(RcaSocieta)
011700130918  "  D Ds2Unita                            like(RcaUnita)
011800130918  "  D Ds2Kcc                              like(RcaKcc)
011900130910  "  D Ds2CdFisc                     20
012000130910  "  D Ds2Partiva                    20
012100130918  "  D Ds2Sogg                        8
012200130919  "  D Ds2descr                      44
012300130918  "  D Ds2Importo                    20  4
012400130918  "  D*
012500130919  "  D DsfetchTab      DS
012600130918  "  D KccPre                              like(H2kcc)
012700130918  "  D Partivapre                          like(H2partiva)
012800130918  "  D CdfiscPre                           like(H2CdFisc)
012900130918  "  D SoggPre                             like(H2Sogg)
013000130918  "  D DescrPre                            like(S2descr)
013100130919  "  D ImpPre                        19  3
013200130918  "  D*
013300130918  "  D $PKcc           S                   Like(RcaKcc)
013400130919  "  D $PSogg          S                   Like(H2Sogg)
013500130918  "  D $PPartiva       S                   Like(H2Partiva)
013600130918  "  D $PCdFisc        S                   Like(H2CdFisc)
013700130919  "  D $First          S              1    inz
013800130918C2226D*
013900940211     D*-------------
014000940207     D* Valori assunti
014100950524     D XMPEDS        E DS                  INZ
014200950524     D XMPEDS1       E DS                  INZ
014300941111     D DSMPE           DS                  INZ
014400940207     D  $LOP                   1      1  0
014500940831     D  $ORD                   2      3  0
014600130912C2226D  $PRE                   4      4
014700130912C2226D  $RGRPRE                5      5
014800940207     D*-------------
014900940211     D* Passaggio Parametri
015000940211     D KPJBA         E DS
015100940211     D*-------------
015200940225     D* Reperimento Autorizzazioni
015300941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015400070531     D*-------------
015500070531     D* Dati per pgm XNOAUT
015600070531     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
015700941124     D*-------------
015800950410     D* Passaggio Autorizzazioni
015900950410     D DSAUT           DS           512
016000090325     D*-------------
016100090325     D* Maschera dati autorizzazioni
016200100409    >D CONRECDS      E DS
016300941209     D*-------------
016400130909C2226D* Autorizzazioni alla unità
016500130909  "  D XAUTUNIDS     E DS
016600130909C2226D*-------------
016700941209     D* Dati di ambiente ottenuti da XSOC
016800970424     D SOC001        E DS                  EXTNAME(XSOC001DS)
016900090324     D    FgO1                        1    overlay(XscFgO)
017000941209     D*-------------
017100941209     D* DS Interna per dati di output di XSOC
017200941209     D XSOCDS          DS          1000
017300070629     D*-------------
017400070629     D* DS gestione parametri modulo
017500070629     D XPAOUT          DS          2000
017600090324     D anp057ds      E DS
017700940325     D*-------------
017800940325     D* Parametri in ricezione
017900090323    >D NDCL76DS      E DS
018000090330     D*-------------
018100090330      * Parametri per scheda cliente
018200090330     D NDCL77DS      E DS                  inz
018300090324     D*-------------
018400090324      * Parametri per ricerca unita
018500090324     Dana009ds       E DS                  inz
018600090323     D*-------------
018700090323     D* Ricerca capoconti
018800090323     D A200DS        E DS                  EXTNAME(ANA200DS) INZ
018900090323     D*-------------
019000090323     D* Ricerca sottoconti
019100090323     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
019200090324     D*-------------
019300090324     D XATBDS        E DS                  INZ
019400090324     D*-------------
019500090324     D* Parametri per ricerca codice ANATB00F
019600090324     D XTABDS        E DS                  INZ
019700090416     D*-------------
019800100407     D* Tabella azioni
019900100407     D Ang047DS      E DS                  Inz
020000100407     D*-------------
020100100407     D* Tabella destinatari
020200100401     D Ang048DS      E DS                  Inz
020300090409     D*-------------------------------------
020400090409     D*  ds  attributi contabili
020500090409     D Ang0A2ds      E DS                  INZ
020600090409     D Ang0A1ds      E DS                  INZ
020700090409     D*-------------------------------------
020800090324     D* Ricerca legende
020900090324     D Ana964ds      E DS                  INZ
021000090324     D*-------------
021100090324     D* Controlli validità legende
021200090324     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
021300090324     D*-------------
021400090324     D* Ricerca profili
021500090324     D Ana001ds      E DS                  inz
021600090324     D*----------------------------------------------------*
021700090324     D* Reperimento profilo
021800090324     D parmok          s              1
021900090324     D* parametro utente
022000090324     D ANUTNParm       s                   like(ANUTN00F)
022100090324     D* lunghezza parametro
022200090324     D LungUTN         S              5s 0
022300090324     D*
022400090324     D Anutn00f      e ds
022500070524     D*-------------
022600070524     D* Parametri del PGM di interrogazione conti
022700070524    >D ANA399DS      E DS                  inz
022800070524    >D  A39RET       E                     INZ('0')
022900070524    >D  A39ERRORI    E                     INZ('0')
023000130919C2226D*-------------
023100130919  "  D* Parametri del PGM di interrogazione conti
023200130919C2226D NDC981DS      E DS                  inz
023300090406     D*-------------
023400070525     D* Controllo conti
023500070525     D ND002DS       E DS
023600090409     D Categorie             131    250
023700100402     D*-------------
023800100402     D* Ricerca agenti
023900100402     D A400DS        E DS                  EXTNAME(ANA400DS) INZ
024000070525     D*-------------
024100090409     D* Schiera categorie da ND002DS
024200090409     D DsCateg         DS           120
024300090409     D SkCateg                        4    dim(30)
024400090409     D*-------------
024500070525     D* Definizioni parametri standard chiamate moduli
024600070525     D$Esito           S              1
024700070525     D$gesmsg          S              1
024800070525     D$lenout          S              9B 0
024900070525     D$struttura       S             10
025000070525     D$kcc             S              6
025100070525     D$ksc             S              8
025200090323     D*-------------
025300960202     D* campo di salvataggio per la DS d'ingresso durante la chiamata
025400960202     D* alla funzione personalizzata
025500090323    >D $SaveDS         S                   like(NDCL76DS)
025600090325     D*-------------
025700090325     D*tasto precedente = F13=selezione multipla
025800090325     D  $TastoSav      s                   Like($Tasto) inz
025900130911C2226D  $TastoSav2     s                   Like($Tasto) inz
026000090325     D*emetto al window di conf.canc. solo x il 1° rec.da cancellare
026100090325     D  PrimaVolta     s              1  0 inz(0)
026200960202     D*-------------
026300940211     D DSFMT           DS           512
026400940506     D  $TASTO               369    369
026500940211     D  NRG                  370    370
026600940211     D  NCL                  371    371
026700940211     D  SFLNRR               378    379B 0
026800090324      * riga cursore window
026900090324     D  NRgWin               382    382
027000090324      * colonna cursore window
027100090324     D  NClWin               383    383
027200940211     D*-------------
027300940211     D* posizione cursore
027400940211     D CURSOR          DS
027500940211     D  RIRI                   1      2B 0 INZ
027600940211     D  R$$                    2      2
027700940211     D  COCO                   3      4B 0 INZ
027800940211     D  C$$                    4      4
027900940127     D*-------------
028000940127     D* Reperimento nome PGM
028100940127     D STATUS         SDS           333
028200940127     D  DSPGM            *PROC
028300950411     D  PARMS            *PARMS
028400101209c2176D  SDSUSR               254    263
028500940207     D*-------------
028600940616     D* Costanti
028700940616     D* nr.di righe del sfl (SFLPAG)
028800090330     D SFLPAG          C                   CONST(16)
028900940616     D*-------------
029000940506     D* Tasti di funzione
029100940506     D F01             C                   CONST(X'31')
029200940506     D F02             C                   CONST(X'32')
029300940506     D F03             C                   CONST(X'33')
029400940506     D F04             C                   CONST(X'34')
029500940506     D F05             C                   CONST(X'35')
029600940506     D F06             C                   CONST(X'36')
029700940506     D F07             C                   CONST(X'37')
029800940506     D F08             C                   CONST(X'38')
029900940506     D F09             C                   CONST(X'39')
030000940506     D F10             C                   CONST(X'3A')
030100940506     D F11             C                   CONST(X'3B')
030200940506     D F12             C                   CONST(X'3C')
030300940506     D F13             C                   CONST(X'B1')
030400940506     D F14             C                   CONST(X'B2')
030500940506     D F15             C                   CONST(X'B3')
030600940506     D F16             C                   CONST(X'B4')
030700940506     D F17             C                   CONST(X'B5')
030800940506     D F18             C                   CONST(X'B6')
030900940506     D F19             C                   CONST(X'B7')
031000940506     D F20             C                   CONST(X'B8')
031100940506     D F21             C                   CONST(X'B9')
031200940506     D F22             C                   CONST(X'BA')
031300940506     D F23             C                   CONST(X'BB')
031400940506     D F24             C                   CONST(X'BC')
031500940506     D ENTER           C                   CONST(X'F1')
031600940506     D ROLDWN          C                   CONST(X'F4')
031700940506     D ROLLUP          C                   CONST(X'F5')
031800961017     D*-------------
031900021010     D* Attributi video
032000021010     D Normale         C                   Const(X'32')
032100021010     D Protetto        C                   Const(X'A0')
032200021010     D*-------------
032300950524     D* Sottotitolo di testata
032400070524     D SOTTIT          C                   CONST(' ')
032500070524     D*SOTTIT          C                   CONST('PROMSG    *LIBL-
032600070524     D*                                         COSxxxx')
032700960109     D*-------------
032800950606     D* Variabili per gestione videate
032900950606     D $Gest           S              2A
033000950606     D $Fine           S              1A
033100950606     D $InzD1          S              1A
033200950606     D $InzS1          S              1A
033300950606     D $InzD2          S              1A
033400130911C2226D $InzS2          S              1A
033500950606     D $LastG          S              2A
033600950606     D $LastV          S              2A
033700950606     D $EndFile        S              1A
033800130911C2226D $EndFilePre     S              1A
033900950606     D $Esci           S              1A
034000950606     D $RcdOK          S              1A
034100130911C2226D $RcdOKPre       S              1A
034200950606     D $Unique         S              1A
034300950606     D*-------------
034400950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
034500950606     D S1Nrr           S                   like(C1Rcd)
034600130911C2226D S2Nrr           S                   like(C2Rcd)
034700950606     D WSfl            S                   like(C1Nrr)
034800130911C2226D WSfl2           S                   like(C1Nrr)
034900950606     D WMax            S                   like(C1Rcd)
035000130911C2226D WMaxS2          S                   like(C2Rcd)
035100950606     D WPag            S                   like(C1Rcd)
035200130911C2226D WPag2           S                   like(C1Rcd)
035300950606     D WInzS1          S                   like($InzS1)
035400130911C2226D WInzS2          S                   like($InzS2)
035500090323    >D WOpr            S                   like(OprL76)
035600090323     D CurR            S              3S 0
035700090323     D CurC            S              3S 0
035800950606     D Win             S             99A
035900950606     D W1Pos           S                   like(C1Pos)
036000130911C2226D W2Pos           S                   like(C2Pos)
036100950606     D WINKR           S              1A
036200970912     D WExf            S              1A
036300070711     D ZDateISO        S               D
036400070525     D*-------------
036500070525     D* Variabili specifiche del programma
036600950606     D*-------------
036700100420     D WDtParDa        S                   like(Zdateiso)
036800100420     D WDtParA         S                   like(Zdateiso)
036900100401     D WDtScaDa        S                   like(Zdateiso)
037000100401     D WDtScaA         S                   like(Zdateiso)
037100100401     D WDtAttDa        S                   like(Zdateiso)
037200100401     D WDtAttA         S                   like(Zdateiso)
037300100416     D WDtSolDa        S                   like(Zdateiso)
037400100416     D WDtSolA         S                   like(Zdateiso)
037500070525     D Wdate           S               D
037600130918C2226D WInUni          S           4000
037700130913  "  D WExi            S           1024
037800130913  "  D* ds nota di chiusura fase
037900130913  "  D d5nota          DS           400
038000130913  "  D d5nota1                 1     36
038100130913  "  D d5nota2                37     88
038200130913  "  D d5nota3                89    140
038300130913  "  D d5nota4               141    192
038400130913  "  D d5nota5               193    244
038500130913  "  D d5nota6               245    296
038600130913  "  D d5nota7               297    348
038700130913C2226D d5nota8               349    400
038800070525     D*-------------
038900950606     D*  Indici
039000070524     D X               S              5S 0
039100061116     D Y               S              5S 0
039200960117     D* Indice della schiera per le opzioni personalizzate
039300960117     D PosOpz          S              2  0
039400960117     D* numero max opzioni
039500960117     D NumOpz          S              2  0 Inz($DIMOP)
039600960117     D* numero effettive opzioni
039700960117     D OpzLen          S              2  0
039800070525     D*-------------
039900070525     D* Stringa per SQL
040000130919C2226X*** SQ              DS          5000
040100130919C2226D SQ              DS          5000
040200070531     D*-------------
040300070531     D $A              s             79
040400070531     D $B              s             79
040500070725     D*-------------
040600070725     D* DS Esterna gestione messaggi                                            *
040700070725     D XMSGDS        E DS                  inz                                  *
040800070725     D  STS          E                     EXTFLD(MSGSTS)                       *
040900070725     D                                     DIM(333)                             *
041000070725     D  JBA          E                     EXTFLD(MSGJBA)                       *
041100070725     D                                     DIM(502)                             *
041200070725     D  MSGRSP       E                     INZ('N')
041300070725     D  MSGEMV       E                     INZ('S')
041400070725     D  MSGMSG       E                     INZ(0)
041500070725     D  MSGRGP       E                     INZ(10)
041600070725     D  MSGCNL       E                     INZ('N')
041700070725     D  MSGVID       E                     INZ('N')
041800070725     D  MSGOPE       E                     INZ('N')
041900070725     D  MSGSTP       E                     INZ('N')
042000070725     D  MSGLCK       E                     INZ('N')
042100070725     D*---------------
042200090409    9DPuntini          C                   const('...................')
042300100407     D*  bottone presente
042400100407     D PRESENTE        C                   CONST(X'A1')
042500100407     D PRESERED        C                   CONST(X'A9')
042600100407     D PRESEPNK        C                   CONST(X'B9')
042700100408     D PRESEBLU        C                   CONST(X'BB')
042800100412     D PRESEHIG        C                   CONST(X'23')
042900100511     D PRESETRQ        C                   CONST(X'B1')
043000110412D2680D WWMSG           s                   like(Txtmsg)
043100110412  "  D* schiera costanti
043200110412D2680D MSG             S             27    DIM(10) CTDATA PERRCD(1)             messaggi possibili
043300940127     C*----------------------------------------------------*
043400940127     C*                MAIN LINE PROGRAM
043500940127     C*----------------------------------------------------*
043600940127     C*
043700940223     C* inizializzazione variabili
043800940223     C                   EXSR      INZVAR
043900940223     C*
044000940223     C     $FINE         DOWEQ     *OFF
044100940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
044200940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
044300940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
044400130911C2226C     $GEST         CASEQ     'S2'          GESS2                          Lista
044500941027     C                   ENDCS
044600941027     C                   ENDDO
044700940325     C* fine programma
044800940325     C                   EXSR      ENDPGM
044900940225     C*
045000940325     C/EJECT
045100940325     C************************************************************
045200940325     C* FINE PROGRAMMA
045300940325     C************************************************************
045400940325     C     ENDPGM        BEGSR
045500940325     C*
045600940325     C* passaggio dati al pgm chiamante
045700090323    >C                   MOVEL     WOPR          OPRL76                                     ne S1
045800090323    >C                   MOVEL     NDCL76DS      KPJBU
045900940325     C*
046000940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
046100090323    >C     OPZL76        IFEQ      *BLANK
046200940325     C                   SETON                                            LR
046300940325     C* altrimenti chiudo in RT
046400940325     C                   ELSE
046500940325     C                   SETON                                            RT
046600940325     C                   ENDIF
046700940616     C                   RETURN
046800940325     C*
046900940325     C                   ENDSR
047000940224     C/EJECT
047100940201     C************************************************************
047200940201     C* RICERCHE
047300940201     C************************************************************
047400940127     C     SEARCH        BEGSR
047500940127     C*
047600090324     C* determino Riga/Colonna del cursore
047700090324     c                   if        $lastV='D2'
047800090324     C                   MOVE      NRGWIN        R$$
047900090324     C                   MOVE      NCLWIN        C$$
048000090324     C                   else
048100090324     C                   MOVE      NRG           R$$
048200090324     C                   MOVE      NCL           C$$
048300090324     c                   endif
048400940127     C                   Z-ADD     RIRI          CURR
048500940127     C                   Z-ADD     COCO          CURC
048600090323     C*
048700090323     C     D1KCC         IFNE      *BLANKS
048800090323     C     D1KCC         ANDNE     'C     '
048900090323     C     D1KCC         ANDNE     'F     '
049000090323     C                   CALL      'XALLINEA'
049100090323     C                   PARM                    D1KCC
049200090323     C                   PARM      6             LEN               5 0
049300090323     C                   PARM                    XSCINP
049400090323     C                   ENDIF
049500090323     C*
049600090323     C     D1KSC         IFNE      *BLANKS
049700090323     C                   CALL      'XALLINEA'
049800090323     C                   PARM                    D1KSC
049900090323     C                   PARM      8             LEN               5 0
050000090323     C                   PARM                    XSCINP
050100090323     C                   ENDIF
050200100402     C*
050300100402     C* agente recupero crediti
050400100402     C     D1KSCAGE      IFNE      *BLANKS
050500100402     C                   CALLB     'XALLINEA'
050600100402     C                   PARM                    D1KSCAGE
050700100402     C                   PARM      8             LEN               5 0
050800100402     C                   PARM                    XSCINP
050900100402     C                   ENDIF
051000090323     C*
051100090323   B1C                   SELECT
051200090324    >C* Ricerca unita
051300090324     C     H1NMFL        wheneq    'D1UNITA'
051400090324     C                   RESET                   Ana009ds
051500090324     C                   MOVE      '01'          opz009
051600090324     C                   MOVE      xscsoc        soc009
051700090324     C                   MOVEL     Ana009ds      KPJBU
051800090324     C                   CALL      'ANA009R'
051900090324     C                   PARM                    KPJBA
052000090324     C                   MOVEL     KPJBU         Ana009ds
052100090324     C*
052200090324     C     uni009        IFNE      *BLANK
052300090324     C                   MOVE      uni009        d1unita
052400090324     C                   ENDIF
052500090324     C* Ricerca capoconto
052600090323     C     H1NMFL        WHENEQ    'D1KCC'
052700090323     C                   RESET                   A200DS
052800090323     C                   MOVEL     '01'          OPZ200
052900090323     C                   MOVE      D1KCC         KCC200
053000090323     C                   MOVEL     A200DS        KPJBU
053100090323     C                   CALL      'ANA200R'
053200090323     C                   PARM                    KPJBA
053300090323     C                   MOVEL     KPJBU         A200DS
053400090323     C*
053500090323     C     KCC200        IFNE      *BLANK
053600090323     C                   MOVEL     KCC200        D1KCC
053700090323     C                   ENDIF
053800090323     C*
053900090324     C* Ricerca sottoconto
054000090323     C     H1NMFL        WHENEQ    'D1KSC'
054100090323     C                   RESET                   A201DS
054200090323     C                   MOVE      XSCSOC        SOC201
054300090323     C                   MOVE      D1KCC         KCC201
054400090323     C                   MOVE      D1KsC         KSC201
054500090323     C                   MOVEL     A201DS        KPJBU
054600090323     C                   CALL      'ANA201R'
054700090323     C                   PARM                    KPJBA
054800090323     C                   MOVEL     KPJBU         A201DS
054900090323     C*
055000090323     C     KSC201        IFNE      *BLANK
055100090323     C                   MOVEL     KCC201        D1KCC
055200090323     C                   MOVEL     KSC201        D1KSC
055300090323     C                   ENDIF
055400100402     C*  Attributo
055500100402     C     H1NMFL        WHENEQ    'D1ATTRIB'
055600100402     C                   RESET                   XTABDS
055700100402     C                   MOVE      '01'          XTAOPZ
055800100402     C                   MOVE      XSCSOC        XTAAZI
055900100402     C                   MOVE      XSCLIN        XTALIN
056000100402     C                   Z-ADD     CURR          XTARIG
056100100402     C                   Z-ADD     CURC          XTACOL
056200100402     c                   move      posattr057    xtapos
056300100402     C                   CLEAR                   KPJBU
056400100402     C                   MOVEL     XTABDS        KPJBU
056500100402     C                   CALL      'ANG0A1R'
056600100402     C                   PARM                    KPJBA
056700100402     C                   MOVEL     KPJBU         XTABDS
056800100402     C     XTAret        IFNE      '2'
056900100402     C                   MOVE      XTAKEY        D1Attrib
057000100402     C                   ENDIF
057100100402     C* - Agente
057200100402     C     H1NMFL        WHENEQ    'D1KSCAGE'
057300100402     C                   RESET                   A400DS
057400100402     C                   MOVEL     '01'          OPZ400
057500100402     C                   MOVE      '3'           AGE400
057600100402     C                   MOVEL     A400DS        KPJBU
057700100402     C                   CALL      'ANA400R'
057800100402     C                   PARM                    KPJBA
057900100402     C                   MOVEL     KPJBU         A400DS
058000100402     C*
058100100402     C     KSC400        IFNE      *BLANK
058200100402     C                   MOVEL     KSC400        D1KSCAGE
058300100402     C                   ENDIF
058400090324     C*
058500090324     C* Ricerca destinatario
058600090324     C     H1NMFL        WHENEQ    'D1DESTIN '
058700090324    >C                   RESET                   ANA001DS
058800090324    >C                   MOVEL     '01'          OPZ001
058900090324    >C                   MOVEL     ANA001DS      KPJBU
059000090324    >C                   CALL      'ANA001R'
059100090324     C                   PARM                    KPJBA
059200090324    >C                   MOVEL     KPJBU         ANA001DS
059300090324     C     RET001        IFNE      '2'
059400090324     c                   eval      D1Destin = Prf001
059500090324     C                   ENDIF
059600100407     C*  Azione
059700100407     C     H1NMFL        WHENEQ    'D1AZIONE'
059800100407     C                   RESET                   XTABDS
059900100407     C                   MOVE      '01'          XTAOPZ
060000100407     C                   MOVE      XSCSOC        XTAAZI
060100100407     C                   MOVE      XSCLIN        XTALIN
060200100407     C                   Z-ADD     CURR          XTARIG
060300100407     C                   Z-ADD     CURC          XTACOL
060400100407     C                   CLEAR                   KPJBU
060500100407     C                   MOVEL     XTABDS        KPJBU
060600100407     C                   CALL      'ANG047R'
060700100407     C                   PARM                    KPJBA
060800100407     C                   MOVEL     KPJBU         XTABDS
060900100407     C     XTAret        IFNE      '2'
061000100407     C                   MOVE      XTAKEY        D1AZIONE
061100100407     C                   ENDIF
061200090324     c*
061300090324     C* Ordinamento
061400090324     C     H1NMFL        WHENEQ    'D2ORD'
061500090324     C                   RESET                   ANA964DS
061600090324     C                   MOVE      '01'          OPZ964
061700090324     C                   MOVEL     'ANDIZ000'    TRC964
061800100401     C                   MOVEL     'DSPORDRCA'   CAM964
061900090324     C                   MOVEL     *oFF          ALL964
062000090324     C                   CLEAR                   KPJBU
062100090324     C                   MOVEL     ANA964DS      KPJBU
062200090324     C                   CALL      'ANA964R'
062300090324     C                   PARM                    KPJBA
062400090324     C                   MOVEL     KPJBU         ANA964DS
062500090324     C     Ret964        IFNE      '2'
062600090324     C                   MOVEL     Val964        D2ORD
062700090324     C                   MOVEL     DES964        D2ORDD
062800090324     C                   ENDIF
062900130912C2226C* Raggruppamento
063000130912  "  C     H1NMFL        WHENEQ    'D2RGRPRE'
063100130912  "  C                   RESET                   ANA964DS
063200130912  "  C                   MOVE      '01'          OPZ964
063300130912  "  C                   MOVEL     'ANDIZ000'    TRC964
063400130912  "  C                   MOVEL     'DSPRGRSCL'   CAM964
063500130912  "  C                   MOVEL     *oFF          ALL964
063600130912  "  C                   CLEAR                   KPJBU
063700130912  "  C                   MOVEL     ANA964DS      KPJBU
063800130912  "  C                   CALL      'ANA964R'
063900130912  "  C                   PARM                    KPJBA
064000130912  "  C                   MOVEL     KPJBU         ANA964DS
064100130912  "  C     Ret964        IFNE      '2'
064200130912  "  C                   MOVEL     Val964        D2RGRPRE
064300130912  "  C                   MOVEL     DES964        D2RGRPRED
064400130912C2226C                   ENDIF
064500090323     C*
064600940223     C* ricerca non abilitata in questa posizione
064700941118   X1C                   OTHER
064800940929     C                   SETON                                        9899
064900940127     C*
065000941118   E1C                   ENDSL
065100950612     C*
065200950612     C* imposto pos. del cursore
065300950612     C                   Z-ADD     CURR          H1RIGA
065400950612     C                   Z-ADD     CURC          H1COLO
065500940506     C                   MOVE      *HIVAL        $LASTV
065600950612     C*
065700940127     C                   ENDSR
065800940131     C/EJECT
065900940224     C************************************************************
066000940131     C* RIEMPIMENTO OPZIONI
066100940224     C************************************************************
066200940131     C     RIEOPZ        BEGSR
066300940223     C*
066400940223     C* Input :
066500940223     C* - $TX = schiera con tutti i testi disponibili
066600940223     C* - $FL = schiere con i flag di validità associati ai testi
066700940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
066800940223     C*          opzione usato
066900940223     C* - WMXO = variabile contenente il numero massimo di testi
067000940223     C*          opzione usabili
067100940721     C* - $L01 = Lunghezza prima riga
067200940721     C* - $L02 = Lunghezza seconda riga
067300940223     C* Output :
067400940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
067500940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
067600941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
067700940223     C*
067800950522     C                   CALLB     'X04OPMSG'
067900940223     C* input
068000940223     C                   PARM                    $TX
068100940223     C                   PARM                    $FL
068200940225     C                   PARM                    WCOZ              3 0
068300940223     C                   PARM                    WMXO              3 0
068400940223     C* output
068500940223     C                   PARM                    WOPZ1            76
068600940223     C                   PARM                    WOPZ2            76
068700941214     C                   PARM                    FF23              1
068800950301     C* input
068900950726     C                   PARM                    $L01              3 0
069000950726     C                   PARM                    $L02              3 0
069100940223     C*
069200060224     C                   PARM                    $ALLOPT         960
069300060224     C*
069400940131     C                   ENDSR
069500940506     C/EJECT
069600940506     C************************************************************
069700940506     C* RIEMPIMENTO TASTI FUNZIONE
069800940506     C************************************************************
069900940506     C     RIEKEY        BEGSR
070000940506     C*
070100940506     C* Input :
070200940506     C* - $FX = schiera con tutti i testi disponibili
070300940506     C* - $FC = schiere con i flag di validità associati ai testi
070400940506     C* - $FR = schiere con i flag di REVERSE IMAGE
070500940506     C* - $ULK = ultimo testo caricato
070600940721     C* - $L01 = Lunghezza prima riga
070700940721     C* - $L02 = Lunghezza seconda riga
070800940506     C* Output :
070900940506     C* - $ULK
071000940609     C* - $KEY1= 1° riga di tasti funzione
071100940609     C* - $KEY2= 2° riga di tasti funzione
071200940506     C*
071300950522     C                   CALLB     'XKEYMSG'
071400940506     C* input
071500940506     C                   PARM                    $FX
071600940506     C                   PARM                    $FC
071700940506     C                   PARM                    $FR
071800940506     C                   PARM                    $ULK              3 0
071900940506     C* output
072000940506     C                   PARM                    $KEY1            79
072100940506     C                   PARM                    $KEY2            79
072200950301     C* input
072300950726     C                   PARM                    $L01              3 0
072400950726     C                   PARM                    $L02              3 0
072500940506     C*
072600060224     C                   PARM                    $ALLFUNCT       480
072700060224     C*
072800940506     C                   ENDSR
072900940224     C/EJECT
073000940224     C************************************************************
073100940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
073200940224     C************************************************************
073300940224     C     GESPIE        BEGSR
073400940224     C*
073500940224     C                   SELECT
073600940224     C* Parzializzazioni
073700940224     C     $GEST         WHENEQ    'D1'
073800940506     C     $ULKD1        IFEQ      0
073900940506     C* riempimento opzioni
074000940506     C                   EXSR      F24D1
074100940506     C                   ELSE
074200940506     C                   WRITE     FMTZ1
074300940506     C                   ENDIF
074400130911C2226C* PreLista
074500130911  "  C     $GEST         WHENEQ    'S2'
074600130911  "  C     $ULKS2        IFEQ      0
074700130911  "  C                   EXSR      F24S2
074800130911  "  C                   ELSE
074900130911  "  C                   WRITE     FMTZ2
075000130911C2226C                   ENDIF
075100940506     C* Lista
075200940506     C     $GEST         WHENEQ    'S1'
075300940506     C     $ULKS1        IFEQ      0
075400940506     C                   EXSR      F24S1
075500940506     C                   ELSE
075600940506     C                   WRITE     FMTZ2
075700940506     C                   ENDIF
075800940916     C* Modifica valori assunti
075900940916     C     $GEST         WHENEQ    'D2'
076000940916     C     $ULKD2        IFEQ      0
076100940916     C                   EXSR      F24D2
076200940916     C                   ELSE
076300940916     C                   WRITE     FMTZ3
076400940916     C                   ENDIF
076500940506     C                   ENDSL
076600940506     C*
076700940224     C                   ENDSR
076800940224     C/EJECT
076900940224     C************************************************************
077000940224     C* GESTIONE PARZIALIZZAZIONI
077100940224     C************************************************************
077200940224     C     GESD1         BEGSR
077300940224     C* inizializzazione videata
077400940223     C     $INZD1        IFEQ      *ON
077500940127     C                   EXSR      INZD1
077600940223     C                   MOVE      *OFF          $INZD1
077700941027     C                   ENDIF
077800940223     C* emissione piede videata se proveniente da altra
077900940223     C* salvataggio valore formato di provenienza
078000940202     C     $LASTV        IFNE      'D1'
078100940223     C* salvo stato F17 per sapere cosa fare in caso di F12
078200940223     C                   MOVE      *INKR         WINKR
078300940202     C                   WRITE     FMTT1
078400940224     C                   EXSR      GESPIE
078500940202     C                   MOVE      $LASTV        $LASTG
078600940202     C                   MOVE      'D1'          $LASTV
078700940707     C                   ENDIF
078800940707     C*
078900940707     C* se esistono errori sulla videata
079000940707     C* emetto la write del formato a indicatori spenti per vedere
079100940707     C* le eventuali decodifiche
079200940707     C     *IN99         IFEQ      *ON
079300940707     C                   MOVEA     *IN           WIN
079400940707     C                   MOVE      *ALL'0'       IN5098           49
079500940707     C                   MOVEA     IN5098        *IN(50)
079600940707     C                   WRITE     FMTD1
079700940707     C                   MOVEA     WIN           *IN
079800940707     C                   ENDIF
079900940131     C*
080000940131     C                   EXFMT     FMTD1
080100960605     C*
080200960605     C* azzero la posizione del cursore
080300960605     C                   Z-ADD     0             H1RIGA
080400960605     C                   Z-ADD     0             H1COLO
080500940506     C*
080600951005     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
080700951005     C* INPUT
080800951005     C* $FC1 = Abilitazioni videata D1
080900951005     C* OUTPUT
081000951005     C* *IN97 + *IN99 Errori
081100951005     C                   MOVEA     $FC1          $FC
081200951005     C                   EXSR      CTRKEY
081300940506     C*
081400940506     C     *IN99         IFEQ      *OFF
081500940131     C*
081600940117     C                   SELECT
081700940916     C* F2=Decodifica
081800940916     C     $TASTO        WHENEQ    F02
081900940916     C                   EXSR      F02D1
082000940131     C* F3=Fine
082100940506     C     $TASTO        WHENEQ    F03
082200940307     C                   EXSR      F03D1
082300940131     C* F4=Ricerca
082400940506     C     $TASTO        WHENEQ    F04
082500940127     C                   EXSR      SEARCH
082600940916     C* F5=Refresh
082700940916     C     $TASTO        WHENEQ    F05
082800940916     C                   EXSR      F05D1
082900941206     C* F8=Successivo
083000941206     C     $TASTO        WHENEQ    F08
083100941206     C                   EXSR      F08D1
083200940131     C* F12=Ritorno
083300940506     C     $TASTO        WHENEQ    F12
083400940307     C                   EXSR      F12D1
083500940131     C* F18=Modifica valori assunti
083600940506     C     $TASTO        WHENEQ    F18
083700940307     C                   EXSR      F18D1
083800940224     C* F24=Altri tasti
083900940506     C     $TASTO        WHENEQ    F24
084000940309     C                   EXSR      F24D1
084100940131     C*
084200940117     C                   OTHER
084300940127     C                   EXSR      CTRD1
084400940127     C     *IN99         IFEQ      *OFF
084500940131     C* definisco l'ordinamento
084600950410     C                   EXSR      DEFORD
084700950410     C* richiamo la lista
084800130912C2226C                   If        $Pre  = *on
084900130911  "  C                   MOVE      'S2'          $GEST
085000130911  "  C                   MOVE      *ON           $INZS2
085100130911  "  C                   MOVE      *BLANK        W2POS
085200130911C2226C                   Else
085300940127     C                   MOVE      'S1'          $GEST
085400940223     C                   MOVE      *ON           $INZS1
085500940302     C                   MOVE      *BLANK        W1POS
085600130911C2226C                   ENDIF
085700940211     C                   ENDIF
085800940224     C*
085900940117     C                   ENDSL
086000940224     C*
086100940506     C                   ENDIF
086200950525     C*
086300940117     C                   ENDSR
086400940224     C/EJECT
086500940224     C************************************************************
086600940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
086700940224     C************************************************************
086800940127     C     INZD1         BEGSR
086900940318     C*
087000940318     C                   CLEAR                   FMTD1
087100100512     c                   move      xscon         d1on1
087200100512     c                   move      xscoff        d1off1
087300100413     c                   move      xscon         d1on2
087400100413     c                   move      xscoff        d1off2
087500131001BRT  c                   move      xscon         d1on3
087600131001BRT  c                   move      xscoff        d1off3
087700100413     c                   move      xscon         d1stato
087800100512     c                   move      xscon         d1FasiApe
087900131001BRT  c                   move      xscon         d1EsclImp
088000090324     c*
088100130905C2226X**Impostazione indicatore 11 per gestione campo unità nei solleciti
088200130905C2226X****               if        Gesuni057='D'
088300130905C2226C* Impostazione indicatore 11 per gestione campo unità recupero cred.
088400130905C2226C                   if        GesuniR057='D'
088500090324     C                   eval      *in11=*on
088600130910C2226C                   exsr      CarUniAbi
088700090324     C                   else
088800090324     C                   eval      *in11=*off
088900090324     C                   endif
089000090409      * Impostazione indicatore 17 per gestione campo attributo contabile
089100090409     c                   if        posattr057 <> *blanks
089200090409     C                   eval      *in17=*on
089300090409     C                   else
089400090409     C                   eval      *in17=*off
089500090409     C                   endif
089600941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
089700941013     C*    parzializzazioni
089800941104     C                   SELECT
089900941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
090000950606     C     $Unique       WHENEQ    *ON
090100070524    >C****               MOVE      UQ1001        D1KCC
090200941104     C* il pgm è richiamato e
090300941104     C* sono stati passati dei valori di parzializzazione ma non
090400941104     C* la chiave univoca
090500090323    >C     OPZL76        WHENNE    *BLANK
090600950403     C* imposto i valori passati sui campi a video
090700090323    >C                   MOVE      UnitaL76      D1Unita
090800090323    >C                   MOVE      KccL76        D1kcc
090900090323    >C                   MOVE      KScL76        D1Ksc
091000100401     c                   move      AttribL76     D1Attrib
091100100401     c                   move      KscAgeL76     D1kscAge
091200100420     c                   if        DtParDaL76 <> *loval
091300100420     c     *jobrun       move      DtParDaL76    D1DtParDA
091400100420     c                   endif
091500100420     c                   if        DtParAL76  <> *loval
091600100420     c     *jobrun       move      DtParAL76     D1DtParA
091700100420     c                   endif
091800100401     c                   if        DtScadDL76 <> *loval
091900100401     c     *jobrun       move      DtScadDL76    D1DtScaDA
092000100401     c                   endif
092100100401     c                   if        DtScadAL76 <> *loval
092200100401     c     *jobrun       move      DtScadAL76    D1DtScaA
092300100401     c                   endif
092400100416     c                   if        DtSolDaL76 <> *loval
092500100416     c     *jobrun       move      DtSolDaL76    D1DtSolDA
092600100416     c                   endif
092700100416     c                   if        DtSolAL76  <> *loval
092800100416     c     *jobrun       move      DtSolAL76     D1DtSolA
092900100416     c                   endif
093000100401     c                   move      FaseDAL76     D1FaseDa
093100100401     c                   move      FaseAL76      D1FaseA
093200090323     c                   move      DestinL76     d1Destin
093300100401     c                   if        DtAttDaL76 <> *loval
093400100401     c     *jobrun       move      DtAttDaL76    D1dtAttDa
093500090323     c                   endif
093600100401     c                   if        DtAttAl76  <> *loval
093700100401     c     *jobrun       move      DtAttAL76     D1dtAttA
093800090323     c                   endif
093900941104     C* il pgm non è richiamato
094000090323    >C     OPZL76        WHENEQ    *BLANK
094100950403     C* imposto dei valori di default
094200100401     c                   move      '9'           D1FaseA
094300131018     c                   move      udate         D1dtAttA
094400941115     C                   ENDSL
094500090409     c*
094600090409     C* decodifica posizione attributo
094700090409     c                   if        PosAttr057 <> *blanks
094800090409     C                   MOVE      *ALL'0'       XTBKEY
094900090409    >C                   MOVE      PosAttr057    XTBKEY
095000090409     C                   MOVE      '0'           XTBALC
095100090409     C                   MOVE      '1'           XTBRIC
095200090409     C                   MOVE      '0A2'         XTBCOD
095300090409     C                   CALLB     'XATB'
095400090409     C                   PARM                    XATBDS
095500090409     C     XTBERR        IFEQ      '0'
095600090409     C                   MOVEL     XTBUNI        Ang0a2ds
095700090409     C     DES0A2        CAT       Puntini:1     D1posattrD
095800090409     c                   move      ':'           D1PosAttrD
095900090409     c                   endif
096000090409     c                   endif
096100940224     C*
096200940117     C                   ENDSR
096300940916     C/EJECT
096400940916     C************************************************************
096500940916     C* GESTIONE F02 VIDEO D1
096600940916     C************************************************************
096700940916     C     F02D1         BEGSR
096800940916     C*
096900940916     C* per decodificare la videata richiamo la routine dei ctrl
097000940916     C* e poi disattivo gli eventuali messaggi di errore
097100940916     C                   EXSR      CTRD1
097200940916     C*
097300940916     C                   MOVE      *ALL'0'       IN5098
097400940916     C                   MOVEA     IN5098        *IN(50)
097500940916     C*
097600940916     C                   ENDSR
097700940224     C/EJECT
097800940131     C************************************************************
097900940307     C* GESTIONE F03 VIDEO D1
098000940131     C************************************************************
098100940307     C     F03D1         BEGSR
098200940131     C*
098300940307     C                   MOVE      *ON           $FINE
098400090323    >C                   MOVE      '1'           RETL76
098500940325     C* fine programma
098600940325     C                   EXSR      ENDPGM
098700940307     C*
098800940307     C                   ENDSR
098900940916     C/EJECT
099000940916     C************************************************************
099100940916     C* GESTIONE F05 VIDEO D1
099200940916     C************************************************************
099300940916     C     F05D1         BEGSR
099400940916     C*
099500940916     C                   MOVE      *ON           $INZD1
099600940916     C*
099700940916     C                   ENDSR
099800941206     C************************************************************
099900941206     C* GESTIONE F08 VIDEO D1
100000941206     C************************************************************
100100941206     C     F08D1         BEGSR
100200941206     C*
100300941206     C                   MOVE      *ON           $FINE
100400090323    >C                   MOVE      '0'           RETL76
100500941206     C*
100600941206     C                   ENDSR
100700950301     C/EJECT
100800940307     C************************************************************
100900940307     C* GESTIONE F12 VIDEO D1
101000940307     C************************************************************
101100940307     C     F12D1         BEGSR
101200940307     C*
101300940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
101400940307     C*  per F12 torno alla lista
101500940307     C     $LOP          IFEQ      2
101600940307     C     WINKR         OREQ      *ON
101700130923C2226X**                 MOVE      'S1'          $GEST
101800130923C2226C                   MOVE      $LastG        $GEST
101900940307     C*  altrimenti, per F12 torno a menu
102000940307     C                   ELSE
102100940307     C                   MOVE      *ON           $FINE
102200090323    >C                   MOVE      '2'           RETL76
102300941027     C                   ENDIF
102400940307     C*
102500940307     C                   ENDSR
102600940307     C/EJECT
102700940307     C************************************************************
102800940307     C* GESTIONE F18 VIDEO D1
102900940307     C************************************************************
103000940307     C     F18D1         BEGSR
103100940307     C*
103200940307     C                   MOVE      'D2'          $GEST
103300940307     C                   MOVEL     *ON           $INZD2
103400940307     C*
103500940307     C                   ENDSR
103600940307     C/EJECT
103700940307     C************************************************************
103800940307     C* GESTIONE F24 VIDEO D1
103900940609     C* input
104000940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
104100940509     C* $FX1 =Testi tasti funzione
104200940509     C* $FC1 = Abilitazioni
104300940509     C* $FR1 = Inversioni di fondo
104400940307     C************************************************************
104500940307     C     F24D1         BEGSR
104600940506     C*
104700940506     C                   MOVEA     $FX1          $FX
104800940506     C                   MOVEA     $FC1          $FC
104900940506     C                   MOVEA     $FR1          $FR
105000940506     C                   Z-ADD     $ULKD1        $ULK
105100950726     C                   Z-ADD     $D1L01        $L01
105200950726     C                   Z-ADD     $D1L02        $L02
105300940506     C                   EXSR      RIEKEY
105400940506     C                   Z-ADD     $ULK          $ULKD1            3 0
105500940720     C                   MOVEA     $FC           $FC1
105600070618     C                   MOVEL     $KEY1         Z1KE
105700060224     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
105800940506     C                   MOVEL     $KEY2         Z1KE2
105900070618     c* riga unica tasti
106000070618     c                   eval      $A = %triml(z1ke)
106100070618     c                   eval      $B = %triml(z1ke2)
106200070618     c     $A            cat(P)    $B:0          z1ke1
106300940506     C                   WRITE     FMTZ1
106400940307     C*
106500940307     C                   ENDSR
106600940307     C/EJECT
106700940307     C************************************************************
106800940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
106900950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
107000960109     C* 93 94 96 97 98
107100940307     C************************************************************
107200940307     C     CTRD1         BEGSR
107300940307     C*
107400940127     C                   SETOFF                                       99
107500070525     C* non imposto pos. del cursore
107600070525     C                   Z-ADD     0             H1RIGA
107700070525     C                   Z-ADD     0             H1COLO
107800940131     C*
107900090324     C* Unità
108000090324     C                   exsr      CtrUnita
108100090324     C* Conto
108200090324     C                   exsr      CtrConto
108300100402     C* Attributo contabile
108400100402     C                   exsr      CtrAttr
108500100402     C* Agente
108600100402     C                   exsr      CtrkscAge
108700100420     C* Date partita
108800100420     C                   exsr      CtrDatePar
108900100402     C* Date scadenza
109000100402     C                   exsr      CtrDateSca
109100100416     C* Date sollecito
109200100416     C                   exsr      CtrDateSol
109300100402     C* Fasi
109400100402     C                   exsr      CtrFasi
109500100407     C* Azione
109600100407     C                   exsr      CtrAzione
109700090324     C* Destinatario
109800090324     C                   exsr      CtrDestin
109900100402     C* Date attività
110000100402     C                   exsr      CtrDateAtt
110100100512     C* fasi aperte
110200100512     c                   if        D1FasiApe <> xscon and
110300100512     c                             D1FasiApe <> xscoff
110400100512     c                   seton                                        6199
110500100512     c                   endif
110600100413     C* partite aperte
110700100413     c                   if        D1Stato <> xscon and
110800100413     c                             D1Stato <> xscoff
110900100413     c                   seton                                        5799
111000100413     c                   endif
111100131001BRT  C* esclusione impegni (98)
111200131001 "   c                   if        D1EsclImp <> xscon and
111300131001 "   c                             D1EsclImp <> xscoff
111400131001 "   c                   seton                                        5899
111500131001BRT  c                   endif
111600070531     c*
111700940117     C                   ENDSR
111800090324     C*----------------------------------------------------------
111900090324     C* Controllo unita
112000090324     C*----------------------------------------------------------
112100090324     C     CtrUnita      BEGSR
112200090324     C*
112300090324     C                   CLEAR                   D1UnitaD
112400090324     C     D1Unita       IFNE      *BLANKS
112500090324     C     *in11         ANDEQ     *on
112600090324     C                   MOVE      xscsoc        UNISOCieta
112700090324     C                   MOVE      D1Unita       UNIUNIta
112800090324     C     K02Uni01      CHAIN     ANUNI01L                           21
112900090324     C     *IN21         IFEQ      *ON
113000090324     C                   SETON                                        6299
113100090324     C                   ELSE
113200090324     C                   MOVEL     UNIDESbrev    D1UnitaD
113300090324     C                   ENDIF
113400130909C2226C* Controllo autorizzazione all'unità
113500130909  "  C     *in62         Ifeq      *off
113600130909  "  C                   RESET                   XAutUniDS
113700130909  "  C                   EVAL      XAUPrf = KNmUs
113800130909  "  C                   EVAL      XAUSoc = XscSoc
113900130909  "  C                   EVAL      XAUUni = d1unita
114000130909  "  C                   CALL      'XAUTUNI'
114100130909  "  C                   PARM                    XAutUniDS
114200130909  "  C     XAUAbl        IFEQ      *OFF
114300130909  "  C                   SETON                                        7599
114400130909  "  C                   ENDIF
114500130909C2226C                   endif
114600090324     C                   ENDIF
114700090324     c*
114800090324     C                   ENDSR
114900090324     C*----------------------------------------------------------
115000090324     C* Controllo conto
115100090324     C*----------------------------------------------------------
115200090324     C     CtrConto      BEGSR
115300090324     C*
115400090324     C* Controllo del conto (niente obbligatorio)
115500090324     C                   CLEAR                   D1Descr
115600090324 B1  c                   if        D1Kcc <> *blanks or
115700090324     c                             D1Ksc <> *blanks
115800090324     c*
115900090324     c                   if        d1kcc=*blanks
116000090324     c                   eval      d1kcc='C'
116100090324     c                   endif
116200090324     C     D1kcc         IFNE      'C'
116300090324     C     D1kcc         ANDNE     'F'
116400090324     C                   CALL      'XALLINEA'
116500090324     C                   PARM                    D1kcc
116600090324     C                   PARM      6             LEN               5 0
116700090324     C                   PARM                    XSCINP
116800090324     C                   ENDIF
116900090324     C*
117000090324     C                   CALL      'XALLINEA'
117100090324     C                   PARM                    D1KSC
117200090324     C                   PARM      8             LEN               5 0
117300090324     C                   PARM                    XSCINP
117400090324     C*
117500090324     C                   MOVE      D1KCC         $kcc
117600090324     C                   MOVE      D1KSC         $ksc
117700090324     C*
117800090324     C* reperisco capoconto se cliente/fornitore
117900090324     C     D1KCC         IFEQ      'C       '
118000090324     C     D1KCC         OREQ      'F       '
118100090324     C                   MOVE      XSCSOC        XCAPSOCIET        3
118200090324     C                   MOVE      D1KCC         XCAPKCC           6
118300090324     C                   MOVE      D1KSC         XCAPKSC           8
118400090324     C                   CALLB     'XCAPCLIFOR'
118500090324     C                   PARM                    XCAPSOCIET
118600090324     C                   PARM                    XCAPKCC
118700090324     C                   PARM                    XCAPKSC
118800090324     C                   MOVE      XCAPKCC       D1KCC
118900090324     C                   MOVE      XCAPKCC       $kcc
119000090324     C                   MOVE      XCAPKsC       $ksc
119100090324     C                   ENDIF
119200090324     C*
119300090324     C* Richiamo modulo controllo conto
119400090324     C                   EXSR      CALLMVC002
119500090324     C* Errore
119600090324     C     $Esito        IFEQ      *ON
119700090324     c                   if        d1ksc = *blanks
119800090324     C                   SETON                                        5099
119900090324     c                   else
120000090324     C                   SETON                                        505199
120100090324     c                   endif
120200090324     c*
120300090324     C                   ELSE
120400090324     C                   MOVEL     Descr002      D1Descr
120500100402     c* solo clienti
120600100402     c                   if        Snatura002 <> 'C'
120700100402     c                   if        d1ksc = *blanks
120800100402     C                   SETON                                        5399
120900100402     c                   else
121000100402     C                   SETON                                        535499
121100100402     c                   endif
121200100402     c                   endif
121300090324     C                   ENDIF
121400090324     C*
121500090324 e1  C                   ENDIF
121600090324     c*
121700090324     C                   ENDSR
121800070525     C*----------------------------------------------------------
121900070525     C* Chiamata NDMVC002
122000070525     C*----------------------------------------------------------
122100070525     C     CallMvc002    BEGSR
122200070525     C*
122300070525     C* Controllo conto
122400070525     C                   Clear                   ND002DS
122500070525     C                   EVAL      $LenOut = %Size(ND002DS)
122600070525     C                   CallB     'NDMVC002'
122700070525     C                   PARM                    xscsoc
122800070525     C                   PARM                    $kcc
122900070525     C                   PARM                    $ksc
123000100521     C                   PARM                    Wdate
123100070525     C                   PARM      *OFF          $GesMsg
123200070525     C                   PARM      *OFF          $Esito
123300070525     C                   PARM      'ND002DS'     $Struttura
123400070525     C                   PARM                    ND002DS
123500070525     C                   PARM                    $LenOut
123600070525     C*
123700070525     C                   ENDSR
123800100402     C*----------------------------------------------------------
123900100402     C* Controllo attributo contabile
124000100402     C*----------------------------------------------------------
124100100402     C     CtrAttr       BEGSR
124200100402     C*
124300100402     c* controllo nel control il codice attributo
124400100402     C                   clear                   d1attribd
124500100402     c                   if        D1attrib <> *blanks
124600100402     C                   MOVE      *ALL'0'       XTBKEY
124700100402     C                   MOVEL     PosAttr057    WRK6              6
124800100402     C                   MOVE      d1Attrib      WRK6
124900100402    >C                   MOVE      wrk6          XTBKEY
125000100402     C                   MOVE      '0'           XTBALC
125100100402     C                   MOVE      '1'           XTBRIC
125200100402     C                   MOVE      '0A1'         XTBCOD
125300100402     C                   CALLB     'XATB'
125400100402     C                   PARM                    XATBDS
125500100402     C     XTBERR        IFEQ      '0'
125600100402     C                   MOVEL     XTBUNI        Ang0a1ds
125700100402     C                   MOVEL     DES0a1        D1AttribD
125800100402     c                   else
125900100402     c                   seton                                        6899
126000100402     c                   endif
126100100402     c                   endif
126200100402     c*
126300100402     c                   ENDSR
126400100402     C/EJECT
126500100402     C*----------------------------------------------------------
126600100402     C* Controllo Agente
126700100402     C*----------------------------------------------------------
126800100402     C     CtrKscAge     BEGSR
126900100402     C*
127000100402     c                   clear                   d1kscageD
127100100402   1 C     D1kscage      IFNE      *blanks
127200100402     C                   CALLb     'XALLINEA'
127300100402     C                   PARM                    D1kscage
127400100402     C                   PARM      8             LEN               5 0
127500100402     C                   PARM                    XSCINP
127600100402     C* reperisco capoconto agente
127700100402     C                   EVAL      Xcapkcc  = 'F     '
127800100402     C                   MOVE      XSCSOC        XCAPSOCIET
127900100402     C                   MOVE      d1kscage      XCAPKSC
128000100402     C                   CALLB     'XCAPCLIFOR'
128100100402     C                   PARM                    XCAPSOCIET        3
128200100402     C                   PARM                    XCAPKCC           6
128300100402     C                   PARM                    XCAPKSC           8
128400100402     C* Richiamo modulo controllo conto
128500100402     c                   move      Xcapkcc       $kcc
128600100402     c                   move      Xcapksc       $ksc
128700100402     C                   EXSR      CALLMVC002
128800100402   2 C     $Esito        IFEQ      *ON
128900100402     C                   SETON                                        5599
129000100402   2xC                   ELSE
129100100402     C                   MOVEL     DESBREV002    D1kscageD
129200100402   2-C                   ENDIF
129300100402   1-C                   ENDIF
129400100402     C*
129500100402     C                   ENDSR
129600100402     c*
129700100416     c*EJECT
129800100420     C*----------------------------------------------------------
129900100420     C* Controllo DATE partita
130000100420     C*----------------------------------------------------------
130100100420     C     CtrDatePar    BEGSR
130200100420     C*
130300100420      * Data partita da a
130400100420     c                   Eval      WdtParDa= *Loval
130500100420 b1  C                   if        D1DtParDa<>   *Zeros
130600100420     C     *JobRun       Test(d)                 D1DtParDa              21
130700100420    >C     *In21         ifeq      *On
130800100420     c* non corretta
130900100420    >C                   SetOn                                        7299
131000100420     c                   else
131100100420     c* è corretta,
131200100420     c     *JobRun       move      D1DtParDa     WdtParDA
131300100420    >C                   endif
131400100420E1  >C                   endif
131500100420     C*
131600100420      * Data scadenza a
131700100420     c                   Eval      WdtParA = *hival
131800100420 b1  C                   if        D1DtParA  <>   *Zeros
131900100420     C     *JobRun       Test(d)                 D1DtParA               21
132000100420    >C     *In21         ifeq      *On
132100100420     c* non corretta
132200100420    >C                   SetOn                                        7399
132300100420     c                   else
132400100420     c* è corretta,
132500100420     c     *JobRun       move      D1DtParA      WdtParA
132600100420    >C                   endif
132700100420E1  >C                   endif
132800100420     c*
132900100420     c* limiti errati
133000100420     c                   if        WdtParDa > WdtParA
133100100420    >C                   SetOn                                        7499
133200100420     c                   endif
133300100420     c
133400100420     C                   ENDSR
133500100420     c*EJECT
133600100402     C*----------------------------------------------------------
133700100402     C* Controllo DATE scadenza
133800100402     C*----------------------------------------------------------
133900100402     C     CtrDateSca    BEGSR
134000100402     C*
134100100402      * Data scadenza  da
134200100402     c                   Eval      WdtScaDa= *Loval
134300100402 b1  C                   if        D1DtScaDa<>   *Zeros
134400100402     C     *JobRun       Test(d)                 D1DtScaDa              21
134500100402    >C     *In21         ifeq      *On
134600100402     c* non corretta
134700100402    >C                   SetOn                                        6999
134800100402     c                   else
134900100402     c* è corretta,
135000100402     c     *JobRun       move      D1DtScaDa     WdtScaDA
135100100402    >C                   endif
135200100402E1  >C                   endif
135300100402     C*
135400100402      * Data scadenza a
135500100402     c                   Eval      WdtScaA = *hival
135600100402 b1  C                   if        D1DtScaA  <>   *Zeros
135700100402     C     *JobRun       Test(d)                 D1DtScaA               21
135800100402    >C     *In21         ifeq      *On
135900100402     c* non corretta
136000100402    >C                   SetOn                                        7099
136100100402     c                   else
136200100402     c* è corretta,
136300100402     c     *JobRun       move      D1DtScaA      WdtScaA
136400100402    >C                   endif
136500100402E1  >C                   endif
136600100402     c*
136700100402     c* limiti errati
136800100402     c                   if        WdtScaDa > WdtScaA
136900100402    >C                   SetOn                                        7199
137000100402     c                   endif
137100100402     c
137200100402     C                   ENDSR
137300100416     c*EJECT
137400100416     C*----------------------------------------------------------
137500100416     C* Controllo DATE sollecito
137600100416     C*----------------------------------------------------------
137700100416     C     CtrDateSol    BEGSR
137800100416     C*
137900100416      * Data sollecito da
138000100416     c                   Eval      WdtSolDa= *Loval
138100100416 b1  C                   if        D1DtSolDa<>   *Zeros
138200100416     C     *JobRun       Test(d)                 D1DtSolDa              21
138300100416    >C     *In21         ifeq      *On
138400100416     c* non corretta
138500100416    >C                   SetOn                                        5899
138600100416     c                   else
138700100416     c* è corretta,
138800100416     c     *JobRun       move      D1DtSolDa     WdtSolDA
138900100416    >C                   endif
139000100416E1  >C                   endif
139100100416     C*
139200100416      * Data sollecito a
139300100416     c                   Eval      WdtSolA = *hival
139400100416 b1  C                   if        D1DtSolA  <>   *Zeros
139500100416     C     *JobRun       Test(d)                 D1DtSolA               21
139600100416    >C     *In21         ifeq      *On
139700100416     c* non corretta
139800100416    >C                   SetOn                                        5999
139900100416     c                   else
140000100416     c* è corretta,
140100100416     c     *JobRun       move      D1DtSolA      WdtSolA
140200100416    >C                   endif
140300100416E1  >C                   endif
140400100416     c*
140500100416     c* limiti errati
140600100416     c                   if        WdtSolDa > WdtSolA
140700100416    >C                   SetOn                                        6099
140800100416     c                   endif
140900100416     c
141000100416     C                   ENDSR
141100100402     C*----------------------------------------------------------
141200100402     C* Controllo fasi
141300100402     C*----------------------------------------------------------
141400100402     C     CtrFasi       BEGSR
141500100402     C*
141600100402     c                   if        D1FaseDa > d1FaseA
141700100402     c                   seton                                        6799
141800100402     c                   endif
141900100402     c*
142000100402     C                   ENDSR
142100100407     C*----------------------------------------------------------
142200100407     C* Controllo azione
142300100407     C*----------------------------------------------------------
142400100407     C     CtrAzione     BEGSR
142500100407     C*
142600100407     c                   clear                   D1AzioneD
142700100407     c                   if        d1Azione <> *blanks
142800100407     C                   MOVE      *ALL'0'       XTBKEY
142900100407    >C                   MOVE      D1Azione      XTBKEY
143000100407     C                   MOVE      '0'           XTBALC
143100100407     C                   MOVE      '1'           XTBRIC
143200100407     C                   MOVE      '047'         XTBCOD
143300100407     C                   CALLB     'XATB'
143400100407     C                   PARM                    XATBDS
143500100407     C     XTBERR        IFEQ      '0'
143600100407     C                   MOVEL     XTBUNI        Ang047ds
143700100407     c                   movel     Des047        D1AzioneD
143800100407     c                   else
143900100407     c                   seton                                        5699
144000100407     c                   endif
144100100407     c                   endif
144200100407     c*
144300100407     C                   ENDSR
144400090324     C*----------------------------------------------------------
144500090324     C* Controllo destinatario
144600090324     C*----------------------------------------------------------
144700090324     C     CtrDestin     BEGSR
144800090324     C*
144900090324      * Controllo codice destinatario
145000090324     c                   if        d1destin  <> *blanks
145100090324     C                   CLEAR                   AUTDS
145200090324     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
145300090324     C                   Z-ADD     0             XCAGRP                         GRUPPO
145400090324     C                   MOVEL     d1Destin      XCAPRF                         PROFILO
145500090324    >C                   MOVE      'CU'          XCATCK                         TIPO CTRL
145600090324    >C                   MOVE      ' '           XCANAU                         RICH.XNOAUT
145700090324     C                   EVAL      Lungutn= %Size(AnUtn00f)
145800090324     C                   CALLB     'XCHKAUT'
145900090324     C                   PARM                    AUTDS
146000090324     C                   PARM                    PARMOK
146100090324     C                   PARM                    ANUTNParm
146200090324     C                   PARM      71            LungUTN
146300090324b1   C     XCARTN        IFne      0
146400090324     c                   seton                                        6599
146500090324     C                   ENDIF
146600090324     c                   endif
146700090324     C*
146800090324     C                   ENDSR
146900090324     C*----------------------------------------------------------
147000100402     C* Controllo data attività
147100090324     C*----------------------------------------------------------
147200100402     C     CtrDateAtt    BEGSR
147300090324     C*
147400100402      * Data attività da
147500100401     c                   Eval      WdtAttda= *Loval
147600100401 b1  C                   if        D1dtAttDa<>   *Zeros
147700100401     C     *JobRun       Test(d)                 D1dtAttDa              21
147800090324    >C     *In21         ifeq      *On
147900090324     c* non corretta
148000090324    >C                   SetOn                                        5299
148100090324     c                   else
148200090324     c* è corretta,
148300100401     c     *JobRun       move      D1dtAttDA     WdtAttDa
148400090324    >C                   endif
148500090324E1  >C                   endif
148600090324     C*
148700100402      * Data attività a
148800100401     c                   Eval      WdtAttA = *hival
148900100401 b1  C                   if        D1dtAttA <>   *Zeros
149000100401     C     *JobRun       Test(d)                 D1dtAttA               21
149100090324    >C     *In21         ifeq      *On
149200090324     c* non corretta
149300090324    >C                   SetOn                                        6399
149400090324     c                   else
149500090324     c* è corretta,
149600100401     c     *JobRun       move      D1dtAttA      WdtAttA
149700090324    >C                   endif
149800090324E1  >C                   endif
149900090324     c*
150000090324     c* limiti errati
150100100401     c                   if        WdtAttDa > WdtAttA
150200090324    >C                   SetOn                                        6499
150300090324     c                   endif
150400090324     c*
150500090324     C                   ENDSR
150600090409     C*
150700950410     C************************************************************
150800950410     C* DEFINIZIONE DELL'ORDINAMENTO
150900950410     C************************************************************
151000950410     C     DEFORD        BEGSR
151100950410     C*
151200950410     C* definisco l'ordinamento della lista a seconda dei valori
151300950410     C* impostati nel video di parzializzazione
151400950410     C*
151500950410     C                   Z-ADD     1             $ORD
151600070524     C*
151700950410     C                   ENDSR
151800960202     C/EJECT
151900960202     C************************************************************
152000960202     C* Chiamata programma con tasto personalizzato
152100960202     C************************************************************
152200960202     C     FPerS1        BEGSR
152300960202     C*
152400960202     C* salvo la DS di ingresso
152500090323    >C                   MOVEL     NDCL76DS      $SaveDS
152600960202     C*
152700090323    >C                   MOVEL     NDCL76DS      KPJBU
152800960202     C                   CALL      $PGF2(PosFun)                        22
152900960202     C                   PARM                    KPJBA
153000960202     C                   PARM                    NOMPGM
153100960202     C*
153200960202     C* ripristino la DS di ingresso
153300090323    >C                   MOVEL     $SaveDS       NDCL76DS
153400960202     C*
153500960202     C                   ENDSR
153600130911C2226C/EJECT
153700130911  "  C************************************************************
153800130911  "  C* Chiamata programma con tasto personalizzato
153900130911  "  C************************************************************
154000130911C2226C     FPerS2        BEGSR
154100130911     C*
154200130911     C* salvo la DS di ingresso
154300130911    >C                   MOVEL     NDCL76DS      $SaveDS
154400130911     C*
154500130911    >C                   MOVEL     NDCL76DS      KPJBU
154600130911     C                   CALL      $PGF2(PosFun)                        22
154700130911     C                   PARM                    KPJBA
154800130911     C                   PARM                    NOMPGM
154900130911     C*
155000130911     C* ripristino la DS di ingresso
155100130911    >C                   MOVEL     $SaveDS       NDCL76DS
155200130911     C*
155300130911C2226C                   ENDSR
155400940224     C/EJECT
155500940127     C************************************************************
155600940131     C* GESTIONE LISTA
155700940127     C************************************************************
155800940127     C     GESS1         BEGSR
155900940223     C* inizializzazione videata
156000940223     C     $INZS1        IFEQ      *ON
156100940127     C                   EXSR      INZS1
156200940223     C                   MOVE      *OFF          $INZS1
156300940127     C                   ENDIF
156400940223     C* emissione piede videata se proveniente da altra
156500940223     C* salvataggio valore formato di provenienza
156600940202     C     $LASTV        IFNE      'S1'
156700940202     C                   WRITE     FMTT1
156800940224     C                   EXSR      GESPIE
156900940202     C                   MOVE      $LASTV        $LASTG
157000940202     C                   MOVE      'S1'          $LASTV
157100941027     C                   ENDIF
157200940223     C*
157300940223     C     WMAX          IFEQ      0
157400940211     C                   WRITE     FMTD3                                        NO RCD
157500940211     C                   ENDIF
157600960202     C* Inizializzazione indice tasti funzione personalizzati
157700960202     C                   Eval      PosFun = 25
157800940127     C*
157900940131     C                   EXFMT     FMTC1
158000960605     C*
158100960605     C* azzero la posizione del cursore
158200960605     C                   Z-ADD     0             H1RIGA
158300960605     C                   Z-ADD     0             H1COLO
158400970912     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
158500970912     C* di refresh dello sfondo
158600970912     C                   MOVE      '1'           WExf
158700960605     C*
158800940204     C     C1NRR         IFNE      0
158900940204     C                   Z-ADD     C1NRR         WSFL
159000940204     C                   ENDIF
159100951020     C                   IF        SFLNRR > 0
159200951020     C                   Z-ADD     SFLNRR        C1RCD
159300951020     C                   ENDIF
159400960122     C* inizializzazione indice opzioni personalizzate
159500960122     C                   Eval      PosOpz = 0
159600951005     C* CONTROLLO ABILITAZIONE TASTI
159700951005     C* INPUT
159800951005     C* $FC2 = Abilitazioni videata S1
159900951005     C* OUTPUT
160000951005     C* *IN97 + *IN99 Errori
160100951005     C                   MOVEA     $FC2          $FC
160200951005     C                   EXSR      CTRKEY
160300941205     C*
160400940506     C     *IN99         IFEQ      *OFF
160500940127     C*
160600941027     C                   SELECT
160700940127     C* F3=Fine
160800940506     C     $TASTO        WHENEQ    F03
160900940309     C                   EXSR      F03S1
161000130924C2226C* F5=Refresh
161100130924  "  C     $TASTO        WHENEQ    F05
161200130924C2226C                   EXSR      F05S1
161300941206     C* F8=Successivo
161400941206     C     $TASTO        WHENEQ    F08
161500941206     C                   EXSR      F08S1
161600940131     C* F12=Ritorno
161700940506     C     $TASTO        WHENEQ    F12
161800940309     C                   EXSR      F12S1
161900090325     C* F13=Selezione multipla
162000090325     C     $TASTO        WHENEQ    F13
162100130924C2226C                   Clear                   $tastoSav2
162200090325     C                   Eval      $TastoSav=$Tasto
162300130924C2226C                   Eval      PrimaVolta = 0
162400090325     C                   EXSR      F13S1
162500940131     C* F17=Parzializzazioni
162600940506     C     $TASTO        WHENEQ    F17
162700940309     C                   EXSR      F17S1
162800940131     C* F18=Modifica valori assunti
162900940506     C     $TASTO        WHENEQ    F18
163000940309     C                   EXSR      F18S1
163100940224     C* F23=Altre opzioni
163200940506     C     $TASTO        WHENEQ    F23
163300940309     C                   EXSR      F23S1
163400940224     C* F24=Altri tasti
163500940506     C     $TASTO        WHENEQ    F24
163600940309     C                   EXSR      F24S1
163700940127     C* ROLL-UP
163800090330     C     $TASTO        WHENEQ    ROLLUP
163900090330     C                   EXSR      ROLS1
164000960202     C* Personalizzata
164100960202     C     $PGF2(PosFun) Whenne    *BLANK
164200960202     C                   EXSR      FperS1
164300940127     C*
164400941027     C                   OTHER
164500940127     C* CONTROLLO DATI
164600940131     C                   EXSR      CTRC1
164700940201     C     *IN99         IFEQ      *OFF
164800940131     C                   EXSR      CTRS1
164900941027     C                   ENDIF
165000941205     C*
165100941027     C                   ENDSL
165200940127     C*
165300940506     C                   ENDIF
165400941205     C*
165500940127     C                   ENDSR
165600940224     C/EJECT
165700940127     C************************************************************
165800940131     C* INIZIALIZZAZIONE LISTA
165900940127     C************************************************************
166000940127     C     INZS1         BEGSR
166100090325     C*appoggio x F13
166200090325     C                   Clear                   $TastoSav
166300090325     C                   eval      PrimaVolta=0
166400130924     C                   Clear                   $TastoSav2
166500940302     C* pulizia SFL
166600940128     C                   SETOFF                                         3031
166700940131     C                   WRITE     FMTC1
166800940128     C                   SETON                                          31
166900940127     C*
167000940128     C* riempimento opzioni
167100940223     C                   MOVEA     $TX1          $TX
167200940223     C                   MOVEA     $FL1          $FL
167300940309     C                   Z-ADD     0             WCOZ
167400960117     C                   Z-ADD     0             WMXO
167500940225     C*  equivale al num.di elementi della schiera $TX1
167600960117     C                   Z-add     OpzLen        WMXO
167700950906     C                   Z-ADD     $C1Op1        $L01
167800950906     C                   Z-ADD     $C1Op2        $L02
167900940128     C                   EXSR      RIEOPZ
168000941214     C                   Z-ADD     23            X
168100941214     C                   MOVE      FF23          $FC2(X)
168200130911     C                   MOVEL     WOPZ1         C1OPZ
168300060224     C                   MOVEL     $ALLOPT       H1ALLOPT
168400940608     C                   MOVEL     WOPZ2         C1OPZ2
168500940225     C                   Z-ADD     WCOZ          C1COZ
168600070531     c* riga unica opzioni
168700070531     c                   eval      $A = %triml(c1opz)
168800070531     c                   eval      $B = %triml(c1opz2)
168900070531     c     $A            cat(P)    $B:2          C1opz1
169000100402     c*
169100100402     c                   eval      C1Unita   =D1Unita
169200100402     c                   eval      C1kcc     =D1kcc
169300100402     c                   eval      C1PosAttrD=D1PosAttrD
169400100402     c                   eval      C1attrib=D1Attrib
169500100402     c                   eval      C1attribD=D1AttribD
169600130911C2226c                   eval      C1Importo=0
169700130912  "  C                   If        $pre = *on
169800130911  "  c                   eval      C1Importo=S2Importo
169900130911C2226C                   Endif
170000940128     C*
170100940128     C* CARICAMENTO SFL A PAGINE
170200940128     C*
170300940201     C                   Z-ADD     0             S1NRR
170400940128     C                   Z-ADD     0             WMAX
170500940224     C*
170600940224     C* Posizionamento su file pilota
170700940302     C                   CLEAR                   C1POS
170800941214     C                   EXSR      SETANA
170900940302     C* ripristino i valori del posizionamento
171000940302     C                   MOVE      W1POS         C1POS
171100940131     C*
171200940201     C* Lettura file pilota
171300941214     C                   EXSR      REDANA
171400940131     C*
171500970318     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
171600970318     C                   IF        $LastV = 'D1'
171700970318     C                   Z-ADD     SFLPAG        WPAG
171800970318     C                   ENDIF
171900940127     C                   EXSR      ROLS1
172000970318     C* imposto il caricamento sfl del numero massimo di righe per pagina
172100970318     C                   Z-ADD     SFLPAG        WPAG
172200940127     C*
172300940127     C                   ENDSR
172400940127     C/EJECT
172500940127     C************************************************************
172600940131     C* CARICAMENTO PAGINA LISTA
172700940127     C************************************************************
172800940127     C     ROLS1         BEGSR
172900940127     C*
173000940128     C                   SETOFF                                       32
173100940223     C                   Z-ADD     0             Y
173200940127     C                   Z-ADD     WMAX          S1NRR
173300940127     C*
173400940127     C* Leggo dal file anagrafico per caricare la lista
173500940127     C*
173600950606     C     $EndFile      DOWEQ     *OFF
173700940201     C     Y             ANDLT     WPAG
173800090325     c     S1nrr         andlt     9999
173900940127     C*
174000940131     C                   EXSR      RIES1
174100940127     C*
174200940127     C                   ADD       1             S1NRR
174300940127     C                   ADD       1             Y
174400940127     C*
174500940131     C                   WRITE     FMTS1
174600940131     C*
174700941214     C                   EXSR      REDANA
174800940128     C*
174900941027     C                   ENDDO
175000940127     C*
175100940223     C                   Z-ADD     S1NRR         WMAX                 30
175200940127     C*
175300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
175400940127     C*
175500940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
175600940127     C                   MVR                     RESTO             3 0
175700940131     C     PAGINE        MULT      SFLPAG        C1RCD
175800941027     C     RESTO         IFGT      0
175900940127     C                   ADD       1             C1RCD
176000941027     C                   ELSE
176100940407     C                   SUB       SFLPAG        C1RCD
176200940407     C                   ADD       1             C1RCD
176300941027     C                   ENDIF
176400940128     C*
176500940127     C                   ENDSR
176600940224     C/EJECT
176700940224     C************************************************************
176800940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
176900940224     C************************************************************
177000941214     C     SETANA        BEGSR
177100940224     C*
177200070524     c                   exsr      InzSql
177300940224     C*
177400940224     C                   ENDSR
177500940128     C/EJECT
177600940128     C************************************************************
177700940131     C* LETTURA RCD ARCHIVIO PILOTA
177800940128     C************************************************************
177900941214     C     REDANA        BEGSR
178000940128     C*
178100950606     C                   MOVEL     *OFF          $EndFile
178200940131     C                   MOVEL     *OFF          $RCDOK
178300970910     C*
178400950606     C     $EndFile      DOUEQ     *ON
178500940131     C     $RCDOK        OREQ      *ON
178600070525     C*
178700070525     c                   exsr      FetchSql
178800070525     c*
178900070525     C                   if        SqlCod<0 or SqlCod=100
179000070525     C                   MOVE      *ON           *IN21
179100070525     C                   ELSE
179200070525     C                   MOVE      *OFF          *IN21
179300070525     C                   END
179400940131     C*
179500950606     C                   MOVE      *IN21         $EndFile
179600940131     C*
179700950606     C     $EndFile      IFEQ      *OFF
179800100407     C                   EXSR      SELANA
179900941027     C                   ENDIF
180000940131     C*
180100941027     C                   ENDDO
180200940131     C*
180300000208     C                   MOVE      $EndFile      *IN33                          SflEnd
180400940131     C*
180500940131     C                   ENDSR
180600940224     C/EJECT
180700130911C2226C************************************************************
180800130911  "  C* GESTIONE PRELISTA
180900130911  "  C************************************************************
181000130911C2226C     GESS2         BEGSR
181100130911     C* inizializzazione videata
181200130911     C     $INZS2        IFEQ      *ON
181300130911     C                   EXSR      INZS2
181400130911     C                   MOVE      *OFF          $INZS2
181500130911     C                   ENDIF
181600130911     C* emissione piede videata se proveniente da altra
181700130911     C* salvataggio valore formato di provenienza
181800130911     C     $LASTV        IFNE      'S2'
181900130911     C                   WRITE     FMTT1
182000130911     C                   EXSR      GESPIE
182100130911     C                   MOVE      $LASTV        $LASTG
182200130911     C                   MOVE      'S2'          $LASTV
182300130911     C                   ENDIF
182400130911     C*
182500130911     C     WMAXS2        IFEQ      0
182600130911     C                   WRITE     FMTD3                                        NO RCD
182700130911     C                   ENDIF
182800130911     C* Inizializzazione indice tasti funzione personalizzati
182900130911     C                   Eval      PosFun = 25
183000130911     C*
183100130911     C                   EXFMT     FMTC2
183200130911     C*
183300130911     C* azzero la posizione del cursore
183400130911     C                   Z-ADD     0             H1RIGA
183500130911     C                   Z-ADD     0             H1COLO
183600130911     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
183700130911     C* di refresh dello sfondo
183800130911     C                   MOVE      '1'           WExf
183900130911     C*
184000130911     C     C2NRR         IFNE      0
184100130911     C                   Z-ADD     C2NRR         WSFL2
184200130911     C                   ENDIF
184300130911     C                   IF        SFLNRR > 0
184400130911     C                   Z-ADD     SFLNRR        C2RCD
184500130911     C                   ENDIF
184600130911     C* inizializzazione indice opzioni personalizzate
184700130911     C                   Eval      PosOpz = 0
184800130911     C* CONTROLLO ABILITAZIONE TASTI
184900130911     C* INPUT
185000130911     C* $FC2 = Abilitazioni videata S1
185100130911     C* OUTPUT
185200130911     C* *IN97 + *IN99 Errori
185300130911     C                   MOVEA     $FC2          $FC
185400130911     C                   EXSR      CTRKEY
185500130911     C*
185600130911     C     *IN99         IFEQ      *OFF
185700130911     C*
185800130911     C                   SELECT
185900130911     C* F3=Fine
186000130911     C     $TASTO        WHENEQ    F03
186100130911     C                   EXSR      F03S2
186200130911     C* F8=Successivo
186300130911     C     $TASTO        WHENEQ    F08
186400130911     C                   EXSR      F08S2
186500130911     C* F12=Ritorno
186600130911     C     $TASTO        WHENEQ    F12
186700130911     C                   EXSR      F12S2
186800130911     C* F13=Selezione multipla
186900130911     C     $TASTO        WHENEQ    F13
187000130911     C                   EXSR      F13S2
187100130911     C* F17=Parzializzazioni
187200130911     C     $TASTO        WHENEQ    F17
187300130911     C                   EXSR      F17S2
187400130911     C* F18=Modifica valori assunti
187500130911     C     $TASTO        WHENEQ    F18
187600130911     C                   EXSR      F18S2
187700130911     C* F23=Altre opzioni
187800130911     C     $TASTO        WHENEQ    F23
187900130911     C                   EXSR      F23S2
188000130911     C* F24=Altri tasti
188100130911     C     $TASTO        WHENEQ    F24
188200130911     C                   EXSR      F24S2
188300130911     C* ROLL-UP
188400130911     C     $TASTO        WHENEQ    ROLLUP
188500130911     C                   EXSR      ROLS2
188600130911     C* Personalizzata
188700130911     C     $PGF2(PosFun) Whenne    *BLANK
188800130911     C                   EXSR      FperS2
188900130911     C*
189000130911     C                   OTHER
189100130911     C* CONTROLLO DATI
189200130911     C                   EXSR      CTRC2
189300130911     C     *IN99         IFEQ      *OFF
189400130911     C                   EXSR      CTRS2
189500130911     C                   ENDIF
189600130911     C*
189700130911     C                   ENDSL
189800130911     C*
189900130911     C                   ENDIF
190000130911     C*
190100130911C2226C                   ENDSR
190200130911C2226C/EJECT
190300130911  "  C************************************************************
190400130911  "  C* INIZIALIZZAZIONE PRELISTA
190500130911  "  C************************************************************
190600130911C2226C     INZS2         BEGSR
190700130911     C* pulizia SFL
190800130911     C                   SETOFF                                         3031
190900130911     C                   WRITE     FMTC2
191000130911     C                   SETON                                          31
191100130911     C*
191200130911     C* riempimento opzioni
191300130911     C                   MOVEA     $TX1          $TX
191400130912     C                   MOVEA     $FL2          $FL
191500130911     C                   Z-ADD     0             WCOZ
191600130911     C                   Z-ADD     0             WMXO
191700130911     C*  equivale al num.di elementi della schiera $TX1
191800130911     C                   Z-add     OpzLen        WMXO
191900130911     C                   Z-ADD     $C2Op1        $L01
192000130911     C                   Z-ADD     $C2Op2        $L02
192100130911     C                   EXSR      RIEOPZ
192200130911     C                   Z-ADD     23            X
192300130911     C                   MOVE      FF23          $FC2(X)
192400130912     C                   MOVEL     WOPZ1         C2OPZ
192500130912     C                   MOVEL     $ALLOPT       H1ALLOPT
192600130912     C                   MOVEL     WOPZ2         C2OPZ2
192700130912     C                   Z-ADD     WCOZ          C2COZ
192800130912     C* riga unica opzioni
192900130912     C                   eval      $A = %triml(c2opz)
193000130912     C                   eval      $B = %triml(c2opz2)
193100130912     C     $A            cat(P)    $B:2          C2opz1
193200130912     C*
193300130912     C                   eval      C2Unita   =D1Unita
193400130912     C                   eval      C2kcc     =D1kcc
193500130912     C                   eval      C2PosAttrD=D1PosAttrD
193600130912     C                   eval      C2attrib=D1Attrib
193700130912     C                   eval      C2attribD=D1AttribD
193800130912     C* Intestazione a seconda del raggruppamento
193900130912     C                   If        $RgrPre = 'P'
194000130912     C                   Eval      C2CodiceD ='P.Iva/Codice fiscale'
194100130912     C                   Else
194200130912     C                   Eval      C2CodiceD ='Soggetto            '
194300130912     C                   Endif
194400130912     C*
194500130912     C* CARICAMENTO SFL A PAGINE
194600130912     C*
194700130912     C                   Z-ADD     0             S2NRR
194800130912     C                   Z-ADD     0             WMAXS2
194900130912     C*
195000130911     C* Posizionamento su file pilota
195100130911     C                   CLEAR                   C2POS
195200130911     C                   EXSR      SETANAPRE
195300130911     C* ripristino i valori del posizionamento
195400130911     C                   MOVE      W2POS         C2POS
195500130911     C*
195600130911     C* Lettura file pilota
195700130911     C                   EXSR      REDANAPRE
195800130911     C*
195900130911     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
196000130911     C                   IF        $LastV = 'D1'
196100130911     C                   Z-ADD     SFLPAG        WPAG2
196200130911     C                   ENDIF
196300130911     C                   EXSR      ROLS2
196400130911     C* imposto il caricamento sfl del numero massimo di righe per pagina
196500130911     C                   Z-ADD     SFLPAG        WPAG2
196600130911     C*
196700130911C2226C                   ENDSR
196800130911  "  C/EJECT
196900130911  "  C************************************************************
197000130911  "  C* CARICAMENTO PAGINA LISTA
197100130911  "  C************************************************************
197200130912C2226C     ROLS2         BEGSR
197300130912     C*
197400130912     C                   SETOFF                                       32
197500130912     C                   Z-ADD     0             Y
197600130912     C                   Z-ADD     WMAXS2        S2NRR
197700130912     C*
197800130912     C* Leggo dal file anagrafico per caricare la lista
197900130912     C*
198000130912     C     $EndFilePre   DOWEQ     *OFF
198100130912     C     Y             ANDLT     WPAG2
198200130912     C     S2nrr         andlt     9999
198300130912     C*
198400130912     C                   EXSR      RIES2
198500130912     C*
198600130912     C                   ADD       1             S2NRR
198700130912     C                   ADD       1             Y
198800130912     C*
198900130912     C                   WRITE     FMTS2
199000130912     C*
199100130912     C                   EXSR      REDANAPRE
199200130911     C*
199300130911     C                   ENDDO
199400130911     C*
199500130911     C                   Z-ADD     S2NRR         WMAXS2               30
199600130911     C*
199700130911     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
199800130911     C*
199900130911     C     S2NRR         DIV       SFLPAG        PAGINE            4 0
200000130911     C                   MVR                     RESTO             3 0
200100130911     C     PAGINE        MULT      SFLPAG        C2RCD
200200130911     C     RESTO         IFGT      0
200300130911     C                   ADD       1             C2RCD
200400130911     C                   ELSE
200500130911     C                   SUB       SFLPAG        C2RCD
200600130911     C                   ADD       1             C2RCD
200700130911     C                   ENDIF
200800130911     C*
200900130911C2226C                   ENDSR
201000130911  "  C/EJECT
201100130911  "  C************************************************************
201200130911  "  C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
201300130911  "  C************************************************************
201400130911C2226C     SETANAPRE     BEGSR
201500130911     C*
201600130911     C                   exsr      InzSqlpre
201700130918     C*
201800130919     C                   exsr      Cartab
201900130919     C*
202000130919     C                   exsr      InzSqlTab
202100130919     C*
202200130918C2226C                   ENDSR
202300130918  "  C/EJECT
202400130918  "  C************************************************************
202500130919  "  C* CARICAMENTO tabella
202600130918  "  C************************************************************
202700130919C2226C     CARTAB        BEGSR
202800130920      *
202900130920     C                   exsr      clocurtab
203000130920      *
203100130920     C/EXEC SQL
203200130920     C+ DROP TABLE qtemp/NDRCATEMP
203300130920     C/END-EXEC
203400130920      *
203500130920     C                   if        SqlCod<0
203600130920     C                             and SqlCod<>-204
203700130920     C                   dump(a)
203800130920     C                   exsr      endpgm
203900130920     C                   endif
204000130919     C/EXEC SQL
204100130919     C+ CREATE TABLE QTEMP/NDRCATEMP (KCCPRE CHAR ( 6) NOT NULL WITH
204200130919     C+ DEFAULT, PARTIVAPRE CHAR ( 20) NOT NULL WITH DEFAULT, CDFISCPRE
204300130919     C+ CHAR ( 20) NOT NULL WITH DEFAULT, SOGGPRE CHAR ( 8) NOT NULL WITH
204400130919     C+ DEFAULT, DESCRPRE CHAR ( 44) NOT NULL WITH DEFAULT, IMPPRE DEC (
204500130919     C+ 19, 3) NOT NULL WITH DEFAULT)
204600130919     C/END-EXEC
204700130919     C                   if        SqlCod<0
204800130919     C                   dump(a)
204900130919     C                   exsr      endpgm
205000130919     C                   endif
205100130918     C*
205200130919     C                   movel     *off          $First
205300130918     C                   Clear                   $PKcc
205400130918     C                   Clear                   $PPartiva
205500130918     C                   Clear                   $PCdFisc
205600130918     C                   Clear                   $PSogg
205700130919     C                   Clear                   ImpPre
205800130919     C                   Clear                   DescrPre
205900130918     C*
206000130918     C                   DO        *hival
206100130918     C*
206200130918     C                   exsr      FetchSqlPre
206300130918     C*
206400130918     C                   If        SqlCod<0 or SqlCod=100
206500130918     C                   LEAVE
206600130918     C                   End
206700130918     C*
206800130918     C                   Reset                   NDC981DS
206900130919     C                   Eval      Societa981 = ds2Societa
207000130919     C                   Eval      Kcc981 = ds2Kcc
207100130919     C                   Eval      UniTa981 = ds2Unita
207200130918     C                   Eval      Partiva981 =  Ds2Partiva
207300130918     C                   Eval      Cdfisc981  =  Ds2CdFisc
207400130918     C                   Eval      Sogg981    =  Ds2Sogg
207500130919     C                   Call      'NDC981R'
207600130919     C                   parm                    Ndc981DS
207700130918     C*
207800130919     C                   If        Errore981 =  *On or
207900130919     C                             impscad981 = 0
208000130918     C                   Iter
208100130918     C                   Endif
208200131021      * Nuovo Filtro su importo scaduto
208300131022     C***                If        impscad981 < d1impda or
208400131022     C***                          d1impa > *zeros and
208500131022     C***                          impscad981 > d1impa
208600131022     C***                Iter
208700131022     C***                Endif
208800130919     C*
208900130919     C                   If        $First = *off
209000130919     C                   Movel     Ds2Kcc        $PKcc
209100130919     C                   Movel     Ds2Partiva    $PPartiva
209200130919     C                   Movel     Ds2CdFisc     $PCdFisc
209300130919     C                   Movel     Ds2Sogg       $PSogg
209400130919     C                   Eval      $First = *on
209500130919     C                   Endif
209600130918     C*
209700130919     C                   Select
209800130919     C*
209900130918     C                   When      $RgrPre = 'P'
210000130919     C*
210100130918     C                   IF        Ds2Kcc + Ds2Partiva + Ds2CdFisc  <>
210200130918     C                             $PKcc + $PPartiva  + $PCdFisc
210300130919     C                   Exsr      InsertTab
210400130919     C                   Movel     Ds2Kcc        $PKcc
210500130918     C                   Movel     Ds2Partiva    $PPartiva
210600130918     C                   Movel     Ds2CdFisc     $PCdFisc
210700130919     C                   Movel     Ds2Sogg       $PSogg
210800130919     C                   Clear                   Imppre
210900130919     C                   Clear                   descrpre
211000130918     C                   Endif
211100130918     C*
211200130918     C                   Other
211300130919     C*
211400130918     C                   IF        Ds2Kcc + Ds2Sogg <>
211500130918     C                             $PKcc + $PSogg
211600130919     C                   Exsr      InsertTab
211700130919     C                   Movel     Ds2Kcc        $PKcc
211800130918     C                   Movel     Ds2Partiva    $PPartiva
211900130918     C                   Movel     Ds2CdFisc     $PCdFisc
212000130919     C                   movel     Ds2Sogg       $PSogg
212100130919     C                   Clear                   Imppre
212200130919     C                   Clear                   descrpre
212300130919     C                   Endif
212400130918     C*
212500130918     C                   EndSl
212600130918     C*
212700130918     C                   Add       ImpScad981    ImpPre
212800130919     C                   movel     Ds2descr      descrpre
212900130918     C*
213000130918     C                   ENDDO
213100130920     C* Anche l'ultimo record
213200130920     C                   If        $First = *on
213300130920     C                   Exsr      InsertTab
213400130920     C                   Endif
213500130918     C*
213600130911C2226C                   ENDSR
213700130911  "  C/EJECT
213800130911  "  C************************************************************
213900130919  "  C* Inserimento tabella
214000130911  "  C************************************************************
214100130919C2226C     InsertTab     BEGSR
214200130919     C*
214300130919     C/EXEC SQL
214400130919     C+ INSERT INTO QTEMP/NDRCATEMP VALUES(:$pKcc,:$pPartiva,
214500130919     C+ :$pcdfisc, :$pSogg, :Descrpre, :imppre)
214600130919     C/END-EXEC
214700130919     C                   if        SqlCod<0
214800130919     C                   dump(a)
214900130919     C                   exsr      endpgm
215000130919     C                   endif
215100130919C2226C                   ENDSR
215200130919  "  C/EJECT
215300130919  "  C************************************************************
215400130919  "  C* LETTURA RCD ARCHIVIO PILOTA
215500130919  "  C************************************************************
215600130919C2226C     REDANAPRE     BEGSR
215700130911     C*
215800130911     C                   MOVEL     *OFF          $EndFilePre
215900130911     C                   MOVEL     *OFF          $RCDOKPre
216000130911     C*
216100130911     C     $EndFilePre   DOUEQ     *ON
216200130911     C     $RCDOKPRE     OREQ      *ON
216300130919     C*
216400130919     C                   exsr      FetchSqlTab
216500130919     C*
216600130919     C                   if        SqlCod<0 or SqlCod=100
216700130919     C                   MOVE      *ON           *IN21
216800130919     C                   ELSE
216900130919     C                   MOVE      *OFF          *IN21
217000130919     C                   END
217100130919     C*
217200130919     C                   MOVE      *IN21         $EndFilePre
217300130912     C*
217400130912     C                   If        $EndFilePre = *off
217500130919     C                   Eval      $rcdOkPre = *on
217600130919     C                   Endif
217700130912     C*
217800130912     C                   ENDDO
217900130912     C*
218000130912     C                   MOVE      $EndFilePre   *IN34                          SflEnd
218100130912     C*
218200130912C2226C                   ENDSR
218300130912  "  C/EJECT
218400130911  "  C************************************************************
218500130911  "  C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL S2
218600130911  "  C************************************************************
218700130911C2262C     CLCPAG2       BEGSR
218800130911     C* Input :
218900130911     C* - WSFL2= numero dell'ultimo rcd su cui era POSIZIONATO il
219000130911     C*          cursore
219100130911     C* - SFLPAG = numero rcd per pagina sfl
219200130911     C* Output :
219300130911     C* - WPAG2= pagina fino a cui deve essere ricaricato il sfl
219400130911     C*
219500130911     C     WSFL2         DIV       SFLPAG        PAGINE            4 0
219600130911     C                   MVR                     RESTO             3 0
219700130911     C     RESTO         IFGT      0
219800130911     C                   ADD       1             PAGINE
219900130911     C                   ENDIF
220000130911     C     PAGINE        MULT      SFLPAG        WPAG2
220100130911     C*
220200130911C2226C                   ENDSR
220300130911C2226C/EJECT
220400940224     C************************************************************
220500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
220600940224     C************************************************************
220700940224     C     CLCPAG        BEGSR
220800940224     C* Input :
220900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
221000940224     C*          cursore
221100940224     C* - SFLPAG = numero rcd per pagina sfl
221200940224     C* Output :
221300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
221400940224     C*
221500940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
221600940224     C                   MVR                     RESTO             3 0
221700940224     C     RESTO         IFGT      0
221800940224     C                   ADD       1             PAGINE
221900940224     C                   ENDIF
222000940224     C     PAGINE        MULT      SFLPAG        WPAG
222100940224     C*
222200940224     C                   ENDSR
222300940131     C/EJECT
222400940131     C************************************************************
222500940131     C* SELEZIONI SULLA RIGA LETTA
222600940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
222700940314     C* non rientra nei parametri di parzializzazione
222800940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
222900940314     C* superiore stabilito nei parametri
223000940314     C*
223100940131     C************************************************************
223200941214     C     SELANA        BEGSR
223300940131     C*
223400940202     C                   MOVEL     *ON           $RCDOK
223500070119     C                   MOVE      *hival        ALL9             15
223600100407     c* aggancio file per partita per reperire altri dati
223700100407     C                   clear                   SAvv              1
223800100407     C                   clear                   Ssol              1
223900100407     C                   clear                   SImp              1
224000100407     C                   clear                   SImpSCa           1
224100100511     C                   clear                   STel              1
224200100511     C                   clear                   Smail             1
224300130923C2226C                   clear                   SAtt              1
224400130923C2226C                   clear                   SAttSCa           1
224500100407     c                   exsr      LeggiRCa
224600970515     C*
224700940202     C                   ENDSR
224800940131     C/EJECT
224900100407     C************************************************************
225000100407     C* leggo rca per la partita in lettura
225100100407     C************************************************************
225200100407     C     LeggiRca      BEGSR
225300100407     c*
225400100407     c                   eval      RcaSocieta=DsSocieta
225500100407     c                   eval      RcaPosiz  =DsPosiz
225600100407     c                   eval      RcaKcc    =DsKcc
225700100407     c                   eval      RcaKsc    =DsKsc
225800100407     c                   eval      RcaDtPar  =DsDtPar
225900100407     c                   eval      RcaNrPar  =DsNrPar
226000100407     c                   eval      RcaSerPar =DsSerPar
226100100413     c     K07Rca01      setll     NdRca01L
226200100407     c                   do        *hival
226300100413     c     K07Rca01      reade     NdRca01L
226400100413     c                   if        %eof(NdRca01L)
226500100407     c                   leave
226600100407     c                   endif
226700100511     c* vedo quale è il tipo di avvitivà da svolgere
226800100511     C                   MOVE      *ALL'0'       XTBKEY
226900100511    >C                   MOVE      RcaAzione     XTBKEY
227000100511     C                   MOVE      '0'           XTBALC
227100100511     C                   MOVE      '1'           XTBRIC
227200100511     C                   MOVE      '047'         XTBCOD
227300100511     C                   CALLB     'XATB'
227400100511     C                   PARM                    XATBDS
227500100511     C     XTBERR        IFEQ      '0'
227600100511     C                   MOVEL     XTBUNI        Ang047ds
227700100407     c*
227800100511     c* avviso stampato
227900100520     c                   if        Attivit47='A'  and RcaStato<> *blanks
228000100407     c                   eval      SAvv  = *on
228100100407     c                   endif
228200100511     c* sollecito stampato
228300100520     c                   if        Attivit47='S'  and RcaStato <> *blanks
228400100407     c                   eval      SSol  = *on
228500100407     c                   endif
228600100511     c* telefonata fatta
228700100511     c                   if        Attivit47='T'  and RcaStato <> *blanks
228800100511     c                   eval      STel  = *on
228900100511     c                   endif
229000100511     c* lettera o e-mail fatte
229100100511     c                   if        Attivit47='L'  and RcaStato <> *blanks
229200100511     c                             or
229300100511     c                             (Attivit47='E'  and RcaStato <> *blanks)
229400100511     c                   eval      SMail = *on
229500100511     c                   endif
229600100524     c* impegno presente NON chiuso, attivo
229700100524     c                   if        Attivit47='I' and RcaStato = *blanks
229800100407     c                   eval      SImp  = *on
229900100407     c                   endif
230000100524     c* impegno scaduto presente, non chiuso
230100100524     c                   if        Attivit47='X' and RcaStato = *blanks
230200100524     c                   eval      SImpSca = *on
230300100407     c                   endif
230400130923C2226c* se attività libera ne verifico la presenza nel file
230500130923  "  C* ndatt00f
230600130923  "  C                   eval      ATTSocieta = RcaSocieta
230700130923  "  C                   eval      ATTKcc     = RcaKcc
230800130923  "  C                   eval      ATTKsc     = RcaKsc
230900130923  "  C                   eval      ATTDtPar   = RcaDtPar
231000130923  "  C                   eval      ATTNrPar   = RcaNrPar
231100130923  "  C                   eval      ATTSerPar  = RcaSerPar
231200130923  "  C                   eval      ATTDtSca   = RcaDtSca
231300130923  "  C                   eval      ATTProgr   = *Hival
231400130923  "  C     K08Att01      setgt     NdATT01l
231500130923  "  C     K07Att01      ReadPe    NdATT01l
231600130923  "  C                   If        Not %eof(NdAtt01l)
231700130923  "  C                   If        AttDtAtt > zdateIso
231800130923  "  C                   Eval      SAtt = *on
231900130923  "  C                   Else
232000130923  "  C                   eval      SAttSca = *on
232100130923  "  C                   Endif
232200130923C2226C                   Endif
232300100407     c*
232400100511     c                   endif
232500100407     c                   enddo
232600100407     c*
232700100407     c                   ENDSR
232800940131     C************************************************************
232900940131     C* RIEMPIMENTO RIGA LISTA
233000940131     C************************************************************
233100940131     C     RIES1         BEGSR
233200940131     C*
233300940131     C                   CLEAR                   FMTS1
233400100407     c                   move      SAvv          H1Avv
233500100407     c                   move      SSol          H1Sol
233600100407     c                   move      SImp          H1Imp
233700100407     c                   move      SImpSca       H1ImpSca
233800100511     c                   move      STel          H1Tel
233900100512     c                   move      Smail         H1in15
234000100419     c                   move      DsDivisa      H1Divisa
234100130923C2226c                   move      SAtt          H1Att
234200130923C2226c                   move      SAttSca       H1AttSca
234300950301     C*
234400950301     C* riempio i campi del sfl, compresi gli hidden con i valori
234500950301     C* del rcd letto
234600100407    >C                   MOVEL     DsKcc         H1Kcc
234700100407    >C                   MOVEL     DsKsc         S1Ksc
234800100402     C* Richiamo modulo controllo conto
234900100407     c                   move      Dskcc         $kcc
235000100407     c                   move      Dsksc         $ksc
235100100402     C                   EXSR      CALLMVC002
235200100402     C     $Esito        IFne      *ON
235300100402     C                   MOVEL     DEScr002      s1descr
235400100521     c                   if        DtAnn002 <=Zdateiso and
235500100521     c                             DtAnn002 <> *loval
235600100521     c                   move      *on           H1in18
235700100521    -C                   endif
235800100402    -C                   ENDIF
235900100402     c*
236000100407     c                   if        DsDtPar    <> *loval
236100100407     c     *jobrun       move      DsDtPar       s1dtPar
236200100402     c                   endif
236300100407     c                   z-add     DsNrPar       S1NrPar
236400100407     c                   move      DsSerPar      S1SerPar
236500100407     c*
236600100407     c*
236700100407     c                   move      DskscAge      s1kscAge
236800100402   1 C     s1kscage      IFNE      *blanks
236900100402     C                   CALLb     'XALLINEA'
237000100402     C                   PARM                    s1kscage
237100100402     C                   PARM      8             LEN               5 0
237200100402     C                   PARM                    XSCINP
237300100402     C* reperisco capoconto agente
237400100402     C                   EVAL      Xcapkcc  = 'F     '
237500100402     C                   MOVE      XSCSOC        XCAPSOCIET
237600100402     C                   MOVE      s1kscage      XCAPKSC
237700100402     C                   CALLB     'XCAPCLIFOR'
237800100402     C                   PARM                    XCAPSOCIET        3
237900100402     C                   PARM                    XCAPKCC           6
238000100402     C                   PARM                    XCAPKSC           8
238100100402     C* Richiamo modulo controllo conto
238200100402     c                   move      Xcapkcc       $kcc
238300100402     c                   move      Xcapksc       $ksc
238400100402     C                   EXSR      CALLMVC002
238500100402   2 C     $Esito        IFne      *ON
238600100402     C                   MOVEL     DESBREV002    s1kscageD
238700100402   2-C                   ENDIF
238800100402   1-C                   ENDIF
238900950301     C*
239000090325     C                   Eval      S1opz    =    Wopz
239100090325     C                   if        S1opz     <>  0
239200090325     C                   SetOn                                        32
239300090325     C                   endif
239400100407     c*
239500090325     c*
239600950301     C* imposto quegli attributi del sfl che sono modificabili
239700950301     C* al richiamo del determinato pgm
239800950301     C                   EXSR      ATRS1
239900940131     C*
240000940131     C                   ENDSR
240100950301     C/EJECT
240200950301     C************************************************************
240300950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
240400950301     C************************************************************
240500950301     C     ATRS1         BEGSR
240600950301     C*
240700971120     C* in questa routine bisogna attivare indicatori o campi programma
240800971120     C* che gestiscono gli attributi di uno o più campi di questa videata;
240900971120     C* attenzione : non è questo il posto in cui eseguire controlli,
241000971120     C*  dovrebbero già arrivare dei flag
241100970618     C*
241200100407     C                   MOVEL     X'A0'         H1AvvA
241300100407     C                   MOVEL     X'A0'         H1SolA
241400100407     C                   MOVEL     X'A0'         H1ImpA
241500100407     C                   MOVEL     X'A0'         H1ImpScaA
241600100511     C                   MOVEL     X'A0'         H1TelA
241700130923C2226C                   MOVEL     X'A0'         H1AttA
241800130923C2226C                   MOVEL     X'A0'         H1AttScaA
241900100412     c*
242000100407     c                   if        H1Avv=*on
242100100407     c                   move      Presente      H1AvvA
242200100407     c                   endif
242300950301     C*
242400100407     c                   if        H1Sol=*on
242500100408     c                   move      PreseBlu      H1SolA
242600100407     c                   endif
242700100511     C*
242800100511     c                   if        H1Tel=*on
242900100511     c                   move      PreseHig      H1TelA
243000100511     c                   endif
243100100511     C*
243200100512     c                   move      H1in15        *in15
243300100407     C*
243400100407     c                   if        H1Imp=*on
243500100407     c                   move      PresePnk      H1ImpA
243600100407     c                   endif
243700100407     C*
243800100407     c                   if        H1ImpSca=*on
243900100407     c                   move      PreseREd      H1ImpScaA
244000100407     c                   endif
244100100521     c* cliente annullato
244200100521     c                   move      H1in18        *in18
244300130923C2226C*
244400130923  "  C* Presenza di attività manuali in essere o scadute
244500130923  "  C                   if        H1Att=*on
244600130923  "  C                   move      PresePnk      H1AttA
244700130923  "  C                   endif
244800130923  "  C*
244900130923  "  C                   if        H1AttSca=*on
245000130923  "  C                   move      PreseREd      H1AttScaA
245100130923C2226c                   endif
245200100407     C*
245300950301     C                   ENDSR
245400941206     C/EJECT
245500941206     C************************************************************
245600941206     C* GESTIONE F08 VIDEO S1
245700941206     C************************************************************
245800941206     C     F08S1         BEGSR
245900941206     C*
246000941206     C                   MOVE      *ON           $FINE
246100090323    >C                   MOVE      '0'           RETL76
246200941206     C*
246300941206     C                   ENDSR
246400940309     C/EJECT
246500940309     C************************************************************
246600940309     C* GESTIONE F03 VIDEO S1
246700940309     C************************************************************
246800940309     C     F03S1         BEGSR
246900940309     C*
247000940309     C                   MOVE      *ON           $FINE
247100090323    >C                   MOVE      '1'           RETL76
247200940325     C* fine programma
247300940325     C                   EXSR      ENDPGM
247400940309     C*
247500940309     C                   ENDSR
247600130924C2226C************************************************************
247700130924  "  C* GESTIONE F05 VIDEO S1
247800130924  "  C************************************************************
247900130924C2226C     F05S1         BEGSR
248000130924     C*
248100130924     C                   MOVE      *ON           $INZS1
248200130924     C                   MOVE      *ON           WINZS1
248300130924     C*
248400130924C2226C                   ENDSR
248500940309     C************************************************************
248600940309     C* GESTIONE F12 VIDEO S1
248700940309     C************************************************************
248800940309     C     F12S1         BEGSR
248900940309     C*
249000940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
249100940309     C     $LOP          IFEQ      1
249200130912C2226C                   IF        $Pre = *on
249300130911  "  C                   MOVE      'S2'          $GEST
249400130923  "  C                   move      *ON           $INZS2                         iniz.sfl fine ciclo
249500130923  "  c                   move      *on           Winzs2
249600130911C2226C                   Else
249700940309     C                   MOVE      'D1'          $GEST
249800130911C2226C                   Endif
249900940309     C*  se la 1a videata è questa, per F12 torno a menu
250000940309     C                   ELSE
250100130912C2226C                   IF        $Pre = *on
250200130911  "  C                   MOVE      'S2'          $GEST
250300130911C2226C                   Else
250400940309     C                   MOVE      *ON           $FINE
250500090323    >C                   MOVE      '2'           RETL76
250600130911C2226C                   ENDIF
250700941027     C                   ENDIF
250800940309     C*
250900940309     C                   ENDSR
251000940309     C/EJECT
251100090325     C************************************************************
251200090325     C* GESTIONE F13 VIDEO S1
251300090325     C************************************************************
251400090325     C     F13S1         BEGSR
251500090325     C*
251600090325     C* E' simile alla funzione F13 del PDM: riporta l'opzione della
251700090325     C* riga del sfl su cui è posizionato il cursore dalla riga
251800090325     C* successiva fino alla fine del sfl stesso,
251900090325     C* senza sovrapporsi alle opzioni preesistenti.
252000090325     C* il controllo lo faccio solo se il sfl NON è vuoto
252100090325   1bC                   If        WMax    <>    0
252200090325     C* determino Riga/Colonna del cursore
252300090325     C                   MOVE      NRG           R$$
252400090325     C                   MOVE      NCL           C$$
252500090325     C                   Z-ADD     RIRI          CURR
252600090325     C                   Z-ADD     COCO          CURC
252700090325     C*
252800090325     C* se sono posizionato sul sfl
252900090325     C* salvo il numero del record su cui sono posizionato
253000090325     c                   Z-Add     C1Nrr         WC1Rcd
253100090325     C* leggo il rcd
253200090325     C     C1NrR         CHAIN     FMTS1                              21
253300090325     C* salvo l'opzione e il numero relativo
253400090325     C                   If        S1Opz   >     0
253500090325     C                   Z-Add     S1Opz         WOpz              2 0
253600090325     c                   Z-Add     C1Nrr         $X
253700090325     C* attivo SflNxtChg
253800090325     C                   SetOn                                        32
253900090325     C                   UpDate    FmtS1
254000090325     C                   SetOff                                       32
254100090325     C*
254200090325     C* mi posiziono sul record successivo
254300090325     C                   Add       1             $X
254400090325     c*
254500090325     C                   DoW       $X      <=    WMax
254600090325     C     $X            Chain     FmtS1                              21
254700090325     C   21              Leave
254800090325     C                   if        S1Opz    =    0
254900090325     C* imposto l'opzione solo se è possibile eseguirla
255000090325     c*
255100090325     C                   Z-Add     WOpz          S1Opz
255200090325     C                   endif
255300090325     C*                  endif
255400090325     C* attivo SflNxtChg
255500090325     C                   SetOn                                        32
255600090325     C                   UpDate    FmtS1
255700090325     C                   SetOff                                       32
255800090325     C*
255900090325     C                   Add       1             $X
256000090325     C                   enddo
256100090325     c*
256200090325     C                   Eval      WPag     =    9999
256300090325     C*
256400090325     C                   ExSr      RolS1
256500090325     C* reimposto sflpag
256600090325     C                   Eval      WPag     =    SflPag
256700090325     C*
256800090325     C* ripristino la posizione iniziale del record
256900090325     c                   Z-Add     WC1Rcd        C1Rcd
257000090325     C* ripristino il campo di salvataggio dell'opzione per non sporca
257100090325     C* re il campo S1Opz in caso di ricaricamento del sfl
257200090325     C                   Z-Add     0             WOpz
257300090325     C*
257400090325     C                   endif
257500090325     C*
257600090325     C                   endif
257700090325     C*
257800090325     C*
257900090325     C                   ENDSR
258000090325     C/EJECT
258100940309     C************************************************************
258200940309     C* GESTIONE F17 VIDEO S1
258300940309     C************************************************************
258400940309     C     F17S1         BEGSR
258500940309     C*
258600940309     C                   MOVE      'D1'          $GEST
258700941104     C* se il pgm è stato richiamato con chiave univoca
258800941104     C* e dopo è stata richiesta una parzializzazione
258900941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
259000941104     C* valore; propongo come ordinamento il 1°)
259100950606     C     $Unique       IFEQ      *ON
259200950606     C                   MOVE      *OFF          $Unique
259300090323    >C***                MOVE      *BLANK        UQ1L76
259400941027     C                   ENDIF
259500940831     C     $ORD          IFEQ      99
259600940831     C                   Z-ADD     1             $ORD
259700941104     C                   MOVE      *ON           $INZD1
259800941027     C                   ENDIF
259900940309     C*
260000940309     C                   ENDSR
260100940309     C/EJECT
260200940309     C************************************************************
260300940309     C* GESTIONE F18 VIDEO S1
260400940309     C************************************************************
260500940309     C     F18S1         BEGSR
260600940309     C*
260700940309     C                   MOVE      'D2'          $GEST
260800940309     C                   MOVEL     *ON           $INZD2
260900940831     C     $ORD          IFEQ      99
261000940831     C                   Z-ADD     1             $ORD
261100941027     C                   ENDIF
261200940309     C*
261300940309     C                   ENDSR
261400940309     C/EJECT
261500940309     C************************************************************
261600940309     C* GESTIONE F23 VIDEO S1
261700940309     C************************************************************
261800940309     C     F23S1         BEGSR
261900940309     C*
262000950906     C                   Z-ADD     $C1Op1        $L01
262100950906     C                   Z-ADD     $C1Op2        $L02
262200940309     C                   EXSR      RIEOPZ
262300941214     C                   Z-ADD     23            X
262400941214     C                   MOVE      FF23          $FC2(X)
262500070531     C                   MOVEL     WOPZ1         C1OPz
262600060224     C                   MOVEL     $ALLOPT       H1ALLOPT
262700940608     C                   MOVEL     WOPZ2         C1OPZ2
262800940309     C                   Z-ADD     WCOZ          C1COZ
262900070531     c* riga unica opzioni
263000070531     c                   eval      $A = %triml(c1opz)
263100070531     c                   eval      $B = %triml(c1opz2)
263200070531     c     $A            cat(P)    $B:2          C1opz1
263300940309     C*
263400940309     C                   ENDSR
263500940309     C/EJECT
263600940309     C************************************************************
263700940309     C* GESTIONE F24 VIDEO S1
263800940309     C************************************************************
263900940309     C     F24S1         BEGSR
264000940506     C*
264100940506     C                   MOVEA     $FX2          $FX
264200940506     C                   MOVEA     $FC2          $FC
264300940506     C                   MOVEA     $FR2          $FR
264400940506     C                   Z-ADD     $ULKS1        $ULK
264500950726     C                   Z-ADD     $S1L01        $L01
264600950726     C                   Z-ADD     $S1L02        $L02
264700940506     C                   EXSR      RIEKEY
264800940506     C                   Z-ADD     $ULK          $ULKS1            3 0
264900940720     C                   MOVEA     $FC           $FC2
265000070618     C                   MOVEL     $KEY1         Z2KE
265100060224     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
265200940506     C                   MOVEL     $KEY2         Z2KE2
265300070618     c* riga unica tasti
265400070618     c                   eval      $A = %triml(z2ke)
265500070618     c                   eval      $B = %triml(z2ke2)
265600070618     c     $A            cat(P)    $B:0          z2ke1
265700940506     C                   WRITE     FMTZ2
265800940506     C*
265900940309     C                   ENDSR
266000940128     C/EJECT
266100940128     C************************************************************
266200940131     C* CONTROLLO TESTATA LISTA
266300940128     C************************************************************
266400940131     C     CTRC1         BEGSR
266500940128     C*
266600940201     C                   MOVE      *OFF          *IN99
266700940131     C*
266800940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
266900940202     C* L'INIZIALIZZAZIONE LISTA
267000940202     C*
267100941214     C     C1POS         IFNE      W1POS
267200940202     C                   EXSR      POSS1
267300941027     C                   ENDIF
267400940202     C*
267500940202     C                   ENDSR
267600940202     C/EJECT
267700940202     C************************************************************
267800940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
267900940202     C************************************************************
268000940202     C     POSS1         BEGSR
268100940202     C*
268200940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
268300940202     C*
268400941118   B1C     WMAX          IFGT      0
268500940202     C                   READC     FMTS1                                  21
268600941118   B2C     *IN21         DOWEQ     *OFF
268700940202     C     *IN99         ANDEQ     *OFF
268800940202     C*
268900941118   B3C     S1OPZ         IFNE      0
269000960109     C                   SETON                                        92  99
269100941118   E3C                   ENDIF
269200940202     C* se rilevata opzione attiva, attivo sflnxtchg
269300941118   B3C     *IN99         IFEQ      *ON
269400940202     C                   MOVE      *ON           *IN32
269500950301     C*
269600950301     C* imposto quegli attributi del sfl che sono modificabili
269700950301     C* al richiamo del determinato pgm
269800941118     C                   EXSR      ATRS1
269900941118     C*
270000940202     C                   UPDATE    FMTS1
270100941118   X3C                   ELSE
270200940202     C                   READC     FMTS1                                  21
270300941118   E3C                   ENDIF
270400940202     C*
270500941118   E2C                   ENDDO
270600940202     C*
270700941118   E1C                   ENDIF
270800940202     C*
270900940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
271000940221     C* AL VALORE DI POSIZIONAMENTO :
271100940221     C* se questo è maggiore del valore del rispettivo campo di
271200940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
271300940221     C* con i valori di parzializzazione.
271400940221     C*
271500941118   B1C     *IN99         IFEQ      *OFF
271600940721    >C                   MOVE      *BLANK        W1POS
271700940202     C*
271800950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
271900950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
272000950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
272100950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
272200950525     C* verificata la condizione suddetta avverrà con posizionamento a
272300950525     C* partire dal valore di parzializzazione
272400950525     C*
272500941118   B2C                   SELECT
272600940202     C*
272700940202     C     $ORD          WHENEQ    1
272800070524    >C     C1POS         IFGT      D1KSC
272900951005    >C                   MOVEL     C1POS         W1POS
273000070524    >C                   CALLB     'XALLINEA'
273100070524    >C                   PARM                    W1Pos
273200070524    >C                   PARM      8             LEN               5 0
273300070524    >C                   PARM                    XSCINP
273400941027     C                   ENDIF
273500950525     C*
273600950525     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
273700950525     C*
273800941118   E2C                   ENDSL
273900950525     C*
274000950525     C* richiedo l'inizializzazione sfl
274100950525     C                   MOVE      *ON           $INZS1
274200940202     C*
274300941118   E1C                   ENDIF
274400940202     C*
274500940131     C                   ENDSR
274600940131     C/EJECT
274700940131     C************************************************************
274800940131     C* CONTROLLO OPZIONI LISTA
274900940131     C************************************************************
275000940131     C     CTRS1         BEGSR
275100940131     C*
275200940202     C                   MOVEL     *OFF          $ESCI
275300940201     C                   SETOFF                                       99
275400940131     C                   Z-ADD     0             S1OPZ
275500940131     C*
275600940127     C* Leggo il sfl solo se ci sono rcd
275700941118   B1C     WMAX          IFGT      0
275800940131     C                   READC     FMTS1                                  21
275900940127     C*
276000940131     C* esce se fine sfl o errore che richiede l'uscita
276100941118   B2C     *IN21         DOWEQ     *OFF
276200940131     C     $ESCI         ANDEQ     *OFF
276300940201     C                   Z-ADD     S1NRR         C1RCD
276400940131     C* ctrl su riga
276500940131     C                   EXSR      RECS1
276600940131     C* gestione opzioni
276700941118   B3C     S1OPZ         IFNE      0
276800940201     C     *IN99         ANDEQ     *OFF
276900940131     C                   EXSR      OPZS1
277000941118   E3C                   ENDIF
277100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
277200941118   B3C     *IN99         IFEQ      *ON
277300940131     C                   MOVE      *ON           *IN32
277400941118   E3C                   ENDIF
277500950301     C*
277600950301     C* imposto quegli attributi del sfl che sono modificabili
277700950301     C* al richiamo del determinato pgm
277800941118     C                   EXSR      ATRS1
277900941118     C*
278000940131     C                   Z-ADD     0             S1OPZ
278100940223     C*
278200940127     C                   UPDATE    FMTS1
278300941125     C                   MOVE      *OFF          *IN32
278400940223     C*
278500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
278600941118   B3C     $ESCI         IFEQ      *OFF
278700940131     C                   READC     FMTS1                                  21
278800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
278900941118   B4C     *IN21         IFEQ      *ON
279000940201     C                   MOVE      WINZS1        $INZS1
279100010620     C                   MOVE      *off          WINZS1
279200980603     C* se è avvenuta almeno una modifica
279300980603   B5C                   IF        $INZS1 = *on
279400940204     C* calcolo pagina a cui deve posizionarsi
279500940728     C     WSFL          IFGT      0
279600940728     C                   EXSR      CLCPAG
279700940728     C                   ELSE
279800940728     C                   Z-ADD     SFLPAG        WPAG
279900940728     C                   ENDIF
280000980603   E5C                   ENDIF
280100941118   E4C                   ENDIF
280200980603   E3C                   ENDIF
280300940131     C*
280400941118   E2C                   ENDDO
280500940127     C*
280600941118   E1C                   ENDIF
280700940131     C*
280800940127     C                   ENDSR
280900940127     C/EJECT
281000940127     C************************************************************
281100940131     C* CONTROLLO CAMPI I/O RIGA LISTA
281200940127     C************************************************************
281300940131     C     RECS1         BEGSR
281400940131     C*
281500940201     C* reset indicatori DSPATR
281600940201     C                   MOVE      *ALL'0'       IN4049           10
281700940201     C                   MOVEA     IN4049        *IN(40)
281800940201     C*
281900940131     C* Controllo se Utente Autorizzato alla scelta
282000940202     C     S1OPZ         IFNE      0
282100940131     C                   Z-ADD     1             X
282200940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
282300960117     C* mi salvo l'indice dell' opzione
282400960117     C   21              Z-add     X             PosOpz            2 0
282500940211     C     *IN21         IFEQ      *OFF
282600940223     C     $FL1(X)       OREQ      'N'
282700940131     C                   MOVE      *ON           $ESCI
282800960109     C                   SETON                                        409199
282900940131     C                   ENDIF
283000940202     C                   ENDIF
283100940131     C*
283200970424     C*
283300940131     C                   ENDSR
283400941118     C/EJECT
283500941118     C************************************************************
283600941118     C* GESTIONE OPZIONI LISTA
283700941118     C************************************************************
283800941118     C     OPZS1         BEGSR
283900950301     C*
284000950301     C                   Z-ADD     RIRI          CURR
284100950301     C                   Z-ADD     COCO          CURC
284200941118     C*
284300941027     C                   SELECT
284400090330     C* se opz=Scheda cliente
284500090330     C* richiamo il pgm di gestione
284600090330     C     S1OPZ         WHENEQ    1
284700090331     c                   move      '02'          opzL77
284800090401     c                   move      xscsoc        SocietaL77
284900100402    >C                   MOVE      C1Unita       UnitaL77
285000090330    >C                   MOVE      h1kcc         kccl77
285100090330    >C                   MOVE      s1ksc         kscl77
285200100408     c     *jobrun       move      s1dtpar       dtParl77
285300100408     c                   z-add     s1nrpar       NrparL77
285400100408     c                   move      s1serpar      SerParL77
285500100419     c                   move      H1divisa      DivisaL77
285600100421     c                   if        d1stato = xscon
285700100421     c                   eval      StatoL77 = *on
285800100421     c                   else
285900100421     c                   eval      StatoL77 = *off
286000100421     c                   endif
286100090330     C                   movel     ndcl77ds      kpjbu
286200090330     C                   CALL      'NDCL77R'                            21
286300090330     C                   PARM                    KPJBA
286400100426c2156C                   movel     kpjbu         ndcl77ds
286500100426c2156c                   exsr      RTNL77
286600940831     C*
286700101209c2176C* se opz=Chiusura scadenze partita
286800101209  "  C     S1OPZ         WHENEQ    15
286900101209c2176c                   exsr      AggRca
287000070531     C* se opz=Interrogazioni
287100070531     C* richiamo il pgm di gestione
287200090402     C     S1OPZ         WHENEQ    16
287300070531     C                   reset                   ana399ds
287400070531     C                   eval      A39societa = XSCSOC
287500070531     C                   eval      A39unita = *blanks
287600070531     C                   eval      A39ctb = 'CG'
287700070531     C                   eval      A39kcc = h1kcc
287800070531     C                   eval      A39ksc = s1ksc
287900070531     C                   movel     ana399ds      kpjbu
288000070531     C                   CALL      'ANA399R'                            21
288100070531     C                   PARM                    KPJBA
288200070531     C     A39ERRORI     IFEQ      '0'
288300070531     C                   endif
288400130912C2226C*
288500130912  "  C* se selezionata opzione personalizzata, richiamo il programma
288600130912  "  C* corrispondente
288700130912  "  C     $PGO1(PosOpz) Whenne    *Blank
288800130912  " >C                   RESET                   NDCL77DS
288900130912  "  c                   move      '02'          opzL77
289000130912  "  c                   move      xscsoc        SocietaL77
289100130912  " >C                   MOVE      C1Unita       UnitaL77
289200130912  " >C                   MOVE      h1kcc         kccl77
289300130912  " >C                   MOVE      s1ksc         kscl77
289400130912  "  c     *jobrun       move      s1dtpar       dtParl77
289500130912  "  c                   z-add     s1nrpar       NrparL77
289600130912  "  c                   move      s1serpar      SerParL77
289700130912  "  c                   move      H1divisa      DivisaL77
289800130912  "  c                   if        d1stato = xscon
289900130912  "  c                   eval      StatoL77 = *on
290000130912  "  c                   else
290100130912  "  c                   eval      StatoL77 = *off
290200130912  "  c                   endif
290300130912  "  C                   movel     ndcl77ds      kpjbu
290400130912  "  C*
290500130912  "  C                   CALL      $PGO1(PosOpz)                        22
290600130912  "  C                   PARM                    KPJBA
290700130912  "  C                   PARM                    NOMPGM
290800130912  "  C*
290900130912  " >C                   MOVEL     KPJBU         NDCL77DS
291000130912  "  C* ritorno da PGM
291100130912  " >C                   EXSR      RTNL77
291200130912C2226C*
291300070531     C                   MOVE      *HIVAL        $LASTV
291400940225     C*
291500950620     C* se altre opzioni
291600941027     C                   OTHER
291700940225     C*
291800941027     C                   ENDSL
291900940225     C*
292000940131     C                   ENDSR
292100130911C2226C************************************************************
292200130911  "  C* RIEMPIMENTO RIGA PRELISTA
292300130911  "  C************************************************************
292400130911C2226C     RIES2         BEGSR
292500130911     C*
292600130911     C                   CLEAR                   FMTS2
292700130911     C*
292800130911     C* riempio i campi del sfl, compresi gli hidden con i valori
292900130911     C* del rcd letto
293000130911     C*
293100130912     C                   Select
293200130912     C                   When      $RgrPre = 'P'
293300130918     C                   Eval      H2Kcc = KccPre
293400130918     C                   Eval      H2PartIva = PartIvaPre
293500130918     C                   Eval      H2CdFisc  = CdFiscPre
293600130918     C                   If        PartivaPre <> *blanks
293700130918     C                   Movel     PartIvaPre    S2Codice
293800130911     C                   Else
293900130918     C                   Movel     CdFiscPre     S2Codice
294000130911     C                   Endif
294100130918     C                   Movel     DescrPre      S2Descr
294200130918     C                   Z-ADD     ImpPre        S2Importo
294300130912     C                   Other
294400130919     C                   Eval      H2Kcc   = KccPre
294500130919     C                   Eval      H2Sogg  = SoggPre
294600130918     C                   Movel     H2Sogg        S2Codice
294700130918     C                   Movel     DescrPre      S2Descr
294800130918     C                   Z-ADD     ImpPre        S2Importo
294900130912     C                   EndSl
295000130911     C*
295100130911     C                   Eval      S2opz    =    Wopz
295200130911     C                   if        S2opz     <>  0
295300130911     C                   SetOn                                        32
295400130911     C                   endif
295500130911     c*
295600130911     c*
295700130911     C* imposto quegli attributi del sfl che sono modificabili
295800130911     C* al richiamo del determinato pgm
295900130911     C                   EXSR      ATRS2
296000130911     C*
296100130911     C                   ENDSR
296200130911C2226C/EJECT
296300130911  "  C************************************************************
296400130911  "  C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
296500130911  "  C************************************************************
296600130911C2226C     ATRS2         BEGSR
296700130911     C*
296800130911     C* in questa routine bisogna attivare indicatori o campi programma
296900130911     C* che gestiscono gli attributi di uno o più campi di questa videata;
297000130911     C* attenzione : non è questo il posto in cui eseguire controlli,
297100130911     C*  dovrebbero già arrivare dei flag
297200130911     C*
297300130911     C                   ENDSR
297400130911C2226C/EJECT
297500130911  "  C************************************************************
297600130911  "  C* GESTIONE F08 VIDEO S2
297700130911  "  C************************************************************
297800130911C2226C     F08S2         BEGSR
297900130911     C*
298000130911     C                   MOVE      *ON           $FINE
298100130911    >C                   MOVE      '0'           RETL76
298200130911     C*
298300130911C2226C                   ENDSR
298400130911  "  C/EJECT
298500130911  "  C************************************************************
298600130911  "  C* GESTIONE F03 VIDEO S2
298700130911  "  C************************************************************
298800130911C2226C     F03S2         BEGSR
298900130911     C*
299000130911     C                   MOVE      *ON           $FINE
299100130911    >C                   MOVE      '1'           RETL76
299200130911     C* fine programma
299300130911     C                   EXSR      ENDPGM
299400130911     C*
299500130911C2226C                   ENDSR
299600130911  "  C************************************************************
299700130911  "  C* GESTIONE F12 VIDEO S2
299800130911  "  C************************************************************
299900130911C2226C     F12S2         BEGSR
300000130911     C*
300100130911     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
300200130911     C     $LOP          IFEQ      1
300300130911     C                   MOVE      'D1'          $GEST
300400130911     C*  se la 1a videata è questa, per F12 torno a menu
300500130911     C                   ELSE
300600130911     C                   MOVE      *ON           $FINE
300700130911    >C                   MOVE      '2'           RETL76
300800130911     C                   ENDIF
300900130911     C*
301000130911C2226C                   ENDSR
301100130911  "  C/EJECT
301200130911  "  C************************************************************
301300130911  "  C* GESTIONE F13 VIDEO S1
301400130911  "  C************************************************************
301500130911C2226C     F13S2         BEGSR
301600130911     C*
301700130911     C* E' simile alla funzione F13 del PDM: riporta l'opzione della
301800130911     C* riga del sfl su cui è posizionato il cursore dalla riga
301900130911     C* successiva fino alla fine del sfl stesso,
302000130911     C* senza sovrapporsi alle opzioni preesistenti.
302100130911     C* il controllo lo faccio solo se il sfl NON è vuoto
302200130911   1bC                   If        WMaxS2  <>    0
302300130911     C* determino Riga/Colonna del cursore
302400130911     C                   MOVE      NRG           R$$
302500130911     C                   MOVE      NCL           C$$
302600130911     C                   Z-ADD     RIRI          CURR
302700130911     C                   Z-ADD     COCO          CURC
302800130911     C*
302900130911     C* se sono posizionato sul sfl
303000130911     C* salvo il numero del record su cui sono posizionato
303100130911     c                   Z-Add     C2Nrr         WC2Rcd
303200130911     C* leggo il rcd
303300130911     C     C2NrR         CHAIN     FMTS2                              21
303400130911     C* salvo l'opzione e il numero relativo
303500130911     C                   If        S2Opz   >     0
303600130911     C                   Z-Add     S2Opz         WOpzS2            2 0
303700130911     c                   Z-Add     C2Nrr         $XS2
303800130911     C* attivo SflNxtChg
303900130911     C                   SetOn                                        32
304000130911     C                   UpDate    FmtS2
304100130911     C                   SetOff                                       32
304200130911     C*
304300130911     C* mi posiziono sul record successivo
304400130911     C                   Add       1             $X
304500130911     c*
304600130911     C                   DoW       $X      <=    WMaxS2
304700130911     C     $XS2          Chain     FmtS2                              21
304800130911     C   21              Leave
304900130911     C                   if        S2Opz    =    0
305000130911     C* imposto l'opzione solo se è possibile eseguirla
305100130911     c*
305200130911     C                   Z-Add     WOpzS2        S2Opz
305300130911     C                   endif
305400130911     C*                  endif
305500130911     C* attivo SflNxtChg
305600130911     C                   SetOn                                        32
305700130911     C                   UpDate    FmtS2
305800130911     C                   SetOff                                       32
305900130911     C*
306000130911     C                   Add       1             $XS2
306100130911     C                   enddo
306200130911     c*
306300130911     C                   Eval      WPag     =    9999
306400130911     C*
306500130911     C                   ExSr      RolS2
306600130911     C* reimposto sflpag
306700130911     C                   Eval      WPag     =    SflPag
306800130911     C*
306900130911     C* ripristino la posizione iniziale del record
307000130911     c                   Z-Add     WC2Rcd        C2Rcd
307100130911     C* ripristino il campo di salvataggio dell'opzione per non sporca
307200130911     C* re il campo S1Opz in caso di ricaricamento del sfl
307300130911     C                   Z-Add     0             WOpzS2
307400130911     C*
307500130911     C                   endif
307600130911     C*
307700130911     C                   endif
307800130911     C*
307900130911     C*
308000130911C2226C                   ENDSR
308100130911  "  C/EJECT
308200130911  "  C************************************************************
308300130911  "  C* GESTIONE F17 VIDEO S2
308400130911  "  C************************************************************
308500130911C2226C     F17S2         BEGSR
308600130911     C*
308700130911     C                   MOVE      'D1'          $GEST
308800130911     C* se il pgm è stato richiamato con chiave univoca
308900130911     C* e dopo è stata richiesta una parzializzazione
309000130911     C* tolgo la gestione della chiave univoca (azzere/abblenco il
309100130911     C* valore; propongo come ordinamento il 1°)
309200130911     C     $Unique       IFEQ      *ON
309300130911     C                   MOVE      *OFF          $Unique
309400130911    >C***                MOVE      *BLANK        UQ1L76
309500130911     C                   ENDIF
309600130911     C     $ORD          IFEQ      99
309700130911     C                   Z-ADD     1             $ORD
309800130911     C                   MOVE      *ON           $INZD1
309900130911     C                   ENDIF
310000130911     C*
310100130911C2226C                   ENDSR
310200130911  "  C/EJECT
310300130911  "  C************************************************************
310400130911  "  C* GESTIONE F18 VIDEO S2
310500130911  "  C************************************************************
310600130911C2226C     F18S2         BEGSR
310700130911     C*
310800130911     C                   MOVE      'D2'          $GEST
310900130911     C                   MOVEL     *ON           $INZD2
311000130911     C     $ORD          IFEQ      99
311100130911     C                   Z-ADD     1             $ORD
311200130911     C                   ENDIF
311300130911     C*
311400130911C2226C                   ENDSR
311500130911  "  C/EJECT
311600130911  "  C************************************************************
311700130911  "  C* GESTIONE F23 VIDEO S2
311800130911  "  C************************************************************
311900130911C2226C     F23S2         BEGSR
312000130911     C*
312100130911     C                   Z-ADD     $C2Op1        $L01
312200130911     C                   Z-ADD     $C2Op2        $L02
312300130911     C                   EXSR      RIEOPZ
312400130911     C                   Z-ADD     23            X
312500130911     C                   MOVE      FF23          $FC2(X)
312600130911     C                   MOVEL     WOPZ1         C2OPz
312700130911     C                   MOVEL     $ALLOPT       H1ALLOPT
312800130911     C                   MOVEL     WOPZ2         C2OPZ2
312900130911     C                   Z-ADD     WCOZ          C2COZ
313000130911     c* riga unica opzioni
313100130911     c                   eval      $A = %triml(c2opz)
313200130911     c                   eval      $B = %triml(c2opz2)
313300130911     c     $A            cat(P)    $B:2          C2opz1
313400130911     C*
313500130911     C                   ENDSR
313600130911C2226C/EJECT
313700130911  "  C************************************************************
313800130911  "  C* GESTIONE F24 VIDEO S2
313900130911  "  C************************************************************
314000130911C2226C     F24S2         BEGSR
314100130911     C*
314200130911     C                   MOVEA     $FX2          $FX
314300130911     C                   MOVEA     $FC2          $FC
314400130911     C                   MOVEA     $FR2          $FR
314500130911     C                   Z-ADD     $ULKS2        $ULK
314600130911     C                   Z-ADD     $S2L01        $L01
314700130911     C                   Z-ADD     $S2L02        $L02
314800130911     C                   EXSR      RIEKEY
314900130911     C                   Z-ADD     $ULK          $ULKS2            3 0
315000130911     C                   MOVEA     $FC           $FC2
315100130911     C                   MOVEL     $KEY1         Z2KE
315200130911     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
315300130911     C                   MOVEL     $KEY2         Z2KE2
315400130911     c* riga unica tasti
315500130911     c                   eval      $A = %triml(z2ke)
315600130911     c                   eval      $B = %triml(z2ke2)
315700130911     c     $A            cat(P)    $B:0          z2ke1
315800130911     C                   WRITE     FMTZ2
315900130911     C*
316000130911     C                   ENDSR
316100130911C2226C/EJECT
316200130911  "  C************************************************************
316300130911  "  C* CONTROLLO TESTATA PRELISTA
316400130911  "  C************************************************************
316500130911C2226C     CTRC2         BEGSR
316600130911     C*
316700130911     C                   MOVE      *OFF          *IN99
316800130911     C*
316900130911     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
317000130911     C* L'INIZIALIZZAZIONE LISTA
317100130911     C*
317200130911     C     C2POS         IFNE      W2POS
317300130911     C                   EXSR      POSS2
317400130911     C                   ENDIF
317500130911     C*
317600130911     C                   ENDSR
317700130911C2226C/EJECT
317800130911  "  C************************************************************
317900130911  "  C* CONTROLLI PER POSIZIONAMENTO SU PRELISTA
318000130911  "  C************************************************************
318100130911C2226C     POSS2         BEGSR
318200130911     C*
318300130911     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
318400130911     C*
318500130911   B1C     WMAXS2        IFGT      0
318600130911     C                   READC     FMTS2                                  21
318700130911   B2C     *IN21         DOWEQ     *OFF
318800130911     C     *IN99         ANDEQ     *OFF
318900130911     C*
319000130911   B3C     S2OPZ         IFNE      0
319100130911     C                   SETON                                        92  99
319200130911   E3C                   ENDIF
319300130911     C* se rilevata opzione attiva, attivo sflnxtchg
319400130911   B3C     *IN99         IFEQ      *ON
319500130911     C                   MOVE      *ON           *IN32
319600130911     C*
319700130911     C* imposto quegli attributi del sfl che sono modificabili
319800130911     C* al richiamo del determinato pgm
319900130911     C                   EXSR      ATRS2
320000130911     C*
320100130911     C                   UPDATE    FMTS2
320200130911   X3C                   ELSE
320300130911     C                   READC     FMTS2                                  21
320400130911   E3C                   ENDIF
320500130911     C*
320600130911   E2C                   ENDDO
320700130911     C*
320800130911   E1C                   ENDIF
320900130911     C*
321000130911     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
321100130911     C* AL VALORE DI POSIZIONAMENTO :
321200130911     C* se questo è maggiore del valore del rispettivo campo di
321300130911     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
321400130911     C* con i valori di parzializzazione.
321500130911     C*
321600130911   B1C     *IN99         IFEQ      *OFF
321700130911    >C                   MOVE      *BLANK        W2POS
321800130911     C*
321900130911     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
322000130911     C* è maggiore del relativo valore di parzializzazione, nel qual caso
322100130911     C* imposto la variabile del posizionamento (W2Pos) al nuovo valore;
322200130911     C* richiedo comunque un ricaricamento del sfl che, se non si è
322300130911     C* verificata la condizione suddetta avverrà con posizionamento a
322400130911     C* partire dal valore di parzializzazione
322500130911     C*
322600130911    >C     C2POS         IFGT      *blanks
322700130911    >C                   MOVEL     C2POS         W2POS
322800130911    >C                   CALLB     'XALLINEA'
322900130911    >C                   PARM                    W2Pos
323000130911    >C                   PARM      8             LEN               5 0
323100130911    >C                   PARM                    XSCINP
323200130911     C                   ENDIF
323300130911     C*
323400130911     C* richiedo l'inizializzazione sfl
323500130911     C                   MOVE      *ON           $INZS2
323600130911     C*
323700130911   E1C                   ENDIF
323800130911     C*
323900130911     C                   ENDSR
324000130911C2226C/EJECT
324100130911  "  C************************************************************
324200130911  "  C* CONTROLLO OPZIONI LISTA
324300130911  "  C************************************************************
324400130911C2226C     CTRS2         BEGSR
324500130911     C*
324600130911     C                   MOVEL     *OFF          $ESCI
324700130911     C                   SETOFF                                       99
324800130911     C                   Z-ADD     0             S2OPZ
324900130911     C*
325000130911     C* Leggo il sfl solo se ci sono rcd
325100130911   B1C     WMAXS2        IFGT      0
325200130911     C                   READC     FMTS2                                  21
325300130911     C*
325400130911     C* esce se fine sfl o errore che richiede l'uscita
325500130911   B2C     *IN21         DOWEQ     *OFF
325600130911     C     $ESCI         ANDEQ     *OFF
325700130911     C                   Z-ADD     S2NRR         C2RCD
325800130911     C* ctrl su riga
325900130911     C                   EXSR      RECS2
326000130911     C* gestione opzioni
326100130911   B3C     S2OPZ         IFNE      0
326200130911     C     *IN99         ANDEQ     *OFF
326300130911     C                   EXSR      OPZS2
326400130911   E3C                   ENDIF
326500130911     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
326600130911   B3C     *IN99         IFEQ      *ON
326700130911     C                   MOVE      *ON           *IN32
326800130911   E3C                   ENDIF
326900130911     C*
327000130911     C* imposto quegli attributi del sfl che sono modificabili
327100130911     C* al richiamo del determinato pgm
327200130911     C                   EXSR      ATRS2
327300130911     C*
327400130911     C                   Z-ADD     0             S2OPZ
327500130911     C*
327600130911     C                   UPDATE    FMTS2
327700130911     C                   MOVE      *OFF          *IN32
327800130911     C*
327900130911     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
328000130911   B3C     $ESCI         IFEQ      *OFF
328100130911     C                   READC     FMTS2                                  21
328200130911     C* a fine ciclo ripristino il flag richiesta iniz.sfl
328300130911   B4C     *IN21         IFEQ      *ON
328400130911     C                   MOVE      WINZS2        $INZS2
328500130911     C                   MOVE      *off          WINZS2
328600130911     C* se è avvenuta almeno una modifica
328700130911   B5C                   IF        $INZS2 = *on
328800130911     C* calcolo pagina a cui deve posizionarsi
328900130911     C     WSFL2         IFGT      0
329000130911     C                   EXSR      CLCPAG
329100130911     C                   ELSE
329200130911     C                   Z-ADD     SFLPAG        WPAG2
329300130911     C                   ENDIF
329400130911   E5C                   ENDIF
329500130911   E4C                   ENDIF
329600130911   E3C                   ENDIF
329700130911     C*
329800130911   E2C                   ENDDO
329900130911     C*
330000130911   E1C                   ENDIF
330100130911     C*
330200130911     C                   ENDSR
330300130911C2226C/EJECT
330400130911  "  C************************************************************
330500130911  "  C* CONTROLLO CAMPI I/O RIGA PRELISTA
330600130911  "  C************************************************************
330700130911C2226C     RECS2         BEGSR
330800130911     C*
330900130911     C* reset indicatori DSPATR
331000130911     C                   MOVE      *ALL'0'       IN4049           10
331100130911     C                   MOVEA     IN4049        *IN(40)
331200130911     C*
331300130911     C* Controllo se Utente Autorizzato alla scelta
331400130911     C     S2OPZ         IFNE      0
331500130911     C                   Z-ADD     1             X
331600130912     C     S2OPZ         LOOKUP    $OP2(X)                                21
331700130911     C* mi salvo l'indice dell' opzione
331800130911     C   21              Z-add     X             PosOpz            2 0
331900130911     C     *IN21         IFEQ      *OFF
332000130912     C     $FL2(X)       OREQ      'N'
332100130911     C                   MOVE      *ON           $ESCI
332200130911     C                   SETON                                        409199
332300130911     C                   ENDIF
332400130911     C                   ENDIF
332500130911     C*
332600130911     C*
332700130911     C                   ENDSR
332800130911C2226C/EJECT
332900130911  "  C************************************************************
333000130911  "  C* GESTIONE OPZIONI LISTA
333100130911  "  C************************************************************
333200130911C2226C     OPZS2         BEGSR
333300130911     C*
333400130911     C                   Z-ADD     RIRI          CURR
333500130911     C                   Z-ADD     COCO          CURC
333600130911     C*
333700130911     C                   SELECT
333800130911     C* se opz=Lista
333900130911     C* Proseguo con il pgm di lista
334000130911     C     S2OPZ         WHENEQ    1
334100130911     C                   MOVE      'S1'          $GEST
334200130911     C                   MOVE      *ON           $INZS1
334300130911     C                   MOVE      *BLANK        W1POS
334400130911     C                   EXSR      GESS1
334500130911     C*
334600130911     C                   MOVE      *HIVAL        $LASTV
334700130911     C*
334800130911     C* se altre opzioni
334900130911     C                   OTHER
335000130911     C*
335100130911     C                   ENDSL
335200130911     C*
335300130911     C                   ENDSR
335400101209C2176 ************************************************************
335500110412  "   * Aggiornam. x chiusura fasi (tutte le fasi aperte della partita)
335600101209  "   ************************************************************
335700101209C2176C     AggRca        Begsr
335800110412D2680C* emetto richiesta data chiusura
335900130924C2226C                   If        (PrimaVolta = 0 and
336000130924  "  C                             $Tastosav = F13) or
336100130924C2226C                             $Tastosav <> F13
336200110412  "  c                   eval      D5DtChiu   =udate
336300130924  "  c                   eval      D5DtPar    =S1DtPar
336400130924  "  c                   eval      D5NrPar    =S1NrPar
336500130924  "  c                   eval      D5SerPar   =S1SerPar
336600130923D2680c                   clear                   WWMSG
336700130923C2226c                   clear                   D5Nota
336800130923D2680c                   eval      Idmsg= Msg(1)
336900110412  "  c                   exsr      PrendiMsg
337000130924D2680c                   eval      Z5Key=txtmsg
337100130924C2226c                   eval      Idmsg= Msg(2)
337200130924  "  c                   exsr      PrendiMsg
337300130924C2226c                   eval      Z5Key=%trim(Z5Key) + ' ' +%trim(Txtmsg)
337400130924D2680c     emetti        tag
337500110412  "  c                   exfmt     Fmtd5
337600130924C2226C                   Eval      Primavolta = 1
337700130923D2680c                   if        $tasto=F12
337800130924C2226C                   If        $tastosav = F13
337900130924     C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
338000130924     c                   move      *on           Winzs1
338100130924  "  C                   Eval      $esci = *on
338200130924C2226C                   Endif
338300130924D2680c                   leavesr
338400110412  "  c                   endif
338500110412  "  C                   if        D5DtChiu <>   *Zeros
338600110412  "  C     *JobRun       Test(d)                 D5DtChiu               21
338700110412  " >C     *In21         ifeq      *On
338800110412  "  c* non corretta
338900110412  " >C                   SetOn                                        6699
339000110412  "  c                   goto      emetti
339100110412  " >C                   endif
339200110412  "  c                   else
339300110412  "  c* non immessa
339400110412  " >C                   SetOn                                        6699
339500110412  "  c                   goto      emetti
339600110412D2680C                   endif
339700130924  "  C                   If        $tasto <> F06
339800130924  "  C                   goto      emetti
339900130924  "  C                   Else
340000130924  "  C                   Eval      $tastoSav2 = F06
340100130924  "  C                   ENdif
340200130924C2226C                   ENdif
340300130924C2226C                   If        $tastosav2 = F06
340400101209     C                   eval      Rcasocieta = xscsoc
340500101209     C                   eval      RcaPosiz   = posattr057
340600101209     C                   eval      Rcakcc     = h1kcc
340700101209     C                   eval      rcaksc     = s1ksc
340800101209     C     *jobrun       move      s1dtpar       RcaDtPar
340900101209     C                   eval      rcanrpar   = s1nrpar
341000101209     C                   eval      rcaserpar  = s1serpar
341100130924     C*
341200101209    >C     K07Rca04      Setll     NDRCA04L
341300101209     c                   do        *hival
341400101209    >C     K07Rca04      reade     NDRCA04                              2221
341500101209     c   21              leave
341600101209     C*
341700101209     C* Gestione RCD allocato
3418001012093    C     *IN22         IFEQ      *ON
341900101209    >C                   MOVEL     'NDRCA00F'    MSGFIL
342000101209     C                   EXSR      MSGPGM
342100101209     c                   if        Msgrtn='C'
342200101209     c                   leave
342300101209     c                   else
342400101209     c* mi riposiziono per riprovare con la stessa scadenza
342500101209    >C     K08Rca04      Setll     NDRCA04L
342600101209     c                   iter
342700101209     c                   endif
3428001012093-   C                   ENDIF
342900101209     c*
343000101210     c* non chiude le fasi impegno
343100101210     c                   if        RcaAzione='98'or
343200101210     c                             RcaAzione='99'
343300101210     c                   iter
343400101210     c                   endif
343500101210     c*
343600101209     C                   if        *in21=*off
343700101209     c                             and RcaDtChiu=*loval
343800110412D2680x*************      eval      RcaDtChiu=Zdateiso
343900110412D2680c     *jobrun       move      D5DtChiu      RcaDtChiu
344000101209     c                   eval      RcaUteChiu=sdsusr
344100101209     c                   eval      RcaStato='1'
344200130913C2226C                   If        D5Nota <> *blanks
344300130913  "  C                   eval      RcaNota = D5Nota
344400130913C2226C                   Endif
344500101209     c                   update    NdRCA04
344600101209     C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
344700101209     c                   move      *on           Winzs1
344800101209     c                   ENDIF
344900101209     c*
345000101209     c                   enddo
345100130924C2226c                   ENDIF
345200101209     c*
345300101209c2176c                   endsr
345400940223     C/EJECT
345500101209c2176C******************************************************
345600101209  "  C* PGM PER GESTIONE MESSAGGI ERRORE                   *
345700101209  "  C******************************************************
345800101209c2176C     MSGPGM        BEGSR
345900101209     C*
346000101209     C                   Z-ADD     2             MSGMSG
346100101209     C                   MOVEL     'S'           MSGRSP
346200101209     C                   MOVEL     'CR'          MSGTPR
346300101209     C                   MOVEL     'S'           MSGEMV
346400101209     C                   Z-ADD     10            MSGRGP
346500101209     C                   MOVEL     'S'           MSGLCK
346600101209     C                   MOVEA     STATUS        STS
346700101209     C                   MOVEL     'N'           MSGCNL
346800101209     C                   MOVEL     'N'           MSGVID
346900101209     C                   MOVEL     'N'           MSGOPE
347000101209     C                   MOVEL     'N'           MSGSTP
347100101209     C                   CALL      'XMSGR'
347200101209     C                   PARM                    XMSGDS
347300101209     C*
347400101209c2176C                   ENDSR
347500101209     C/EJECT
347600100426c2156C************************************************************
347700100426     C* GESTIONE RITORNO DA PGM DI GESTIONE
347800100426     C************************************************************
347900100426c2156C     RTNL77        BEGSR
348000100426     C*
348100100426     C* modo di ritorno
348200100426     C*
348300100426     C                   SELECT
348400100426     C* questi modi di utilizzo dei valori di ritorno dal
348500100426     C* pgm di manutenzione rcd di anagrafica sono delle
348600100426     C* proposte, normalmente sempre valide, ma modificabili
348700100426     C* per situazioni particolari
348800100426     C* 1 = F3
348900100426    >C     RETL77        WHENEQ    '1'
349000100426     C                   MOVE      *ON           $ESCI                          sospese READC
349100100426     C                   MOVE      *ON           $FINE                          fine pgm
349200100426    >C                   MOVE      '1'           RETL76
349300100426     C* 2 = F12
349400100426    >C     RETL77        WHENEQ    '2'
349500100426     C* se il pgm di manutenzione ha prima una guida, è necessario anche
349600100426     c* questo test, altrimenti prevale il ret002 e il sfl non viene
349700100426     C* ricaricato
349800100426    >C     OPRL77        ANDNE     '1'
349900100426     C                   MOVE      *ON           $ESCI                          sospese READC
350000100426     C*
350100100426     C                   ENDSL
350200100426     C*
350300100426     C* operazione eseguite dal pgm chiamato
350400100426     C*
350500100426     C                   SELECT
350600100426     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
350700100426    >C     OPRL77        WHENEQ    '1'
350800100426     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
350900100426     C*
351000100426     C                   ENDSL
351100100426     C*
351200100426     C* funzione non eseguibile per errore :
351300100426     C*
351400100426     C                   ENDSR
351500100426     C/EJECT
351600950301     C************************************************************
351700950301     C* AGGIORNAMENTO RIGA LISTA
351800950301     C************************************************************
351900950301     C     AGGS1         BEGSR
352000950301     C*
352100950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
352200950301     C* di manutenzione dopo una opzione 02
352300950301     C*
352400950301     C*
352500950301     C                   ENDSR
352600941124     C/EJECT
352700940131     C************************************************************
352800940131     C* GESTIONE VALORI ASSUNTI
352900940131     C************************************************************
353000940131     C     GESD2         BEGSR
353100940131     C*
353200941118   B1C     $INZD2        IFEQ      *ON
353300940131     C                   EXSR      INZD2
353400940223     C                   MOVE      *OFF          $INZD2
353500941118   E1C                   ENDIF
353600940202     C*
353700941118   B1C     $LASTV        IFNE      'D2'
353800940923     C                   EXSR      GESPIE
353900961023   B1C     $LASTV        IFNE      *HIVAL
354000940202     C                   MOVE      $LASTV        $LASTG
354100961023     C                   ENDIF
354200940202     C                   MOVE      'D2'          $LASTV
354300941118   E1C                   ENDIF
354400940131     C*
354500940707     C* se esistono errori sulla videata
354600940707     C* emetto la write del formato a indicatori spenti per vedere
354700940707     C* le eventuali decodifiche
354800941118   B1C     *IN99         IFEQ      *ON
354900940707     C                   MOVEA     *IN           WIN
355000940707     C                   MOVE      *ALL'0'       IN5098           49
355100940707     C                   MOVEA     IN5098        *IN(50)
355200940707     C                   WRITE     FMTD2
355300940707     C                   MOVEA     WIN           *IN
355400941118   E1C                   ENDIF
355500940707     C*
355600940131     C                   EXFMT     FMTD2
355700960605     C*
355800960605     C* azzero la posizione del cursore
355900960605     C                   Z-ADD     0             H1RIGA
356000960605     C                   Z-ADD     0             H1COLO
356100941205     C*
356200951005     C* CONTROLLO ABILITAZIONE TASTI
356300951005     C* INPUT
356400951005     C* $FC3 = Abilitazioni videata D2
356500951005     C* OUTPUT
356600951005     C* *IN97 + *IN99 Errori
356700951005     C                   MOVEA     $FC3          $FC
356800951005     C                   EXSR      CTRKEY
356900941205     C*
357000941205   B1C     *IN99         IFEQ      *OFF
357100941205     C*
357200941205   B2C                   SELECT
357300090324     C* F2=Decodifica
357400090324     C     $TASTO        WHENEQ    F02
357500090324     C                   EXSR      F02D2
357600940131     C* F3=Fine
357700940506     C     $TASTO        WHENEQ    F03
357800940309     C                   EXSR      F03D2
357900090324     C* F4=Ricerca
358000090324     C     $TASTO        WHENEQ    F04
358100090324     C                   EXSR      SEARCH
358200940131     C* F12=Ritorno
358300940506     C     $TASTO        WHENEQ    F12
358400940309     C                   EXSR      F12D2
358500940916     C* F24=Altri tasti
358600940916     C     $TASTO        WHENEQ    F24
358700940916     C                   EXSR      F24D2
358800940131     C*
358900940131     C                   OTHER
359000940131     C                   EXSR      CTRD2
359100941205   B3C     *IN99         IFEQ      *OFF
359200940211     C                   MOVE      $LASTG        $GEST
359300941205   B4C                   SELECT
359400940202     C     $LASTG        WHENEQ    'S1'
359500940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
359600941205   B5C     $ORD          IFNE      D2ORD
359700940223     C                   MOVE      *ON           $INZS1
359800940302     C                   MOVE      *BLANK        W1POS
359900941205   E5C                   ENDIF
360000130912C2226C     $LASTG        WHENEQ    'S2'
360100130912  "  C* se è stato modificato il raggruppamento ricarico prelista
360200130912  "  C     $RGRPRE       IFNE      D2RGRPRE
360300130912  "  C                   MOVE      *ON           $INZS2
360400130912  "  C                   MOVE      *BLANK        W2POS
360500130912C2226C                   ENDIF
360600940202     C     $LASTG        WHENEQ    'D1'
360700940318     C* per ora niente
360800941205   E4C                   ENDSL
360900940131     C* definisco i nuovi valori di default
361000940201     C                   Z-ADD     D2LOP         $LOP
361100940131     C                   Z-ADD     D2ORD         $ORD
361200130912C2226C                   If        D2Pre = XscOn
361300130912  "  C                   Movel     *on           $Pre
361400130910  "  C                   Else
361500130912  "  C                   Movel     *off          $Pre
361600130912  "  C                   Endif
361700130912C2226C                   Movel     D2RgrPre      $RgrPre
361800950322     C*
361900950322     C* aggiorno valori assunti
362000950328     C* se richiesto il salvataggio
362100950328   B4C     D2BSV         IFEQ      1
362200950322     C                   EXSR      AGGMPE
362300950322   E4C                   ENDIF
362400950322     C*
362500941205   E3C                   ENDIF
362600940131     C*
362700941205   E2C                   ENDSL
362800940131     C*
362900941205   E1C                   ENDIF
363000941205     C*
363100940131     C                   ENDSR
363200940223     C/EJECT
363300940131     C************************************************************
363400940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
363500940131     C************************************************************
363600940201     C     INZD2         BEGSR
363700940131     C*
363800940131     C                   CLEAR                   FMTD2
363900130910C2226C                   EVAL      D2On1 = Xscon
364000130912C2226C                   EVAL      D2Off1= Xscoff
364100021010     C                   EVAL      D2BSVA = Normale
364200950328     C* se sono entrato con chiave univoca
364300950328     C* impedisco il salvataggio valori assunti
364400950606     C     $Unique       IFEQ      *ON
364500021010     C                   EVAL      D2BSVA = Protetto
364600950328     C                   ENDIF
364700940201     C                   MOVEL     $LOP          D2LOP
364800940831     C                   MOVE      $ORD          D2ORD
364900130924C2226C                   If        $pre = ' '
365000130924  "  C                   Movel     *on           $pre
365100130924  "  C                   Movel     'P'           $RgrPre
365200130924  "  C                   Endif
365300130924  "  C                   If        $Pre = *on
365400130912  "  C                   Movel     XscOn         D2Pre
365500130910  "  C                   Else
365600130912  "  C                   Movel     XscOff        D2Pre
365700130920  "  C                   Endif
365800130924C2226C                   Movel     $RgrPre       D2RgrPre
365900090324     c* decodifica
366000090324     C                   EXSR      CTRD2
366100090324     C*
366200090324     C                   MOVE      *ALL'0'       IN5098
366300090324     C                   MOVEA     IN5098        *IN(50)
366400090324     C*
366500940131     C*
366600940131     C                   ENDSR
366700940309     C/EJECT
366800090324     C************************************************************
366900090324     C* GESTIONE F02 VIDEO D1
367000090324     C************************************************************
367100090324     C     F02D2         BEGSR
367200090324     C*
367300090324     C* per decodificare la videata richiamo la routine dei ctrl
367400090324     C* e poi disattivo gli eventuali messaggi di errore
367500090324     C                   EXSR      CTRD2
367600090324     C*
367700090324     C                   MOVE      *ALL'0'       IN5098
367800090324     C                   MOVEA     IN5098        *IN(50)
367900090324     C*
368000090324     C                   ENDSR
368100090324     C/EJECT
368200940309     C************************************************************
368300940309     C* GESTIONE F03 VIDEO D2
368400940309     C************************************************************
368500940309     C     F03D2         BEGSR
368600940309     C*
368700940309     C                   MOVE      *ON           $FINE
368800940325     C* fine programma
368900940325     C                   EXSR      ENDPGM
369000940309     C*
369100940309     C                   ENDSR
369200940309     C/EJECT
369300940309     C************************************************************
369400940309     C* GESTIONE F12 VIDEO D2
369500940309     C************************************************************
369600940309     C     F12D2         BEGSR
369700940309     C*
369800940309     C                   MOVE      $LASTG        $GEST
369900940309     C*
370000940309     C                   ENDSR
370100940916     C/EJECT
370200940916     C************************************************************
370300940916     C* GESTIONE F24 VIDEO D2
370400940916     C************************************************************
370500940916     C     F24D2         BEGSR
370600940916     C*
370700940916     C                   MOVEA     $FX3          $FX
370800940916     C                   MOVEA     $FC3          $FC
370900940916     C                   MOVEA     $FR3          $FR
371000940916     C                   Z-ADD     $ULKD2        $ULK
371100950726     C                   Z-ADD     $D2L01        $L01
371200950726     C                   Z-ADD     $D2L02        $L02
371300940916     C                   EXSR      RIEKEY
371400940916     C                   Z-ADD     $ULK          $ULKD2            3 0
371500940916     C                   MOVEA     $FC           $FC3
371600940916     C                   MOVEL     $KEY1         Z3KE1
371700060224     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
371800090324     C                   MOVEL     $KEY2         Z3KE2
371900940916     C                   WRITE     FMTZ3
372000940916     C*
372100940916     C                   ENDSR
372200940223     C/EJECT
372300940131     C************************************************************
372400940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
372500940131     C************************************************************
372600940131     C     CTRD2         BEGSR
372700940131     C*
372800940131     C                   SETOFF                                       99
372900940131     C*
373000940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
373100940201     C     D2LOP         IFEQ      0
373200940131     C                   SETON                                          5099
373300941027     C                   ENDIF
373400940131     C*
373500940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
373600940131     C     D2ORD         IFEQ      0
373700940131     C                   SETON                                          5199
373800941027     C                   ENDIF
373900090324     c* Controllo ordinamento
374000090324     C                   RESET                   X07DS
374100090324     C                   MOVE      '1'           X07RIC
374200090324     C                   MOVEL     'ANDIZ000'    X07TRC
374300100401     C                   MOVEL     'DSPORDRCA'   X07CAM
374400090324     C                   MOVEL     D2ORD         X07VAL
374500090324     C                   MOVE      *OFF          X07ALL
374600090324     C                   CALLB     'X07VALR'
374700090324     C                   PARM                    X07DS
374800090324   B3C     X07ERR        IFEQ      '0'
374900090324     C                   MOVEL     X07DES        D2ORDD
375000090324     c                   else
375100090324     C                   SETON                                        5299
375200090324     C                   ENDIF
375300130910C2226C* CONTROLLO FLAG PRELISTA
375400130912  "  c                   if        D2Pre <> XscOn and
375500130912  "  c                             D2Pre <> XscOff
375600130910  "  c                   seton                                        5399
375700130912  "  c                   endif
375800130912  "  C* CONTROLLO FLAG Raggruppamento obbligatorio
375900130912  "  c                   if        D2RgrPre = ' ' and
376000130912  "  c                             D2Pre = XscOn
376100130912  "  c                   seton                                        5499
376200130912  "  c                   endif
376300130912  "  c* Controllo values raggruppamento
376400130912  "  C                   RESET                   X07DS
376500130912  "  C                   MOVE      '1'           X07RIC
376600130912  "  C                   MOVEL     'ANDIZ000'    X07TRC
376700130912  "  C                   MOVEL     'DSPRGRSCL'   X07CAM
376800130912  "  C                   MOVEL     D2RGRPre      X07VAL
376900130912  "  C                   MOVE      *OFF          X07ALL
377000130912  "  C                   CALLB     'X07VALR'
377100130912  "  C                   PARM                    X07DS
377200130912  "  C     X07ERR        IFEQ      '0'
377300130912  "  C                   MOVEL     X07DES        D2RgrPreD
377400130912  "  c                   else
377500130912  "  C                   SETON                                        5599
377600130912C2226C                   ENDIF
377700940131     C*
377800940131     C                   ENDSR
377900941205     C/EJECT
378000941205     C************************************************************
378100941205     C* CONTROLLO ABILITAZIONE TASTI
378200941205     C* INPUT
378300941205     C* $TASTO =Tasto funzione premuto
378400941205     C* $FC = Tasti abilitati
378500941205     C* OUTPUT
378600941205     C* IN99 = ERrore
378700941205     C************************************************************
378800941205     C     CTRKEY        BEGSR
378900941205     C*
379000951005     C                   SETOFF                                       99
379100951005     C*
379200941205     C                   SELECT
379300941205     C     $TASTO        WHENEQ    F01
379400960202     C                   z-add     01            posfun
379500941205     C     $FC(1)        IFEQ      '0'
379600951005     C                   SETON                                        9997
379700941205     C                   ENDIF
379800941205     C     $TASTO        WHENEQ    F02
379900960202     C                   z-add     02            posfun
380000941205     C     $FC(2)        IFEQ      '0'
380100951005     C                   SETON                                        9997
380200941205     C                   ENDIF
380300941205     C     $TASTO        WHENEQ    F03
380400960202     C                   z-add     03            posfun
380500941205     C     $FC(3)        IFEQ      '0'
380600951005     C                   SETON                                        9997
380700941205     C                   ENDIF
380800941205     C     $TASTO        WHENEQ    F04
380900960202     C                   z-add     04            posfun
381000941205     C     $FC(4)        IFEQ      '0'
381100951005     C                   SETON                                        9997
381200941205     C                   ENDIF
381300941205     C     $TASTO        WHENEQ    F05
381400960202     C                   z-add     05            posfun
381500941205     C     $FC(5)        IFEQ      '0'
381600951005     C                   SETON                                        9997
381700941205     C                   ENDIF
381800941205     C     $TASTO        WHENEQ    F06
381900960202     C                   z-add     06            posfun
382000941205     C     $FC(6)        IFEQ      '0'
382100951005     C                   SETON                                        9997
382200941205     C                   ENDIF
382300941205     C     $TASTO        WHENEQ    F07
382400960202     C                   z-add     07            posfun
382500941205     C     $FC(7)        IFEQ      '0'
382600951005     C                   SETON                                        9997
382700941205     C                   ENDIF
382800941205     C     $TASTO        WHENEQ    F08
382900960202     C                   z-add     08            posfun
383000941205     C     $FC(8)        IFEQ      '0'
383100951005     C                   SETON                                        9997
383200941205     C                   ENDIF
383300941205     C     $TASTO        WHENEQ    F09
383400960202     C                   z-add     09            posfun
383500941205     C     $FC(09)       IFEQ      '0'
383600951005     C                   SETON                                        9997
383700941205     C                   ENDIF
383800941205     C     $TASTO        WHENEQ    F10
383900960202     C                   z-add     10            posfun
384000941205     C     $FC(10)       IFEQ      '0'
384100951005     C                   SETON                                        9997
384200941205     C                   ENDIF
384300941205     C     $TASTO        WHENEQ    F11
384400960202     C                   z-add     11            posfun
384500941205     C     $FC(11)       IFEQ      '0'
384600951005     C                   SETON                                        9997
384700941205     C                   ENDIF
384800941205     C     $TASTO        WHENEQ    F12
384900960202     C                   z-add     12            posfun
385000941205     C     $FC(12)       IFEQ      '0'
385100951005     C                   SETON                                        9997
385200941205     C                   ENDIF
385300941205     C     $TASTO        WHENEQ    F13
385400960202     C                   z-add     13            posfun
385500941205     C     $FC(13)       IFEQ      '0'
385600951005     C                   SETON                                        9997
385700941205     C                   ENDIF
385800941205     C     $TASTO        WHENEQ    F14
385900960202     C                   z-add     14            posfun
386000941205     C     $FC(14)       IFEQ      '0'
386100951005     C                   SETON                                        9997
386200941205     C                   ENDIF
386300941205     C     $TASTO        WHENEQ    F15
386400960202     C                   z-add     15            posfun
386500941205     C     $FC(15)       IFEQ      '0'
386600951005     C                   SETON                                        9997
386700941205     C                   ENDIF
386800941205     C     $TASTO        WHENEQ    F16
386900960202     C                   z-add     16            posfun
387000941205     C     $FC(16)       IFEQ      '0'
387100951005     C                   SETON                                        9997
387200941205     C                   ENDIF
387300941205     C     $TASTO        WHENEQ    F17
387400960202     C                   z-add     17            posfun
387500941205     C     $FC(17)       IFEQ      '0'
387600951005     C                   SETON                                        9997
387700941205     C                   ENDIF
387800941205     C     $TASTO        WHENEQ    F18
387900960202     C                   z-add     18            posfun
388000941205     C     $FC(18)       IFEQ      '0'
388100951005     C                   SETON                                        9997
388200941205     C                   ENDIF
388300941205     C     $TASTO        WHENEQ    F19
388400960202     C                   z-add     19            posfun
388500941205     C     $FC(19)       IFEQ      '0'
388600951005     C                   SETON                                        9997
388700941205     C                   ENDIF
388800941205     C     $TASTO        WHENEQ    F20
388900960202     C                   z-add     20            posfun
389000941205     C     $FC(20)       IFEQ      '0'
389100951005     C                   SETON                                        9997
389200941205     C                   ENDIF
389300941205     C     $TASTO        WHENEQ    F21
389400960202     C                   z-add     21            posfun
389500941205     C     $FC(21)       IFEQ      '0'
389600951005     C                   SETON                                        9997
389700941205     C                   ENDIF
389800941205     C     $TASTO        WHENEQ    F22
389900960202     C                   z-add     22            posfun
390000941205     C     $FC(22)       IFEQ      '0'
390100951005     C                   SETON                                        9997
390200941205     C                   ENDIF
390300941205     C     $TASTO        WHENEQ    F23
390400960202     C                   z-add     23            posfun
390500941205     C     $FC(23)       IFEQ      '0'
390600951005     C                   SETON                                        9997
390700941205     C                   ENDIF
390800941205     C     $TASTO        WHENEQ    F24
390900960202     C                   z-add     24            posfun
391000941205     C     $FC(24)       IFEQ      '0'
391100951005     C                   SETON                                        9997
391200941205     C                   ENDIF
391300941205     C                   ENDSL
391400941205     C*
391500941205     C                   ENDSR
391600940223     C/EJECT
391700940201     C************************************************************
391800940201     C* AGGIORNAMENTO VALORI ASSUNTI
391900940201     C************************************************************
392000940201     C     AGGMPE        BEGSR
392100940201     C*
392200950524     C* preparo i parametri di chiamata al driver
392300950524     C                   CLEAR                   XMPEDS
392400950524     C                   EVAL      TpChiamata = '03'
392500950524     C                   EVAL      NChiavi = 5
392600950524     C                   EVAL      VLogica = 2
392700950524     C*
392800950524     C* preparo le chiavi per la chiamata al driver
392900950524     C                   CLEAR                   XMPEDS1
393000950524     C                   MOVEL     XSCSOC        DSMPESOC
393100950524     C                   MOVEL     DSPGM         DSMPEPGM
393200950524     C                   MOVEL     KNMUS         DSMPEPRF
393300941013     C*
393400950322     C* Si memorizzano i dati di default a seconda delle modalità con
393500950322     C* cui il pgm è usato
393600950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
393700950322     C* MPETIP = 'PV'  pgm richiamato da menù
393800950322     C* MPETIP = 'PB'  x lancio Batch
393900950322     C*
394000941013     C                   SELECT
394100090323    >C     OPZL76        WHENEQ    '01'
394200950524     C                   MOVEL     'PX'          DSMPETIP
394300941013     C                   OTHER
394400950524     C                   MOVEL     'PV'          DSMPETIP
394500941013     C                   ENDSL
394600950524     C* dati
394700950524     C                   MOVEL     DSMPE         DSMPEPAR
394800950524     C*
394900950524     C                   CALLB     'XMPE'
395000950524     C                   PARM                    XMPEDS
395100950524     C                   PARM                    XMPEDS1
395200940201     C*
395300940201     C                   ENDSR
395400940223     C/EJECT
395500940131     C************************************************************
395600940131     C* OPERAZIONI INIZIALI
395700940131     C************************************************************
395800940131     C     *INZSR        BEGSR
395900070711     C     *DMY          Move      Udate         ZDateISO
396000940131     C*
396100940127     C* Reperimento parametri
396200940127     C*
396300940117     C     *ENTRY        PLIST
396400940117     C                   PARM                    KPJBA
396500950410     C                   PARM                    DSAUT
396600950410     C*
396700951010     C* testo se sono stati passati i dati di autorizzazione
396800951010     C*
396900950410     C     PARMS         IFLT      2
397000950410     C                   MOVE      *BLANK        AUTDS
397100950410     C                   ELSE
397200950410     C                   MOVEL     DSAUT         AUTDS
397300950410     C                   ENDIF
397400951206     C*
397500951206     C* Reperimento dati società
397600951206     C*
397700970424     C                   MOVEL     'SOC001'      TIPXSC
397800970424     C                   MOVEL     *BLANK        SOCXSC
397900951206     C                   EXSR      REPSOC
398000960409     C*
398100960409     C* Controlli dati società
398200960409     C*
398300960409     C     RTNXSC        IFNE      '1'
398400970424     C                   MOVEL     XSOCDS        SOC001
398500960409     C                   ELSE
398600960409     C                   MOVE      *ON           $FINE
398700090323    >C                   MOVE      '1'           ErrL76
398800960409     C                   EXSR      ENDPGM
398900960409     C                   ENDIF
399000070725     c*
399100090323     c                   open      ndcL76d                              13
399200070725     c* emetto messaggio per video non a 132
399300070725     c                   if        *in13=*on
399400070725     c                   move      'COS2651'     XmsgCd1
399500070725     C                   CALL      'XMSGR'
399600070725     C                   PARM                    XMSGDS
399700070725     C                   PARM                    XMStxt          135
399800070725     C                   PARM                    XMSGinz           1
399900070725     C                   PARM                    XMSGCd1           7
400000070725     C                   PARM                    XMSGCd2           7
400100070725     C                   PARM                    XMSGCd3           7
400200070725     C                   PARM                    XMSGSn1           7
400300070725     C                   PARM                    XMSGCdTxT        63
400400070725     C                   PARM                    MSGCdTit         14
400500070725     C                   PARM                    MSGClPrtf         1
400600070725     C                   PARM                    VarMsg1         100
400700070725     C                   PARM                    VarMsg2         100
400800070725     C                   PARM                    VarMsg3         100
400900090323    >C                   MOVE      '1'           ErrL76
401000070725     c                   exsr      endpgm
401100070725     c                   endif
401200950726     C*
401300950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
401400070618     C                   EVAL      $D1L01 = %Size(Z1Ke)
401500950726     C                   EVAL      $D1L02 = %Size(Z1Ke2)
401600070618     C                   EVAL      $S1L01 = %Size(Z2Ke)
401700950726     C                   EVAL      $S1L02 = %Size(Z2Ke2)
401800070531     C                   EVAL      $C1Op1 = %Size(C1Opz)
401900950926     C                   EVAL      $C1Op2 = %Size(C1Opz2)
402000950906     C                   EVAL      $D2L01 = %Size(Z3Ke1)
402100090324     C                   EVAL      $D2L02 = %size(z3ke2)
402200130911C2226C                   EVAL      $S2L01 = %Size(Z2Ke)
402300130911  "  C                   EVAL      $S2L02 = %Size(Z2Ke2)
402400130911  "  C                   EVAL      $C2Op1 = %Size(C2Opz)
402500130911C2226C                   EVAL      $C2Op2 = %Size(C2Opz2)
402600940225     C*
402700940223     C* Reperimento opzioni
402800940127     C*
402900940128     C                   EXSR      REPOPZ
403000950406     C*
403100940506     C* Reperimento tasti di funzione
403200940506     C*
403300940506     C                   EXSR      REPKEY
403400960117     C* Chiamata a routine per il reperimento delle opzioni
403500960117     C* personalizzate
403600960117     C                   CAllb     'X46USREXT'
403700960117     C                   PARM                    $OP1
403800960117     C                   PARM                    $TX1
403900960117     C                   PARM                    $FL1
404000960117     C                   PARM                    $PGO1
404100960117     C                   PARM      $dimOp        NumOpz
404200960202     C                   PARM                    $FX2
404300960202     C                   PARM                    $FC2
404400960202     C                   PARM                    $PGF2
404500960117     C                   PARM                    DsPgm
404600960117     C                   PARM                    xsccut
404700960117     C                   PARM                    xscgrp
404800960117     C*
404900960118     C                   Z-add     1             OPZLEN
405000960118     C     *blank        Lookup    $TX1(opzlen)                           21
405100960117     C                   if        *in21 = *on
405200960117     C                   sub       1             OPZLEN
405300960117     C                   else
405400960117     C                   Z-add     $DimOp        OPZLEN
405500960117     C                   endif
405600940127     C*
405700940117     C                   ENDSR
405800940223     C/EJECT
405900940223     C************************************************************
406000940223     C* INIZIALIZZAZIONE VARIABILI
406100940223     C************************************************************
406200940223     C     INZVAR        BEGSR
406300941215     C*
406400941215     C* Dati da PGM chiamante
406500941215     C*
406600090323    >C                   MOVEL     KPJBU         NdcL76DS
406700951206     C*
406800951206     C* Se società diversa da quella impostata nella *inzsr, rieseguo
406900951206     C* reperimento dati società
407000090323    >C                   IF        SocietaL76 <> xscsoc  and
407100090323    >C                             OpzL76 <>*blank
407200960119     C                   MOVEL     'SOC001'      TIPXSC
407300090323    >C                   MOVEL     SocietaL76    SOCXSC
407400951122     C                   EXSR      REPSOC
407500951010     C     RTNXSC        IFNE      '1'
407600970424     C                   MOVEL     XSOCDS        SOC001
407700951010     C                   ELSE
407800951010     C                   MOVE      *ON           $FINE
407900090323    >C                   MOVE      '1'           ErrL76
408000951010     C                   EXSR      ENDPGM
408100951010     C                   ENDIF
408200960410     C                   ENDIF
408300090325     C*
408400090325     C* Reperimento autorizzazioni
408500090325     C*
408600090325     C* le autorizzazioni potrebbero essere già state passate,
408700090325     C* nel qual caso non vanno reperite
408800090325    >C     AUTDS         IFEQ      *BLANK
408900090325    >C                   EXSR      REPAUT
409000090325    >C                   ELSE
409100100409    >C                   MOVEL     XCADDS        CONRECDS
409200090325     C* Se non autorizzati alla funzione oppure                              ed esco
409300100409    >C     VisATT        IFNE      *ON
409400090325     C                   MOVE      XCAFNC        XNOFNC
409500090325     C                   MOVE      XCAPRF        XNOPRF
409600090325     C                   MOVE      XCACUT        XNOCUT
409700090325     C                   MOVE      XCATCK        XNOTCK
409800090325     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
409900090325     C                   CALL      'XNOAUTR'
410000090325     C                   PARM                    XNOADS
410100090325    >C                   MOVE      '2'           ErrL76
410200090325     C                   MOVE      *ON           $fine
410300090325     C                   EXSR      ENDPGM
410400090325     C                   ENDIF
410500090325    >C                   ENDIF
410600090324     C*
410700090324     c* reperimento dati parametro modulo solleciti
410800090324     C                   movel     'NDCL1S'      xpapar
410900090324     C                   movel     *blank        xparic
411000090324     C                   CALLb     'XPAR'
411100090324     C                   PARM                    xscsoc
411200090324     C                   PARM                    XPAPAR            8
411300090324     C                   PARM                    XPAOUT
411400090324     C                   PARM                    XPAERR            1
411500090324     C                   PARM                    XPARIC            1
411600090324     C     XPAERR        IFEQ      '0'
411700090324     C                   movel(p)  XpaOut        Anp057Ds
411800090324     c                   else
411900090324     c                   seton                                        lr
412000090324     c                   return
412100090324     C                   endif
412200941026     C*
412300941026     C* Reperimento valori default utente
412400941026     C*
412500941026     C                   EXSR      REPDFT
412600940506     C*
412700940506     C* Definizione delle opzioni utilizzabili a pgm
412800940506     C*
412900940506     C                   EXSR      DEFOPZ
413000950330     C*
413100940223     C* Variabili per gestione videate
413200940223     C*
413300940223     C                   MOVE      *OFF          $FINE                          fine pgm
413400940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
413500940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
413600130911C2226C                   MOVE      *OFF          $INZS2                         inizializzione S1
413700940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
413800950606     C                   MOVE      *OFF          $EndFile                       memo fine file
413900130911C2226C                   MOVE      *OFF          $EndFilePre                    memo fine file
414000940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
414100940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
414200130911C2226C                   MOVE      *OFF          $RCDOKPre                      rcd letto valido
414300940509     C                   MOVE      *HIVAL        $LASTV
414400940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
414500940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
414600940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
414700130911C2226C                   Z-ADD     0             $ULKS2            3 0          piede da emett. S1
414800960605     C* azzero la posizione del cursore
414900960605     C                   Z-ADD     0             H1RIGA
415000960605     C                   Z-ADD     0             H1COLO
415100940223     C*
415200940318     C                   MOVE      *ON           $INZD1
415300940318     C                   MOVE      *ON           $INZS1
415400130911C2226C                   MOVE      *ON           $INZS2
415500940831     C*
415600940831     C* se pgm richiamato con chiave univoca impostata,
415700940831     C* forzo la emissione della lista e imposto l'ordinamento
415800090323    >C*    OpzL76        IFNE      *BLANK
415900070524    >C*    UQ1001        IFNE      *BLANK
416000070524     C*                  MOVE      *ON           $Unique
416100070524     C*                  Z-ADD     2             $LOP
416200070524     C*                  Z-ADD     99            $ORD
416300070524     C*                  ELSE
416400070524     C*                  MOVE      *OFF          $Unique
416500070524     C*                  ENDIF
416600070524     C*                  ENDIF
416700950330     C*
416800950330     C* Variabili appoggio
416900950330     C*
417000950330     C                   Z-ADD     SFLPAG        WPAG
417100130924C2226C                   Z-ADD     SFLPAG        WPAG2
417200950330     C                   MOVEL     *OFF          WOPR
417300950515     C                   CLEAR                   W1POS
417400941104     C*
417500941104     C* definisco qual'è la prima videata
417600950410     C* (se la prima videata risulta la lista, devo comunque caricare
417700950410     C*  la videata di parzializzazione e gestire la definizione
417800950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
417900950410     C*  passati dei dati, può variare il caricamento)
418000940223     C                   SELECT
418100940223     C     $LOP          WHENEQ    1
418200940223     C                   MOVE      'D1'          $GEST
418300940223     C     $LOP          WHENEQ    2
418400130912C2226C                   IF        $Pre = *on
418500130911  "  C                   MOVE      'S2'          $GEST
418600130911C2226C                   Else
418700940223     C                   MOVE      'S1'          $GEST
418800130911C2226C                   Endif
418900941104     C                   EXSR      INZD1
419000090325     C                   EXSR      CTRD1
419100090325     C                   MOVE      *ALL'0'       IN5098
419200090325     C                   MOVEA     IN5098        *IN(50)
419300941104     C                   MOVE      *OFF          $INZD1
419400950410     C                   EXSR      DEFORD
419500941104     C                   ENDSL
419600940223     C*
419700940223     C* Valorizzazione campi univoci testate
419800940223     C*
419900940223     C                   CLEAR                   FMTT1
420000940223     C                   MOVEL     KNSIF         NOMSIF
420100941109     C                   MOVEL     XSCDSI        NOMDIT
420200941027     C                   MOVEL     DSPGM         NOMPGM
420300950331     C* recupero il sottotitolo (se c'è)
420400950331     C                   MOVEL     *BLANK        T1TIT
420500950331     C     SOTTIT        IFNE      *BLANK
420600950331     C                   MOVEL     SOTTIT        IDMSG
420700950522     C                   CALLB     'XRTVM'
420800950331     C                   PARM                    IDMSG            27
420900950331     C                   PARM                    TXTMSG          100
421000950331     C                   PARM                    ERRMSG            1
421100950331     C                   PARM      *ON           CTRMSG            1            centratura
421200950331     C                   PARM      30            LENMSG            3 0          lun output.
421300950331     C     ERRMSG        IFNE      *ON
421400950331     C                   MOVEL     TXTMSG        T1TIT
421500950331     C                   ELSE
421600950331     C                   MOVEL     *ALL'?'       T1TIT
421700950331     C                   ENDIF
421800950331     C                   ENDIF
421900940223     C*
422000940223     C                   ENDSR
422100941209     C/EJECT
422200070525     C************************************************************
422300070525     C* Composizione stringa sql
422400070525     C************************************************************
422500070525     C     InzSql        BEGSR
422600130912     C*
422700100407     C                   EVAL      SQ='Select distinct RcaSocieta,           +
422800100407     C                                 RcaPosiz, RcaAttrib, Rcakcc, Rcaksc,  +
422900130912     C                                 RcaDtPar, RcaNrPar, RcaSerPar, RcaDivisa+
423000130912     C                                 ,RcaKscAge from NDRCA00F'
423100130912C2226C                   If        $Pre = *on
423200130911  "  C                   Eval      SQ=%trimr(SQ) + ',ANRCO00F,ANSOG00F +
423300130911  "  C                             Where RcaKcc=RcoKcc and RcaKsc = RcoKsc and +
423400130911  "  C                             RcoSogg = SogSogg and '
423500130918  "  C                   If        $RgrPre = 'P'
423600130911  "  C                   Cat       'SogPartiva':1Sq
423700130911  "  C                   Cat       '='         :1Sq
423800130911  "  C                   Cat       ''''        :0Sq
423900130911  "  C                   Cat       H2Partiva   :0Sq
424000130911  "  C                   Cat       ''''        :0Sq
424100130911  "  C                   Cat       'and'       :1Sq
424200130911  "  C                   Cat       'SogCdFisc' :1Sq
424300130911  "  C                   Cat       '='         :1Sq
424400130911  "  C                   Cat       ''''        :0Sq
424500130911  "  C                   Cat       H2CdFisc    :0Sq
424600130912  "  C                   Cat       ''''        :0Sq
424700130912  "  C                   Cat       'and'       :1Sq
424800130912  "  C                   Else
424900130912  "  C                   Cat       'SogSogg'   :1Sq
425000130912  "  C                   Cat       '='         :1Sq
425100130912  "  C                   Cat       ''''        :0Sq
425200130912  "  C                   Cat       H2Sogg      :0Sq
425300130912  "  C                   Cat       ''''        :0Sq
425400130912  "  C                   Cat       'and'       :1Sq
425500130912  "  C                   Endif
425600130912C2226C                   Else
425700130912     C*
425800070525     C                   Cat       'Where'     :1Sq
425900130911C2226C                   Endif
426000100401     c* CAMPI OBBLIGATORI
426100070525     c* Societa
426200100401     C                   Cat       'Rcasocieta':1Sq
426300100401     C                   Cat       '='         :1Sq
426400100401     C                   Cat       ''''        :0Sq
426500070525     C                   Cat       xscsoc      :0Sq
426600100401     C                   Cat       ''''        :0Sq
426700130910C2226X*** unita (se gestita dai solleciti)
426800130910C2226C* unita (se gestita nella attività di recupero credito)
426900100401     c                   if        D1Unita<> *blanks
427000100401     C                   Cat       'and'       :1Sq
427100100401     C                   Cat       'RcaUnita'  :1Sq
427200100401     C                   Cat       '='         :1Sq
427300100401     C                   Cat       ''''        :0Sq
427400100401     C                   Cat       D1Unita     :0Sq
427500100401     C                   Cat       ''''        :0Sq
427600130910C2226C                   Else
427700130912  "  C                   if        GesuniR057='D'
427800130910  "  C                   Cat       'and'       :1Sq
427900130910  "  C                   Cat       'RcaUnita'  :1Sq
428000130910  "  C                   If        WinUni=' '
428100130910  "  C                   Cat       '='         :1Sq
428200130910  "  C                   Cat       ''''        :0Sq
428300130910  "  C                   Cat       ''''        :0Sq
428400130910  "  C                   Else
428500130910  "  C                   Cat       WinUni      :1Sq
428600130910  "  C                   Endif
428700130910C2226C                   Endif
428800130911     C                   endif
428900100401     c* capoconto
429000100407     c                   if        D1kcc  <> *blanks
429100100401     C                   Cat       'and'       :1Sq
429200100401     C                   Cat       'RcaKcc'    :1Sq
429300100401     C                   Cat       '='         :1Sq
429400100401     C                   Cat       ''''        :0Sq
429500100401     C                   Cat       D1kcc       :0Sq
429600100401     C                   Cat       ''''        :0Sq
429700130911C2226C                   else
429800130912  "  C                   If        $Pre = *on
429900130911  "  C                   Cat       'and'       :1Sq
430000130911  "  C                   Cat       'RcaKcc'    :1Sq
430100130911  "  C                   Cat       '='         :1Sq
430200130911  "  C                   Cat       ''''        :0Sq
430300130911  "  C                   Cat       H2kcc       :0Sq
430400130911  "  C                   Cat       ''''        :0Sq
430500130911C2226C                   endif
430600130911     C                   endif
430700100401     c*
430800100401     c* posizione attributo = a quella del parametro mod. solleciti
430900100401     C                   Cat       'and'       :1Sq
431000100401     C                   Cat       'RcaPosiz'  :1Sq
431100100401     C                   Cat       '='         :1Sq
431200100401     C                   Cat       ''''        :0Sq
431300100401     C                   Cat       PosAttr057  :0Sq
431400100401     C                   Cat       ''''        :0Sq
431500100401     c* CAMPI FACOLTATIVI
431600100401     c* attributo
431700100401     c                   if        D1attrib <> *blanks
431800100401     C                   Cat       'and'       :1Sq
431900100401     C                   Cat       'RcaAttrib' :1Sq
432000100401     C                   Cat       '='         :1Sq
432100100401     C                   Cat       ''''        :0Sq
432200100401     C                   Cat       d1Attrib    :0Sq
432300100401     C                   Cat       ''''        :0Sq
432400100401     c                   endif
432500100401     c* ksc
432600100401     c                   if        D1ksc <> *blanks
432700100401     C                   Cat       'and'       :1Sq
432800100401     C                   Cat       'Rcaksc'    :1Sq
432900100401     C                   Cat       '='         :1Sq
433000100401     C                   Cat       ''''        :0Sq
433100100401     C                   Cat       d1ksc       :0Sq
433200100401     C                   Cat       ''''        :0Sq
433300100401     c                   endif
433400100401     c* agente
433500100401     c                   if        D1kscAge <> *blanks
433600090409     C                   Cat       'and'       :1Sq
433700100401     C                   Cat       'RcakscAge' :1Sq
433800090409     C                   Cat       '='         :1Sq
433900100401     C                   Cat       ''''        :0Sq
434000100401     C                   Cat       d1kscage    :0Sq
434100100401     C                   Cat       ''''        :0Sq
434200090409     c                   endif
434300100512     c* parzializzazione fasi
434400100512     C                   Cat       'and'       :1Sq
434500100512     C                   Cat       'RcaUteChiu':1Sq
434600100512     c* aperte
434700100512     c                   if        D1FasiApe=xscon
434800100512     C                   Cat       '='         :1Sq
434900100512     C                   Cat       ''''        :0Sq
435000100512     C                   Cat       ' '         :0Sq
435100100512     C                   Cat       ''''        :0Sq
435200100512     c                   else
435300100512     C                   Cat       '<>'        :1Sq
435400100512     C                   Cat       ''''        :0Sq
435500100512     C                   Cat       ' '         :0Sq
435600100512     C                   Cat       ''''        :0Sq
435700100512     c                   endif
435800100413     c* parzializzazione partite
435900100413     C                   Cat       'and'       :1Sq
436000100413     C                   Cat       'RcaStato'  :1Sq
436100100413     c* aperte
436200100413     c                   if        D1Stato=xscon
436300100413     C                   Cat       '<>'        :1Sq
436400100413     C                   Cat       ''''        :0Sq
436500100413     C                   Cat       '2'         :0Sq
436600100413     C                   Cat       ''''        :0Sq
436700100413     c                   else
436800100413     C                   Cat       '='         :1Sq
436900100413     C                   Cat       ''''        :0Sq
437000100413     C                   Cat       '2'         :0Sq
437100100413     C                   Cat       ''''        :0Sq
437200100413     c                   endif
437300100407     c*
437400100420     c* parzializzazione data partita
437500100420     c                   exsr      ParzDtPar
437600100401     c* parzializzazione data scadenza
437700100401     c                   exsr      ParzDtSca
437800100416     c* parzializzazione data sollecito
437900100416     c                   exsr      ParzDtSol
438000100401     c* parzializzazione fase
438100100401     c                   if        D1FaseDa <> *blanks
438200100401     c                   move      D1FaseDa      FaseDa            1
438300100401     c                   move      D1FaseA       FaseA             1
438400100401     C                   Cat       'and'       :1Sq
438500100401     C                   Cat       'RcaFase'   :1Sq
438600100407     C                   Cat       'between'   :1Sq
438700100401     C                   Cat       FaseDa      :1Sq
438800100401     C                   Cat       'and'       :1Sq
438900100401     C                   Cat       FaseA       :1Sq
439000100401     c                   endif
439100100407     c* parzializzazione azione
439200100407     c                   if        D1Azione <> *blanks
439300100407     C                   Cat       'and'       :1Sq
439400100407     C                   Cat       'RcaAzione' :1Sq
439500100407     C                   Cat       '='         :1Sq
439600100407     C                   Cat       ''''        :0Sq
439700100407     C                   Cat       d1Azione    :0Sq
439800100407     C                   Cat       ''''        :0Sq
439900100407     c                   endif
440000131001BRT  C* esclusione impegni (98)
440100131001 "   c                   if        d1EsclImp = xscon
440200131001 "   C                   Cat       'and'       :1Sq
440300131001 "   C                   Cat       'RcaAzione' :1Sq
440400131001 "   C                   Cat       '<>'        :0Sq
440500131001 "   C                   Cat       '''98'''    :0Sq
440600131001BRT  C                   endif
440700131001BRT  C* parzializzazione fasi aperte BRT
440800131001BRT  C                   exsr      ParzFasiBRT
440900100401     c* parzializzazione destinatario
441000100401     c                   exsr      ParzDest
441100100401     c*
441200100401     c* parzializzazione data attività
441300100401     c                   exsr      ParzDtAtt
441400100401     C* ORDINAMENTO
441500100401     c                   cat       'Order by'  :1Sq
441600100401     c                   select
441700100401     c                   when      $ord = 1
441800100402     c                   cat       'RcaKsc,'   :1Sq
441900100402     c                   cat       'RcaDtPar'  :1Sq
442000100401     c                   when      $ord = 2
442100100402     c                   cat       'RcaDtPar,' :1Sq
442200100402     c                   cat       'RcaKsc'    :1Sq
442300100401     c                   when      $ord = 3
442400100402     c                   cat       'RcaKscAge,':1Sq
442500100401     c                   cat       'RcaKsc'    :1Sq
442600100401     c                   endsl
442700070525     C*
442800070525     C* Esecuzione SQL
442900070525     C                   Exsr      ExeSQL
443000070525     C*
443100070525     C                   ENDSR
443200070525     C/EJECT
443300130910C2226C************************************************************
443400130910  "  C* Composizione stringa sql di prelista
443500130910  "  C************************************************************
443600130910C2226C     InzSqlPre     BEGSR
443700130918     C* Selezione
443800130919     C                   EVAL      SQ='Select +
443900130918     C                                 RcaSocieta,RcaUnita,RcaKcc,SogCdFisc,+
444000130919     C                                 SogPartIva,SogSogg,Min(SogDes)  +
444100130918     C                                 from NDRCA00F,ANRCO00F,ANSOG00F'
444200130912     C*
444300130912     C                   Cat       'Where'     :1Sq
444400130912     C* CAMPI OBBLIGATORI
444500130912     C* Societa
444600130918     C                   Cat       'RcaSocieta':1Sq
444700130912     C                   Cat       '='         :1Sq
444800130912     C                   Cat       ''''        :0Sq
444900130912     C                   Cat       xscsoc      :0Sq
445000130912     C                   Cat       ''''        :0Sq
445100130912     C* unita (se gestita nella attività di recupero credito)
445200130912     C                   if        D1Unita<> *blanks
445300130918     C                   Cat       'and'       :1Sq
445400130918     C                   Cat       'RcaUnita'  :1Sq
445500130918     C                   Cat       '='         :1Sq
445600130918     C                   Cat       ''''        :0Sq
445700130918     C                   Cat       D1Unita     :0Sq
445800130918     C                   Cat       ''''        :0Sq
445900130910     C                   Else
446000130918     C* unita se gestita ma vuota, include tutte le unità
446100130910     C* abilitate per il profilo.
446200130910     C                   if        GesuniR057='D'
446300130910     C                   Cat       'and'       :1Sq
446400130918     C                   Cat       'RcaUnita'  :1Sq
446500130910     C                   If        WinUni=' '
446600130910     C                   Cat       '='         :1Sq
446700130910     C                   Cat       ''''        :0Sq
446800130910     C                   Cat       ''''        :0Sq
446900130910     C                   Else
447000130910     C                   Cat       WinUni      :1Sq
447100130910     C                   Endif
447200130910     C                   Endif
447300130912     C                   Endif
447400130912     C* capoconto
447500130912     C                   if        D1kcc  <> *blanks
447600130912     C                   Cat       'and'       :1Sq
447700130918     C                   Cat       'RcaKcc'    :1Sq
447800130912     C                   Cat       '='         :1Sq
447900130912     C                   Cat       ''''        :0Sq
448000130912     C                   Cat       D1kcc       :0Sq
448100130912     C                   Cat       ''''        :0Sq
448200130912     C                   endif
448300130918     c* posizione attributo = a quella del parametro mod. solleciti
448400130918     C                   Cat       'and'       :1Sq
448500130918     C                   Cat       'RcaPosiz'  :1Sq
448600130918     C                   Cat       '='         :1Sq
448700130918     C                   Cat       ''''        :0Sq
448800130918     C                   Cat       PosAttr057  :0Sq
448900130918     C                   Cat       ''''        :0Sq
449000130912     C*
449100130912     C* join con i file anagrafici(anrco00f/ansog00f)
449200130912     C* per reperire la partita Iva
449300130910     C                   Cat       'and'       :1Sq
449400130918     C                   Cat       'RcaSocieta':1Sq
449500130910     C                   Cat       '='         :1Sq
449600130910     C                   Cat       'RcoSocieta':0Sq
449700130910     C*
449800130910     C                   Cat       'and'       :1Sq
449900130918     C                   Cat       'RcaKcc'    :1Sq
450000130910     C                   Cat       '='         :1Sq
450100130910     C                   Cat       'RcoKcc'    :0Sq
450200130910     C*
450300130910     C                   Cat       'and'       :1Sq
450400130918     C                   Cat       'RcaKsc'    :1Sq
450500130910     C                   Cat       '='         :1Sq
450600130918     C                   Cat       'RcoKsc'    :1Sq
450700130910     C*
450800130910     C                   Cat       'and'       :1Sq
450900130910     C                   Cat       'RcoSogg'   :1Sq
451000130910     C                   Cat       '='         :1Sq
451100130910     C                   Cat       'SogSogg'   :1Sq
451200130918     C* CAMPI FACOLTATIVI
451300130918     C* attributo
451400130918     C                   if        D1attrib <> *blanks
451500130918     C                   Cat       'and'       :1Sq
451600130918     C                   Cat       'RcaAttrib' :1Sq
451700130918     C                   Cat       '='         :1Sq
451800130918     C                   Cat       ''''        :0Sq
451900130918     C                   Cat       d1Attrib    :0Sq
452000130918     C                   Cat       ''''        :0Sq
452100130918     C                   endif
452200130918     C* ksc
452300130918     C                   if        D1ksc <> *blanks
452400130918     C                   Cat       'and'       :1Sq
452500130918     C                   Cat       'Rcaksc'    :1Sq
452600130918     C                   Cat       '='         :1Sq
452700130918     C                   Cat       ''''        :0Sq
452800130918     C                   Cat       d1ksc       :0Sq
452900130918     C                   Cat       ''''        :0Sq
453000130918     C                   endif
453100130918     C* agente
453200130918     C                   if        D1kscAge <> *blanks
453300130918     C                   Cat       'and'       :1Sq
453400130918     C                   Cat       'RcakscAge' :1Sq
453500130918     C                   Cat       '='         :1Sq
453600130918     C                   Cat       ''''        :0Sq
453700130918     C                   Cat       d1kscage    :0Sq
453800130918     C                   Cat       ''''        :0Sq
453900130918     C                   endif
454000130918     C* parzializzazione fasi
454100130918     C                   Cat       'and'       :1Sq
454200130918     C                   Cat       'RcaUteChiu':1Sq
454300130918     C* aperte
454400130918     c                   if        D1FasiApe=xscon
454500130918     C                   Cat       '='         :1Sq
454600130918     C                   Cat       ''''        :0Sq
454700130918     C                   Cat       ' '         :0Sq
454800130918     C                   Cat       ''''        :0Sq
454900130918     C                   else
455000130918     C                   Cat       '<>'        :1Sq
455100130918     C                   Cat       ''''        :0Sq
455200130918     C                   Cat       ' '         :0Sq
455300130918     C                   Cat       ''''        :0Sq
455400130918     C                   endif
455500130918     C* parzializzazione partite
455600130918     C                   Cat       'and'       :1Sq
455700130918     C                   Cat       'RcaStato'  :1Sq
455800130918     C* aperte
455900130918     C                   if        D1Stato=xscon
456000130918     C                   Cat       '<>'        :1Sq
456100130918     C                   Cat       ''''        :0Sq
456200130918     C                   Cat       '2'         :0Sq
456300130918     C                   Cat       ''''        :0Sq
456400130918     C                   else
456500130918     C                   Cat       '='         :1Sq
456600130918     C                   Cat       ''''        :0Sq
456700130918     C                   Cat       '2'         :0Sq
456800130918     C                   Cat       ''''        :0Sq
456900130919     C                   endif
457000130918     C* parzializzazione data partita
457100130918     C                   exsr      ParzDtPar
457200130918     C* parzializzazione data scadenza
457300130918     C                   exsr      ParzDtSca
457400130918     C* parzializzazione data sollecito
457500130918     C                   exsr      ParzDtSol
457600130918     C* parzializzazione fase
457700130918     C                   if        D1FaseDa <> *blanks
457800130918     C                   move      D1FaseDa      FaseDa            1
457900130918     C                   move      D1FaseA       FaseA             1
458000130918     C                   Cat       'and'       :1Sq
458100130918     C                   Cat       'RcaFase'   :1Sq
458200130918     C                   Cat       'between'   :1Sq
458300130918     C                   Cat       FaseDa      :1Sq
458400130918     C                   Cat       'and'       :1Sq
458500130918     C                   Cat       FaseA       :1Sq
458600130918     C                   endif
458700130918     C* parzializzazione azione
458800130918     c                   if        D1Azione <> *blanks
458900130918     C                   Cat       'and'       :1Sq
459000130918     C                   Cat       'RcaAzione' :1Sq
459100130918     C                   Cat       '='         :1Sq
459200130918     C                   Cat       ''''        :0Sq
459300130918     C                   Cat       d1Azione    :0Sq
459400130918     C                   Cat       ''''        :0Sq
459500130918     C                   endif
459600131001BRT  C* esclusione impegni (98)
459700131001 "   c                   if        d1EsclImp = xscon
459800131001 "   C                   Cat       'and'       :1Sq
459900131001 "   C                   Cat       'RcaAzione' :1Sq
460000131001 "   C                   Cat       '<>'        :0Sq
460100131001 "   C                   Cat       '''98'''    :0Sq
460200131001BRT  C                   endif
460300131001BRT  C* parzializzazione fasi aperte BRT
460400131001BRT  C                   exsr      ParzFasiBRT
460500130918     C* parzializzazione destinatario
460600130918     C                   exsr      ParzDest
460700130918     C*
460800130918     C* parzializzazione data attività
460900130918     C                   exsr      ParzDtAtt
461000130919     C* Raggruppamento
461100130919     C                   cat       'Group by'  :1Sq
461200130919     C                   cat       'RcaSocieta':1Sq
461300130919     C                   cat       ','         :0Sq
461400130919     C                   cat       'RcaUnita'  :0Sq
461500130919     C                   cat       ','         :0Sq
461600130919     C                   cat       'RcaKcc'    :0Sq
461700130919     C                   cat       ','         :0Sq
461800130919     C                   cat       'SogCdFisc' :0Sq
461900130919     C                   cat       ','         :0Sq
462000130919     C                   cat       'SogPartIva':0Sq
462100130919     C                   cat       ','         :0Sq
462200130919     C                   cat       'SogSogg'   :0Sq
462300130919     C* ordinamento
462400130919     C                   cat       'Order by'  :1Sq
462500130919     C                   if        $RgrPre = 'P'
462600130919     C                   cat       'RcaKcc'    :1Sq
462700130919     C                   cat       ','         :0Sq
462800130919     C                   cat       'SogCdFisc' :0Sq
462900130919     C                   cat       ','         :0Sq
463000130919     C                   cat       'SogPartIva':0Sq
463100130919     C                   Else
463200130919     C                   cat       'RCaKcc'    :1Sq
463300130919     C                   cat       ','         :0Sq
463400130919     C                   cat       'SogSogg'   :0Sq
463500130919     C                   Endif
463600130912     C*
463700130910     C* Esecuzione SQL
463800130910     C                   Exsr      ExeSQLPre
463900130918C2226C*
464000130918  "  C                   ENDSR
464100130918C2226C/EJECT
464200130919C2226C************************************************************
464300130919  "  C* Composizione stringa sql di tabella in qtemp
464400130919  "  C************************************************************
464500130919C2226C     InzSqltab     BEGSR
464600130919     C* Selezione
464700131022     C                   EVAL      SQ='Select * from Qtemp/NDrcaTemp'
464800131022      *
464900131022     C                   if        d1impda > 0 or
465000131022     C                             d1impa > 0
465100131022     C                   if        d1impa = 0
465200131022     C                   eval      d1impa=*all'9'
465300131022     C                   ENDIF
465400131022      *
465500131022     C                   eval      SQ=%trim(SQ) +
465600131022     C                             ' where imppre between ' +
465700131022     C                             %char(d1impda) +
465800131022     C                             ' and ' + %char(d1impa)
465900131022     C                   ENDIF
466000131022     C
466100130919     C* ordinamento
466200130919     C                   cat       'Order by'  :1Sq
466300130919     C                   cat       'ImpPre'    :1Sq
466400130919     C                   cat       'Desc'      :1Sq
466500130919     C*
466600130919     C* Esecuzione SQL
466700130919     C                   Exsr      ExeSQLTab
466800130919C2226C*
466900130919  "  C                   ENDSR
467000130919C2226C/EJECT
467100131001BRT  C************************************************************
467200131001 "   C* Selezione fasi aperte / chiuse BRT
467300131001 "   C************************************************************
467400131001 "   C     ParzFasiBRT   BEGSR
467500131001 "    /free
467600131001 "          if d1FasiApe = xscoff;
467700131001 "         eval sq = %trim(sq) +
467800131001 "       ' and not exists +
467900131001 "       (select rcaksc from ndrca00f as b  +
468000131001 "       where b.rcautechiu ='' '' +
468100131001 "       and ndrca00f.rcasocieta = b.rcasocieta +
468200131001 "       and ndrca00f.rcaposiz = b.rcaposiz +
468300131001 "       and ndrca00f.rcaunita = b.rcaunita +
468400131001 "       and ndrca00f.rcakcc = b.rcakcc +
468500131001 "       and ndrca00f.rcaksc = b.rcaksc +
468600131001 "       and ndrca00f.rcadtsca = b.rcadtsca +
468700131001 "       and ndrca00f.rcadtpar = b.rcadtpar +
468800131001 "       and ndrca00f.rcanrpar = b.rcanrpar +
468900131001 "       and ndrca00f.rcaserpar = b.rcaserpar) ';
469000131001 "       endif;
469100131001 "    /end-free
469200131001BRT  C                   ENDSR
469300100420     C******************************************************
469400100420     C* Parzializzazioni data partita
469500100420     C******************************************************
469600100420     C     ParzDtPar     BEGSR
469700100420     c*
469800131001BRT  c                   if        D1DtParDa <> 0
469900131001BRT  c                             or D1DtParA <> 0
470000100420     c                   move      WdtParDa      DtPar10DA        10
470100100420     c                   move      WdtParA       DtPar10A         10
470200100420     C                   Cat       'and'       :1Sq
470300100420     C                   Cat       '(RcaDtPar' :1Sq
470400100420     C                   Cat       '>='        :1Sq
470500100420     C                   Cat       '''':0        Sq
470600100420     C                   Cat       DtPar10DA   :0Sq
470700100420     C                   Cat       '''':0        Sq
470800100420     C                   Cat       'and'       :1Sq
470900100420     C                   Cat       'RcaDtPar'  :1Sq
471000100420     C                   Cat       '<='        :1Sq
471100100420     C                   Cat       '''':0        Sq
471200100420     C                   Cat       DtPar10A    :0Sq
471300100420     C                   Cat       '''':0        Sq
471400100420     C                   Cat       ')'         :1Sq
471500131001BRT  C                   Endif
471600100420     C*
471700100420     C                   ENDSR
471800090325     C******************************************************
471900100401     C* Parzializzazioni data scadenza
472000090325     C******************************************************
472100100401     C     ParzDtSca     BEGSR
472200090325     c*
472300131001BRT  c                   if        D1DtScaDa <> 0
472400131001BRT  c                             or D1DtScaA <> 0
472500100401     c                   move      WdtScaDa      DtSca10DA        10
472600100401     c                   move      WdtScaA       DtSca10A         10
472700090325     C                   Cat       'and'       :1Sq
472800100401     C                   Cat       '(RcaDtSca' :1Sq
472900090325     C                   Cat       '>='        :1Sq
473000090325     C                   Cat       '''':0        Sq
473100100401     C                   Cat       DtSca10DA   :0Sq
473200090325     C                   Cat       '''':0        Sq
473300090325     C                   Cat       'and'       :1Sq
473400100401     C                   Cat       'RcaDtSca'  :1Sq
473500090325     C                   Cat       '<='        :1Sq
473600090325     C                   Cat       '''':0        Sq
473700100401     C                   Cat       DtSca10A    :0Sq
473800090325     C                   Cat       '''':0        Sq
473900090325     C                   Cat       ')'         :1Sq
474000131001BRT  C                   Endif
474100090325     C*
474200090325     C                   ENDSR
474300100416     C******************************************************
474400100416     C* Parzializzazioni data sollecito
474500100416     C******************************************************
474600100416     C     ParzDtSol     BEGSR
474700100416     c*
474800131001BRT  c                   if        D1DtSolDa <> 0
474900131001BRT  c                             or D1DtSolA <> 0
475000100416     c                   move      WdtSolDa      DtSol10DA        10
475100100416     c                   move      WdtSolA       DtSol10A         10
475200100416     C                   Cat       'and'       :1Sq
475300100416     C                   Cat       '(RcaDtLet' :1Sq
475400100416     C                   Cat       '>='        :1Sq
475500100416     C                   Cat       '''':0        Sq
475600100416     C                   Cat       DtSol10DA   :0Sq
475700100416     C                   Cat       '''':0        Sq
475800100416     C                   Cat       'and'       :1Sq
475900100416     C                   Cat       'RcaDtLet'  :1Sq
476000100416     C                   Cat       '<='        :1Sq
476100100416     C                   Cat       '''':0        Sq
476200100416     C                   Cat       DtSol10A    :0Sq
476300100416     C                   Cat       '''':0        Sq
476400100416     C                   Cat       ')'         :1Sq
476500131001BRT  C                   Endif
476600100416     C*
476700100416     C                   ENDSR
476800100401     C******************************************************
476900100401     C* Parzializzazioni destinatario
477000100401     C******************************************************
477100100401     C     ParzDest      BEGSR
477200100401     c*
477300100401     c                   if        D1Destin <> *blanks
477400100401     C                   Cat       'and'       :1Sq
477500100401     C                   Cat       'RcaGrute'  :1Sq
477600100402     C                   Cat       'in('       :1Sq
477700100401     C*
477800100401     C*   leggo tutta la tabella 048 per caricare i gruppi del
477900100401     c*   destinatario
478000100401     C*
478100100401     C                   MOVE      '2'           XTBRIC
478200100401     C                   MOVE      XSCSOC        XTBAZI
478300100401     C                   MOVE      '048'         XTBCOD
478400100401     C                   MOVE      *BLANKS       XTBKEY
478500100401     C                   CALLb     'XATB'
478600100401     C                   PARM                    XATBDS
478700100401     C     XTBERR        DOWEQ     *OFF
478800100401     C                   MOVEL     XTBUNI        ang048ds
478900100401     c                   move      xtbkey        Gruppo            3
479000100401     c                   select
479100100401     c                   when      d1Destin=usr1048
479200100401     C                   Cat       ''''        :0Sq
479300100402     c                   cat       Gruppo      :0Sq
479400100401     C                   Cat       ''''        :0Sq
479500100402     C                   Cat       ','         :1Sq
479600100401     c                   when      d1Destin=usr2048
479700100401     C                   Cat       ''''        :0Sq
479800100402     c                   cat       Gruppo      :0Sq
479900100401     C                   Cat       ''''        :0Sq
480000100402     C                   Cat       ','         :1Sq
480100100401     c                   when      d1Destin=usr3048
480200100401     C                   Cat       ''''        :0Sq
480300100402     c                   cat       Gruppo      :0Sq
480400100401     C                   Cat       ''''        :0Sq
480500100402     C                   Cat       ','         :1Sq
480600100401     c                   when      d1Destin=usr4048
480700100401     C                   Cat       ''''        :0Sq
480800100402     c                   cat       Gruppo      :0Sq
480900100401     C                   Cat       ''''        :0Sq
481000100402     C                   Cat       ','         :1Sq
481100100401     c                   when      d1Destin=usr5048
481200100401     C                   Cat       ''''        :0Sq
481300100402     c                   cat       Gruppo      :0Sq
481400100401     C                   Cat       ''''        :0Sq
481500100402     C                   Cat       ','         :1Sq
481600100401     c                   when      d1Destin=usr6048
481700100401     C                   Cat       ''''        :0Sq
481800100402     c                   cat       Gruppo      :0Sq
481900100401     C                   Cat       ''''        :0Sq
482000100402     C                   Cat       ','         :1Sq
482100100401     c                   when      d1Destin=usr7048
482200100401     C                   Cat       ''''        :0Sq
482300100402     c                   cat       Gruppo      :0Sq
482400100401     C                   Cat       ''''        :0Sq
482500100402     C                   Cat       ','         :1Sq
482600100401     c                   when      d1Destin=usr8048
482700100401     C                   Cat       ''''        :0Sq
482800100402     c                   cat       Gruppo      :0Sq
482900100401     C                   Cat       ''''        :0Sq
483000100402     C                   Cat       ','         :1Sq
483100100401     c                   endsl
483200100401     C*
483300100401     C                   MOVE      '3'           XTBRIC
483400100401     C                   MOVE      XSCSOC        XTBAZI
483500100401     C                   MOVE      *ZERO         XTBKEY
483600100401     C                   MOVE      '048'         XTBCOD
483700100401     C                   CALLb     'XATB'
483800100401     C                   PARM                    XATBDS
483900100401     C*
484000100401     C                   ENDDO                                                  *
484100100401     c* chiudo stringa sql
484200100402     C                   Cat       ''''        :1Sq
484300100402     C                   Cat       ' '         :1Sq
484400100402     C                   Cat       ''''        :1Sq
484500100402     C                   Cat       ')'         :1Sq
484600100401     c
484700100401     c                   endif
484800100401     C*
484900100401     C                   ENDSR
485000100401     C******************************************************
485100100401     C* Parzializzazioni data attività
485200100401     C******************************************************
485300100401     C     ParzDtAtt     BEGSR
485400100401     c*
485500100401     c                   move      WdtAttDa      DtAtt10DA        10
485600100401     c                   move      WdtAttA       DtAtt10A         10
485700100401     C                   Cat       'and'       :1Sq
485800100401     C                   Cat       '(RcaDtAtt' :1Sq
485900100401     C                   Cat       '>='        :1Sq
486000100401     C                   Cat       '''':0        Sq
486100100401     C                   Cat       DtAtt10DA   :0Sq
486200100401     C                   Cat       '''':0        Sq
486300100401     C                   Cat       'and'       :1Sq
486400100401     C                   Cat       'RcaDtAtt'  :1Sq
486500100401     C                   Cat       '<='        :1Sq
486600100401     C                   Cat       '''':0        Sq
486700100401     C                   Cat       DtAtt10A    :0Sq
486800100401     C                   Cat       '''':0        Sq
486900100401     C                   Cat       ')'         :1Sq
487000100401     C*
487100100401     C                   ENDSR
487200070525     C******************************************************
487300070525     C* ESECUZIONE SQL
487400070525     C******************************************************
487500070525     C     EXESQL        BEGSR
487600070525     C*
487700070525     C                   EXSR      CLOCUR
487800070525     C*
487900070525     C/EXEC SQL
488000070525     C+     PREPARE S1 FROM :SQ
488100070525     C/END-EXEC
488200070525     C*
488300070525     C/EXEC SQL
488400070525     C+     DECLARE A1 CURSOR FOR S1
488500070525     C/END-EXEC
488600070525     C*
488700070525     C/EXEC SQL
488800070525     C+     OPEN A1
488900070525     C/END-EXEC
489000070525     c*
489100070525     C                   MOVE      *ON           $OPEN             1
489200070525     c*
489300070525     C                   ENDSR
489400070525     C*
489500070525     C************************************************************
489600070525     C* CHIUSURA CURSORE
489700070525     C************************************************************
489800070525     C     CLOCUR        BEGSR
489900070525     C*
490000070525     C/EXEC SQL
490100070525     C+     CLOSE A1
490200070525     C/END-EXEC
490300070525     C*
490400070525     C                   MOVE      *OFF          $OPEN
490500070525     C*
490600070525     C                   ENDSR
490700070525     C/EJECT
490800070525     C************************************************************
490900070525     C* fetch
491000070525     C************************************************************
491100070525     C     FetchSql      BEGSR
491200070525     C*
491300070525     C/EXEC SQL
491400100402     C+     FETCH A1 INTO :DsFetch
491500070525     C/END-EXEC
491600070525     c*
491700070525     C                   ENDSR
491800130910C2226C******************************************************
491900130910  "  C* ESECUZIONE SQL
492000130910  "  C******************************************************
492100130910C2226C     EXESQLPRE     BEGSR
492200130910     C*
492300130910     C                   EXSR      CLOCURPRE
492400130910     C*
492500130910     C/EXEC SQL
492600130910     C+     PREPARE S2 FROM :SQ
492700130910     C/END-EXEC
492800130910     C*
492900130910     C/EXEC SQL
493000130910     C+     DECLARE A2 CURSOR FOR S2
493100130910     C/END-EXEC
493200130910     C*
493300130910     C/EXEC SQL
493400130910     C+     OPEN A2
493500130910     C/END-EXEC
493600130910     c*
493700130910     C                   MOVE      *ON           $OPENPre          1
493800130910     c*
493900130910C2226C                   ENDSR
494000130910  "  C*
494100130910  "  C************************************************************
494200130910  "  C* CHIUSURA CURSORE
494300130910  "  C************************************************************
494400130910C2226C     CLOCURPRE     BEGSR
494500130910     C*
494600130910     C/EXEC SQL
494700130910     C+     CLOSE A2
494800130910     C/END-EXEC
494900130910     C*
495000130910     C                   MOVE      *OFF          $OPENPre
495100130910     C*
495200130910C2226C                   ENDSR
495300130910  "  C/EJECT
495400130910  "  C************************************************************
495500130910  "  C* fetch
495600130910  "  C************************************************************
495700130910C2226C     FetchSqlPre   BEGSR
495800130910     C*
495900130910     C/EXEC SQL
496000130910     C+     FETCH A2 INTO :DsFetchPre
496100130910     C/END-EXEC
496200130910     c*
496300130912C2226C                   ENDSR
496400130919C2226C******************************************************
496500130919  "  C* ESECUZIONE SQL
496600130919  "  C******************************************************
496700130919C2226C     EXESQLTAB     BEGSR
496800130919     C*
496900130919     C                   EXSR      CLOCURTAB
497000130919     C*
497100130919     C/EXEC SQL
497200130919     C+     PREPARE S3 FROM :SQ
497300130919     C/END-EXEC
497400130919     C*
497500130919     C/EXEC SQL
497600130919     C+     DECLARE A3 CURSOR FOR S3
497700130919     C/END-EXEC
497800130919     C*
497900130919     C/EXEC SQL
498000130919     C+     OPEN A3
498100130919     C/END-EXEC
498200130919     c*
498300130919     C                   MOVE      *ON           $OPENTab          1
498400130919     c*
498500130919C2226C                   ENDSR
498600130919  "  C*
498700130919  "  C************************************************************
498800130919  "  C* CHIUSURA CURSORE
498900130919  "  C************************************************************
499000130919C2226C     CLOCURTAB     BEGSR
499100130919     C*
499200130919     C/EXEC SQL
499300130919     C+     CLOSE A3
499400130919     C/END-EXEC
499500130919     C*
499600130919     C                   MOVE      *OFF          $OPENTAB
499700130919     C*
499800130919C2226C                   ENDSR
499900130919  "  C/EJECT
500000130919  "  C************************************************************
500100130919  "  C* fetch
500200130919  "  C************************************************************
500300130919C2226C     FetchSqlTAB   BEGSR
500400130919     C*
500500130919     C/EXEC SQL
500600130919     C+     FETCH A3 INTO :DsFetchTab
500700130919     C/END-EXEC
500800130919     c*
500900130919C2226C                   ENDSR
501000070525     C*
501100941209     C************************************************************
501200941209     C* REPERIMENTO DATI SOCIETA'
501300941209     C************************************************************
501400941209     C     REPSOC        BEGSR
501500941209     C*
501600950522     C                   CALLB     'XSOC'
501700941209     C                   PARM                    TIPXSC            6
501800941209     C                   PARM                    SOCXSC            3
501900941209     C                   PARM                    CDSXSC            9 0
502000941209     C                   PARM                    MODXSC            3
502100941209     C                   PARM      *BLANKS       RTNXSC            1
502200941209     C                   PARM                    XSOCDS
502300941209     C                   PARM                    KPJBA
502400941209     C*
502500941209     C                   ENDSR
502600940223     C/EJECT
502700090325     C************************************************************
502800090325     C* REPERIMENTO AUTORIZZAZIONI UTENTE
502900090325     C************************************************************
503000090325     C     REPAUT        BEGSR
503100090325     C*
503200090325     C                   CLEAR                   AUTDS
503300090325     C*
503400090325     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
503500090325     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
503600090325     C                   Z-ADD     0             XCAGRP                         GRUPPO
503700090325     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
503800100409    >C                   MOVEL     'CONREC'      XCAFNC                         FUNZIONE
503900090325    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
504000090325     c                   MOVE      'REC'         XCAVFU
504100090511    >C                   MOVE      'CC'          XCATCK                         TIPO CTRL
504200090325    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
504300090325     C*
504400090325     C                   CALLB     'XCHKAUT'
504500090325     C                   PARM                    AUTDS
504600090325     C*
504700090325     C* errori/non abilitazione
504800090325     C     XCARTN        IFNE      0
504900090325    >C                   MOVE      '2'           ErrL76
505000090325     C                   EXSR      ENDPGM
505100090325     C                   ELSE
505200100409    >C                   MOVEL     XCADDS        CONRECDS
505300090325     C* Se non autorizzati alla funzione oppure                              ed esco
505400100409    >C     visATT        IFNE      *ON
505500090325     C                   MOVE      XCAFNC        XNOFNC
505600090325     C                   MOVE      XCAPRF        XNOPRF
505700090325     C                   MOVE      XCACUT        XNOCUT
505800090325     C                   MOVE      XCATCK        XNOTCK
505900090325     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
506000090325     C                   CALL      'XNOAUTR'
506100090325     C                   PARM                    XNOADS
506200090325     C                   MOVE      '2'           ErrL76
506300090325     C                   MOVE      *ON           $fine
506400090325     C                   EXSR      ENDPGM
506500090325     C                   ENDIF
506600090325     C                   ENDIF
506700090325     C*
506800090325     C                   ENDSR
506900090325     C/EJECT
507000940127     C************************************************************
507100940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
507200940127     C************************************************************
507300940131     C     REPDFT        BEGSR
507400940127     C*
507500950524     C* preparo i parametri di chiamata al driver
507600950524     C                   CLEAR                   XMPEDS
507700950524     C                   EVAL      TpChiamata = '01'
507800950524     C                   EVAL      NChiavi = 5
507900950524     C                   EVAL      VLogica = 2
508000950524     C*
508100950524     C* preparo le chiavi per la chiamata al driver
508200950524     C                   CLEAR                   XMPEDS1
508300950524     C                   MOVEL     XSCSOC        DSMPESOC
508400950524     C                   MOVEL     DSPGM         DSMPEPGM
508500950524     C                   MOVEL     KNMUS         DSMPEPRF
508600950524     C* Si reperiscono i dati di default a seconda delle modalità con
508700950524     C* cui il pgm è usato
508800950524     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
508900950524     C* MPETIP = 'PV'  pgm richiamato da menù
509000950524     C* MPETIP = 'PB'  x lancio Batch
509100941013     C                   SELECT
509200090323    >C     OPZL76        WHENEQ    '01'
509300950524     C                   MOVEL     'PX'          DSMPETIP
509400941013     C                   OTHER
509500950524     C                   MOVEL     'PV'          DSMPETIP
509600941013     C                   ENDSL
509700950524     C*
509800950524     C                   CALLB     'XMPE'
509900950524     C                   PARM                    XMPEDS
510000950524     C                   PARM                    XMPEDS1
510100950524     C*
510200950524     C     Risultato     IFEQ      '0'
510300950524     C                   MOVEL     DSMPEPAR      DSMPE
510400940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
510500940131     C* i suoi valori di default vengono decisi a pgm
510600940131     C                   ELSE
510700090323     C                   Z-ADD     2             $LOP
510800940131     C                   Z-ADD     1             $ORD
510900130924C2226C                   eval      $PRE    = *on
511000130924C2226C                   eval      $RGRPRE = 'P'
511100941027     C                   ENDIF
511200940131     C*
511300940131     C                   ENDSR
511400940223     C/EJECT
511500940131     C************************************************************
511600940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
511700940131     C************************************************************
511800940225     C     DEFOPZ        BEGSR
511900940131     C*
512000940225     C* Questa routine deve valorizzare la schiera $FL1
512100940225     C* con S o N a seconda della utilizzabilità o meno
512200940225     C* dell'opzione correlata
512300940225     C*
512400950301     C* Inizialmente si considerano tutte le opzioni come valide
512500940317     C     1             DO        $DIMOP        X
512600960119     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
512700960117     C                   If        $TX1(X)  <> *Blank
512800940225     C                   MOVE      'S'           $FL1(X)
512900960117     C                   Else
513000960117     C                   Leave
513100960117     C                   EndIf
513200941027     C                   ENDDO
513300940715     C*
513400940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
513500941018     C* con 1 o 0 a seconda della utilizzabilità o meno
513600940715     C* del tasto funzione correlato
513700940715     C*
513800940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
513900940715     C* di compilazione
514000940715     C                   RESET                   $FC1
514100940715     C                   RESET                   $FR1
514200940715     C                   RESET                   $FC2
514300940715     C                   RESET                   $FR2
514400940919     C                   RESET                   $FC3
514500940919     C                   RESET                   $FR3
514600940225     C*
514700940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
514800940225     C* invalido alcune opzioni
514900940225     C                   SELECT
515000940225     C* Se pgm richiamato in ricerca disattivo :
515100090323    >C     OPZL76        WHENEQ    '01'
515200940603     C* F3=Fine
515300940603     C                   Z-ADD     3             X
515400941216     C                   MOVE      '0'           $FC1(X)
515500941216     C                   MOVE      '0'           $FC2(X)
515600941216     C                   MOVE      '0'           $FC3(X)
515700950116     C*
515800940225     C* Se pgm richiamato in interrogazione disattivo :
515900090323    >C     OPZL76        WHENEQ    '05'
516000940225     C* 1=Selezione
516100940225     C                   Z-ADD     1             X
516200940225     C     01            LOOKUP    $OP1(X)                                21
516300940225     C     *IN21         IFEQ      *ON
516400940225     C                   MOVE      'N'           $FL1(X)
516500940225     C                   ENDIF
516600940225     C* 2=Modifica
516700940225     C                   Z-ADD     1             X
516800940225     C     02            LOOKUP    $OP1(X)                                21
516900940225     C     *IN21         IFEQ      *ON
517000940225     C                   MOVE      'N'           $FL1(X)
517100940225     C                   ENDIF
517200940225     C* 3=Copia
517300940225     C                   Z-ADD     1             X
517400940225     C     03            LOOKUP    $OP1(X)                                21
517500940225     C     *IN21         IFEQ      *ON
517600940225     C                   MOVE      'N'           $FL1(X)
517700940225     C                   ENDIF
517800940225     C* 4=Annullamento
517900940225     C                   Z-ADD     1             X
518000940225     C     04            LOOKUP    $OP1(X)                                21
518100940225     C     *IN21         IFEQ      *ON
518200940225     C                   MOVE      'N'           $FL1(X)
518300940225     C                   ENDIF
518400940225     C*
518500090323    >C     OPZL76        WHENEQ    *BLANKS
518600940225     C* Se pgm non richiamato, disattivo subito :
518700941124     C* F8=Selezione seguente
518800941124     C                   Z-ADD     8             X
518900941206     C                   MOVE      '0'           $FC1(X)
519000941206     C                   MOVE      '0'           $FC2(X)
519100940225     C*
519200950301     C* In tutti gli altri casi, disattivo subito :
519300941228     C                   OTHER
519400941228     C*
519500940225     C                   ENDSL
519600941118     C*
519700941118     C* poi, disattivo a seconda delle autorizzazioni :
519800070524    >C*                  IF        AUT___ <> *on
519900070524    >C*                  Z-ADD     1             X
520000070524    >C*    XX            LOOKUP    $OP1(X)                                21
520100070524    >C*    *IN21         IFEQ      *ON
520200070524    >C*                  MOVE      'N'           $FL1(X)
520300070524    >C*                  ENDIF
520400070524    >C*                  ENDIF
520500940225     C*
520600940225     C                   ENDSR
520700940225     C/EJECT
520800940225     C************************************************************
520900940225     C* REPERIMENTO OPZIONI
521000940225     C************************************************************
521100940225     C     REPOPZ        BEGSR
521200940225     C*
521300940314     C     1             DO        $DIMOP        X
521400940128     C*
521500940223     C     $MS1(X)       IFNE      *BLANK
521600940128     C*
521700940223     C                   MOVEL     $MS1(X)       IDMSG
521800950522     C                   CALLB     'XRTVM'
521900940201     C                   PARM                    IDMSG            27
522000940201     C                   PARM                    TXTMSG          100
522100940201     C                   PARM                    ERRMSG            1
522200940128     C*
522300940128     C     ERRMSG        IFNE      *ON
522400940223     C                   MOVEL     TXTMSG        $TX1(X)
522500940128     C                   ELSE
522600940223     C                   MOVEL     *ALL'?'       $TX1(X)
522700941027     C                   ENDIF
522800940128     C*
522900941027     C                   ENDIF
523000940128     C*
523100941027     C                   ENDDO
523200940128     C*
523300940128     C                   ENDSR
523400940506     C/EJECT
523500940506     C************************************************************
523600940506     C* REPERIMENTO TASTI DI FUNZIONE
523700940506     C************************************************************
523800940506     C     REPKEY        BEGSR
523900940506     C                   MOVEA     $FM1          $FM
524000950721     C                   MOVEA     $FC1          $FC
524100940506     C                   EXSR      REPKKK
524200940506     C                   MOVEA     $FX           $FX1
524300940506     C*
524400940506     C                   MOVEA     $FM2          $FM
524500950721     C                   MOVEA     $FC2          $FC
524600940506     C                   EXSR      REPKKK
524700940506     C                   MOVEA     $FX           $FX2
524800940919     C*
524900940919     C                   MOVEA     $FM3          $FM
525000950721     C                   MOVEA     $FC3          $FC
525100940919     C                   EXSR      REPKKK
525200940919     C                   MOVEA     $FX           $FX3
525300940919     C                   ENDSR
525400940506     C/EJECT
525500940506     C************************************************************
525600940506     C* REPERIMENTO TASTI DI FUNZIONE
525700940506     C************************************************************
525800940506     C     REPKKK        BEGSR
525900940506     C*
526000940506     C                   CLEAR                   $FX
526100940506     C     1             DO        24            X
526200940506     C*
526300940506     C     $FM(X)        IFNE      *BLANK
526400950721     C     $Fc(X)        andne     '0'
526500940506     C*
526600940506     C                   MOVEL     $FM(X)        IDMSG
526700950522     C                   CALLB     'XRTVM'
526800940506     C                   PARM                    IDMSG            27
526900940506     C                   PARM                    TXTMSG          100
527000940506     C                   PARM                    ERRMSG            1
527100940506     C*
527200940506     C     ERRMSG        IFNE      *ON
527300940506     C                   MOVEL     TXTMSG        $FX(X)
527400940506     C                   ELSE
527500940506     C                   MOVEL     *ALL' '       $FX(X)
527600941027     C                   ENDIF
527700940506     C*
527800941027     C                   ENDIF
527900940506     C*
528000941027     C                   ENDDO
528100940506     C*
528200940506     C                   ENDSR
528300940223     C/EJECT
528400940128     C************************************************************
528500950606     C* DEFINIZIONE KLIST
528600940128     C************************************************************
528700950606     C     DefKlist      BEGSR
528800940128     C*
528900940223     C* klist
529000090324    >C* AnUni01L
529100090324    >C     K02Uni01      KLIST
529200090324    >C                   KFLD                    UNISocieta
529300090324    >C                   KFLD                    UNIUnita
529400100413    >C* NdRCa01L
529500100413    >C     K07Rca01      KLIST
529600100402    >C                   KFLD                    RcaSocieta
529700100402    >C                   KFLD                    RcaPosiz
529800100402    >C                   KFLD                    RcaKCC
529900100402    >C                   KFLD                    RcaKSC
530000100402    >C                   KFLD                    RcaDtpar
530100100402    >C                   KFLD                    RcaNrpar
530200100402    >C                   KFLD                    RcaSerPar
530300101209c2176c*
530400101209  " >C     K07Rca04      KLIST
530500101209  " >C                   KFLD                    RcaSocieta
530600101209  " >C                   KFLD                    RcaPosiz
530700101209  " >C                   KFLD                    RcaKCC
530800101209  " >C                   KFLD                    RcaKSC
530900101209  " >C                   KFLD                    RcaDtpar
531000101209  " >C                   KFLD                    RcaNrpar
531100101209  "  C                   KFLD                    RcaSerPar
531200101209  "  C*
531300101209  " >C     K08Rca04      KLIST
531400101209  " >C                   KFLD                    RcaSocieta
531500101209  " >C                   KFLD                    RcaPosiz
531600101209  " >C                   KFLD                    RcaKCC
531700101209  " >C                   KFLD                    RcaKSC
531800101209  " >C                   KFLD                    RcaDtpar
531900101209  " >C                   KFLD                    RcaNrpar
532000101209  "  C                   KFLD                    RcaSerPar
532100101209c2176C                   KFLD                    RcaDtSca
532200130923C2226C* Attività Manuali
532300130923C2226C     K08Att01      KLIST
532400130923  "  C                   KFLD                    AttSocieta
532500130923  "  C                   KFLD                    AttKCC
532600130923  "  C                   KFLD                    AttKSC
532700130923  "  C                   KFLD                    AttDtpar
532800130923  "  C                   KFLD                    AttNrpar
532900130923  "  C                   KFLD                    AttSerPar
533000130923  "  C                   KFLD                    AttDtSca
533100130923  "  C                   KFLD                    AttProgr
533200130923  "  C*
533300130923  "  C     K07Att01      KLIST
533400130923  "  C                   KFLD                    AttSocieta
533500130923  "  C                   KFLD                    AttKCC
533600130923  "  C                   KFLD                    AttKSC
533700130923  "  C                   KFLD                    AttDtpar
533800130923  "  C                   KFLD                    AttNrpar
533900130923  "  C                   KFLD                    AttSerPar
534000130923  "  C                   KFLD                    AttDtSca
534100130923C2226C*
534200110412     C                   ENDSR
534300110412D2680C************************************************************
534400110412  "  C* Chiamata a xrtvm
534500110412  "  C************************************************************
534600110412D2680C     PrendiMsg     BEGSR
534700110412     C*
534800110412     C                   CALLB     'XRTVM'
534900110412     C                   PARM                    IDMSG            27
535000110412     C                   PARM                    TXTMSG          100
535100110412     C                   PARM                    ERRMSG            1
535200110412     C                   PARM      *off          CTRMSG            1            centratura
535300110412     C                   PARM                    LENMSG            3 0          lun output.
535400110412     C                   PARM                    MSgDta          100            lun output.
535500110412     C                   PARM                    TxtMsg30         30            lun output.
535600110412     C                   PARM                    TxtMsg3000     3000            lun output.
535700110412     C     ERRMSG        IFNE      *ON
535800110412     C                   MOVEL     TxTMsg        WWMSG           100            *
535900110412     C                   ELSE
536000110412     C                   MOVEL     *ALL'?'       WWMSG
536100110412     C                   ENDIF
536200110412     C*
536300110412D2680C                   ENDSR
536400130910C2226C************************************************************
536500130910  "  C* Reperisco le unità abilititate all'utente
536600130910  "  C************************************************************
536700130910C2226C     CarUniAbi     BEGSR
536800130910     C                   Clear                   WinUni
536900130910     C     XscSoc        setll     AnUni01l
537000130910     c                   do        *hival
537100130910     c     XscSoc        reade     AnUni01l
537200130910     c                   If        %eof(AnUni01l)
537300130910     c                   Leave
537400130910     c                   Endif
537500130910     C* Controllo autorizzazione all'unità
537600130910     C                   RESET                   XAutUniDS
537700130910     C                   EVAL      XAUPrf = KNmUs
537800130910     C                   EVAL      XAUSoc = XscSoc
537900130910     C                   EVAL      XAUUni = UniUnita
538000130910     C                   CALL      'XAUTUNI'
538100130910     C                   PARM                    XAutUniDS
538200130910     C     XAUAbl        IFNE      *OFF
538300130910     C                   If        WinUni = *blanks
538400130910     C                   Eval      WInUni='in (''' + %trimr(UniUnita)
538500130910     C                             + ''''
538600130910     C                   Else
538700130910     C                   Eval      WInUni=%trimr(WinUni) + ',' +''''
538800130910     C                             + %trimr(UniUnita)
538900130910     C                             + ''''
539000130910     C                   Endif
539100130910     C                   ENDIF
539200130910     C                   Enddo
539300130910     C                   If        WinUni <> *blanks
539400130910     C                   Eval      WInUni=%trimr(WinUni) +')'
539500130910     C                   Endif
539600130910     C*
539700130910C2226C                   ENDSR
539800940223     C/EJECT
539900980319** $MS1 / $TX1
540000090323PROMSG    *LIBL     OPZ0001
540100101209PROMSG    *LIBL     OPZ0458                                          C2176
540200090406PROMSG    *LIBL     OPZ0642
540300980319** $OP1 / $FL1
54040009032301S      <Scelta
54050010120915S      <Chiusura fasi partita                                      C2176
54060009040216S      <Interrogazioni
540700130912** $OP2 / $FL2                                                       C2226
54080013091201S      <Scelta                                                     C2226
540900980319** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
541000940506PROMSG    *LIBL     KEY0001  01
541100940506PROMSG    *LIBL     KEY0002  02
541200940506PROMSG    *LIBL     KEY0003  03
541300940506PROMSG    *LIBL     KEY0004  04
541400940506PROMSG    *LIBL     KEY0005  05
541500070531PROMSG    *LIBL     KEY0323  06
541600940506PROMSG    *LIBL     KEY0007  07
541700940506PROMSG    *LIBL     KEY0008  08
541800940506PROMSG    *LIBL     KEY0009  09
541900940506PROMSG    *LIBL     KEY0010  10
542000940506PROMSG    *LIBL     KEY0011  11
542100940506PROMSG    *LIBL     KEY0012  12
542200940506PROMSG    *LIBL     KEY0013  13
542300940506PROMSG    *LIBL     KEY0014  14
542400940506PROMSG    *LIBL     KEY0015  15
542500940506PROMSG    *LIBL     KEY0016  16
542600940506PROMSG    *LIBL     KEY0017  17
542700940506PROMSG    *LIBL     KEY0018  18
542800940506PROMSG    *LIBL     KEY0019  19
542900940506PROMSG    *LIBL     KEY0020  20
543000940506PROMSG    *LIBL     KEY0021  21
543100940506PROMSG    *LIBL     KEY0022  22
543200940506PROMSG    *LIBL     KEY0023  23
543300940506PROMSG    *LIBL     KEY0024  24
543400980319**  ABILITAZIONE D1 : $FC1 / $FR1
5435009407200N  01
5436009507031N  02
5437009407201N  03
5438009407201N  04
5439009407200N  05
5440000903230N  06
5441009407200N  07
5442009412061N  08
5443009407200N  09
5444000705250N  10
5445009407200N  11
5446009407201N  12
5447009407200N  13
5448009407200N  14
5449009407200N  15
5450009407200N  16
5451009407200N  17
5452000705311N  18
5453009407200N  19
5454009407200N  20
5455009407200N  21
5456009407200N  22
5457009507260N  23   Impostare a '1' se si utilizzano opzioni
5458009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
545900980319** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
546000940916PROMSG    *LIBL     KEY0001  01
546100940916PROMSG    *LIBL     KEY0002  02
546200940916PROMSG    *LIBL     KEY0003  03
546300940916PROMSG    *LIBL     KEY0004  04
546400940916PROMSG    *LIBL     KEY0005  05
546500070531PROMSG    *LIBL     KEY0323  06
546600940916PROMSG    *LIBL     KEY0007  07
546700940916PROMSG    *LIBL     KEY0008  08
546800940916PROMSG    *LIBL     KEY0009  09
546900940916PROMSG    *LIBL     KEY0010  10
547000940916PROMSG    *LIBL     KEY0011  11
547100940916PROMSG    *LIBL     KEY0012  12
547200090323PROMSG    *LIBL     KEY0061  13
547300940916PROMSG    *LIBL     KEY0014  14
547400940916PROMSG    *LIBL     KEY0015  15
547500940916PROMSG    *LIBL     KEY0016  16
547600940916PROMSG    *LIBL     KEY0017  17
547700940916PROMSG    *LIBL     KEY0018  18
547800940916PROMSG    *LIBL     KEY0019  19
547900940916PROMSG    *LIBL     KEY0020  20
548000940916PROMSG    *LIBL     KEY0021  21
548100940916PROMSG    *LIBL     KEY0022  22
548200940916PROMSG    *LIBL     KEY0023  23
548300940916PROMSG    *LIBL     KEY0024  24
548400980319**  ABILITAZIONE S1 : $FC2 / $FR2
5485009409160N  01
5486009409160N  02
5487009409161N  03
5488009409160N  04
5489001309241N  05                                       *C2226*
5490000903230N  06
5491009409160N  07
5492009411241N  08
5493009409160N  09
5494000705250N  10
5495009409160N  11
5496009409161N  12
5497000903231N  13
5498009409160N  14
5499009409160N  15
5500009409160N  16
5501009409161N  17
5502000705311N  18
5503009409160N  19
5504009409160N  20
5505009409160N  21
5506009409160N  22
5507009507261N  23   Impostare a '1' se si utilizzano opzioni
5508009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
550900980319** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
551000940506PROMSG    *LIBL     KEY0001  01
551100940506PROMSG    *LIBL     KEY0002  02
551200940506PROMSG    *LIBL     KEY0003  03
551300940506PROMSG    *LIBL     KEY0004  04
551400940506PROMSG    *LIBL     KEY0005  05
551500940506PROMSG    *LIBL     KEY0006  06
551600940506PROMSG    *LIBL     KEY0007  07
551700940506PROMSG    *LIBL     KEY0008  08
551800940506PROMSG    *LIBL     KEY0009  09
551900940506PROMSG    *LIBL     KEY0010  10
552000940506PROMSG    *LIBL     KEY0011  11
552100940506PROMSG    *LIBL     KEY0012  12
552200940506PROMSG    *LIBL     KEY0013  13
552300940506PROMSG    *LIBL     KEY0014  14
552400940506PROMSG    *LIBL     KEY0015  15
552500940506PROMSG    *LIBL     KEY0016  16
552600940506PROMSG    *LIBL     KEY0017  17
552700940506PROMSG    *LIBL     KEY0018  18
552800940506PROMSG    *LIBL     KEY0019  19
552900940506PROMSG    *LIBL     KEY0020  20
553000940506PROMSG    *LIBL     KEY0021  21
553100940506PROMSG    *LIBL     KEY0022  22
553200940506PROMSG    *LIBL     KEY0023  23
553300940506PROMSG    *LIBL     KEY0024  24
553400980319**  ABILITAZIONE D3 : $FC3 / $FR3
5535009407200N  01
5536000903241N  02
5537009407201N  03
5538000903241N  04
5539009407200N  05
5540009407200N  06
5541009407200N  07
5542009407200N  08
5543009407200N  09
5544009409160N  10
5545009407200N  11
5546009407201N  12
5547009407200N  13
5548009407200N  14
5549009407200N  15
5550009407200N  16
5551009409160N  17
5552009409160N  18
5553009407200N  19
5554009407200N  20
5555009407200N  21
5556009407200N  22
5557009507260N  23   Impostare a '1' se si utilizzano opzioni
5558009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
555900110412** MSG
556000110412PROMSG    *LIBL     KEY0012    F12=Ritorno                D2680
556100130924PROMSG    *LIBL     KEY0006    F06=Conferma               C2226
