000100070524     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200070524     H*PARMS BNDDIR(PJXBND)
000300070524     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000400070524     H*PARMS DYNUSRPRF(*OWNER) RDB(*NONE)
000500140310     H DFTACTGRP(*NO) ACTGRP('QILE') DATFMT(*ISO) BNDDIR('PJXBND')
000600140310     H DECEDIT('0,') DATEDIT(*DMY.)
000700941115      *
000800090323    > *                Lista Attività Recupero credito
000900090323      *           -------------------------------------
001000090324     FAnUni01L  IF   E           K DISK
001100130923C2226FNdAtt01L  IF   E           K DISK
001200100413     FNdRca01L  IF   E           K DISK
001300101209c2176FNdRca04L  UF   E           K DISK
001400101209c2176F                                     RENAME(NDRCA000:NDRCA04)
001500940211     F*------------
001600090323    >FNDCL76D   CF   E             WORKSTN usropn
001700090323    >F                                     RENAME(CL76D1:FMTD1)
001800090323    >F                                     RENAME(CL76D2:FMTD2)
001900090323    >F                                     RENAME(CL76D3:FMTD3)
002000090323    >F                                     RENAME(CL76S1:FMTS1)
002100090323    >F                                     RENAME(CL76C1:FMTC1)
002200130911C2226F                                     RENAME(CL76C2:FMTC2)
002300130911C2226F                                     RENAME(CL76S2:FMTS2)
002400090323    >F                                     RENAME(CL76T1:FMTT1)
002500090323    >F                                     RENAME(CL76Z1:FMTZ1)
002600090323    >F                                     RENAME(CL76Z2:FMTZ2)
002700090323    >F                                     RENAME(CL76Z3:FMTZ3)
002800110412D2680F                                     RENAME(CL76D5:FMTD5)
002900940131     F                                     SFILE(FMTS1:S1NRR)
003000130911C2226F                                     SFILE(FMTS2:S2NRR)
003100940201     F                                     INFDS(DSFMT)
003200940128     D*----------------------------------------------------*
003300940302     D* Schiere per la gestione delle opzioni
003400940302     D*
003500960117     D* dimensione della schiera $MS1, da non cambiare
003600960117     D $DIMOP          C                   CONST(30)
003700950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003800950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
003900950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004000950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
004100130912C2226D $OP2            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004200130912C2226D $FL2            S              1    DIM($dimop) ALT($OP2)
004300960117     D* schiera contenente i nomi dei programma richiamati attraverso
004400960117     D* le opzioni personalizzate
004500960117     D $PGO1           S             10    DIM($dimop)
004600941214     D $TX             S             50    DIM(50)
004700941214     D $FL             S              1    DIM(50)
004800940909     D* Schiere generali per la gestione dei tasti di funzione
004900940506     D $FM             S             27    DIM(24)
005000940506     D $FX             S             50    DIM(24)
005100940506     D $FC             S              1    DIM(24)
005200940506     D $FR             S              1    DIM(24)
005300940909     D* Schiere per la gestione dei tasti di funzione
005400940506     D* Video D1
005500940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005600940506     D $FX1            S             50    DIM(24) ALT($FM1)
005700940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005800940506     D $FR1            S              1    DIM(24) ALT($FC1)
005900960202     D* Schiera contente i nomi dei programmi da richiamare con i tasti
006000960202     D* funzione personalizzata S1
006100960202     D $PGF2           S             10    DIM(25)
006200940506     D* Schiere per la gestione dei tasti di funzione
006300940506     D* Video S1
006400940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006500940506     D $FX2            S             50    DIM(24) ALT($FM2)
006600940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006700940506     D $FR2            S              1    DIM(24) ALT($FC2)
006800940916     D* Schiere per la gestione dei tasti di funzione
006900940916     D* Video D2
007000940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
007100940916     D $FX3            S             50    DIM(24) ALT($FM3)
007200940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007300940916     D $FR3            S              1    DIM(24) ALT($FC3)
007400090325     D*-------------
007500090325     D* Video S1
007600090325     D WC1Rcd          S                   like(c1nrr) inz
007700090325     D $OpzOk          S              1
007800090325     D WOpz            S                   like(S1Opz) inz
007900090325     D $X              S                   like(c1nrr) inz
008000950726     D*
008100130911C2226D* Video S2
008200130911  "  D WC2Rcd          S                   like(c2nrr) inz
008300130911  "  D $OpzOkS2        S              1
008400130911  "  D WOpzS2          S                   like(S2Opz) inz
008500130911  "  D $XS2            S                   like(c2nrr) inz
008600130911C2226D*
008700950726     D $D1L01          S              3  0
008800950726     D $D1L02          S              3  0
008900950726     D $S1L01          S              3  0
009000950726     D $S1L02          S              3  0
009100130911C2226D $S2L01          S              3  0
009200130911C2226D $S2L02          S              3  0
009300950906     D $C1Op1          S              3  0
009400950906     D $C1Op2          S              3  0
009500950906     D $D2L01          S              3  0
009600950906     D $D2L02          S              3  0
009700130911C2226D $C2Op1          S              3  0
009800130911C2226D $C2Op2          S              3  0
009900940302     D*
010000960202     D*-------------
010100960202     D* Indice della schiera tasti funzione
010200960202     D PosFun          S              2  0
010300940211     D/EJECT
010400940128     D*----------------------------------------------------*
010500100402     DDsFetch          Ds
010600100407     D DsSocieta                           like(RcaSocieta)
010700100402     D DsPosiz                             like(RcaPosiz)
010800100407     D DsAttrib                            like(RcaAttrib)
010900100402     D DsKcc                               like(RcaKcc)
011000100402     D DsKsc                               like(RcaKsc)
011100100402     D DsDtPar                             like(RcaDtPar)
011200100402     D DsNrPar                             like(RcaNrPar)
011300100402     D DsSerPar                            like(RcaSerPar)
011400100419     D Dsdivisa                            like(RcaDivisa)
011500100407     D DskscAge                            like(RcakscAge)
011600130910C2226DDsFetchPre       Ds
011700130910  "  D Ds2Societa                          like(RcaSocieta)
011800130918  "  D Ds2Unita                            like(RcaUnita)
011900130918  "  D Ds2Kcc                              like(RcaKcc)
012000130910  "  D Ds2CdFisc                     20
012100130910  "  D Ds2Partiva                    20
012200130918  "  D Ds2Sogg                        8
012300130919  "  D Ds2descr                      44
012400130918  "  D Ds2Importo                    20  4
012500130918  "  D*
012600130919  "  D DsfetchTab      DS
012700130918  "  D KccPre                              like(H2kcc)
012800130918  "  D Partivapre                          like(H2partiva)
012900130918  "  D CdfiscPre                           like(H2CdFisc)
013000130918  "  D SoggPre                             like(H2Sogg)
013100130918  "  D DescrPre                            like(S2descr)
013200130919  "  D ImpPre                        19  3
013300130918  "  D*
013400130918  "  D $PKcc           S                   Like(RcaKcc)
013500130919  "  D $PSogg          S                   Like(H2Sogg)
013600130918  "  D $PPartiva       S                   Like(H2Partiva)
013700130918  "  D $PCdFisc        S                   Like(H2CdFisc)
013800130919  "  D $First          S              1    inz
013900130918C2226D*
014000940211     D*-------------
014100940207     D* Valori assunti
014200950524     D XMPEDS        E DS                  INZ
014300950524     D XMPEDS1       E DS                  INZ
014400941111     D DSMPE           DS                  INZ
014500940207     D  $LOP                   1      1  0
014600940831     D  $ORD                   2      3  0
014700130912C2226D  $PRE                   4      4
014800130912C2226D  $RGRPRE                5      5
014900940207     D*-------------
015000940211     D* Passaggio Parametri
015100940211     D KPJBA         E DS
015200940211     D*-------------
015300940225     D* Reperimento Autorizzazioni
015400941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015500070531     D*-------------
015600070531     D* Dati per pgm XNOAUT
015700070531     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
015800941124     D*-------------
015900950410     D* Passaggio Autorizzazioni
016000950410     D DSAUT           DS           512
016100090325     D*-------------
016200090325     D* Maschera dati autorizzazioni
016300100409    >D CONRECDS      E DS
016400941209     D*-------------
016500130909C2226D* Autorizzazioni alla unità
016600130909  "  D XAUTUNIDS     E DS
016700130909C2226D*-------------
016800941209     D* Dati di ambiente ottenuti da XSOC
016900970424     D SOC001        E DS                  EXTNAME(XSOC001DS)
017000090324     D    FgO1                        1    overlay(XscFgO)
017100941209     D*-------------
017200941209     D* DS Interna per dati di output di XSOC
017300941209     D XSOCDS          DS          1000
017400070629     D*-------------
017500070629     D* DS gestione parametri modulo
017600070629     D XPAOUT          DS          2000
017700090324     D anp057ds      E DS
017800940325     D*-------------
017900940325     D* Parametri in ricezione
018000090323    >D NDCL76DS      E DS
018100090330     D*-------------
018200090330      * Parametri per scheda cliente
018300090330     D NDCL77DS      E DS                  inz
018400090324     D*-------------
018500090324      * Parametri per ricerca unita
018600090324     Dana009ds       E DS                  inz
018700090323     D*-------------
018800090323     D* Ricerca capoconti
018900090323     D A200DS        E DS                  EXTNAME(ANA200DS) INZ
019000090323     D*-------------
019100090323     D* Ricerca sottoconti
019200090323     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
019300090324     D*-------------
019400090324     D XATBDS        E DS                  INZ
019500090324     D*-------------
019600090324     D* Parametri per ricerca codice ANATB00F
019700090324     D XTABDS        E DS                  INZ
019800090416     D*-------------
019900100407     D* Tabella azioni
020000100407     D Ang047DS      E DS                  Inz
020100100407     D*-------------
020200100407     D* Tabella destinatari
020300100401     D Ang048DS      E DS                  Inz
020301140530     D* Tabella ABC
020302140530     D AngC48DS      E DS                  Inz
020303140530     D*-------------------------------------
020500090409     D*  ds  attributi contabili
020600090409     D Ang0A2ds      E DS                  INZ
020700090409     D Ang0A1ds      E DS                  INZ
020800090409     D*-------------------------------------
020900090324     D* Ricerca legende
021000090324     D Ana964ds      E DS                  INZ
021100090324     D*-------------
021200090324     D* Controlli validità legende
021300090324     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
021400090324     D*-------------
021500090324     D* Ricerca profili
021600090324     D Ana001ds      E DS                  inz
021700090324     D*----------------------------------------------------*
021800090324     D* Reperimento profilo
021900090324     D parmok          s              1
022000090324     D* parametro utente
022100090324     D ANUTNParm       s                   like(ANUTN00F)
022200090324     D* lunghezza parametro
022300090324     D LungUTN         S              5s 0
022400090324     D*
022500090324     D Anutn00f      e ds
022600070524     D*-------------
022700070524     D* Parametri del PGM di interrogazione conti
022800070524    >D ANA399DS      E DS                  inz
022900070524    >D  A39RET       E                     INZ('0')
023000070524    >D  A39ERRORI    E                     INZ('0')
023100130919C2226D*-------------
023200130919  "  D* Parametri del PGM di interrogazione conti
023300130919C2226D NDC981DS      E DS                  inz
023400090406     D*-------------
023500070525     D* Controllo conti
023600070525     D ND002DS       E DS
023700090409     D Categorie             131    250
023800100402     D*-------------
023900100402     D* Ricerca agenti
024000100402     D A400DS        E DS                  EXTNAME(ANA400DS) INZ
024100070525     D*-------------
024200090409     D* Schiera categorie da ND002DS
024300090409     D DsCateg         DS           120
024400090409     D SkCateg                        4    dim(30)
024401140604     D*
024402140604     D*- Ottimizzazione call X20scadr
024403140604     D SkSaveKsc       S              8    dim(200)
024404140604     D nextksc         S              3  0
024405140604     D SavePartiva     S                   like(h2Partiva)
024500090409     D*-------------
024600070525     D* Definizioni parametri standard chiamate moduli
024700070525     D$Esito           S              1
024800070525     D$gesmsg          S              1
024900070525     D$lenout          S              9B 0
025000070525     D$struttura       S             10
025100070525     D$kcc             S              6
025200070525     D$ksc             S              8
025300090323     D*-------------
025400960202     D* campo di salvataggio per la DS d'ingresso durante la chiamata
025500960202     D* alla funzione personalizzata
025600090323    >D $SaveDS         S                   like(NDCL76DS)
025700090325     D*-------------
025800090325     D*tasto precedente = F13=selezione multipla
025900090325     D  $TastoSav      s                   Like($Tasto) inz
026000130911C2226D  $TastoSav2     s                   Like($Tasto) inz
026100090325     D*emetto al window di conf.canc. solo x il 1° rec.da cancellare
026200090325     D  PrimaVolta     s              1  0 inz(0)
026300960202     D*-------------
026400940211     D DSFMT           DS           512
026500940506     D  $TASTO               369    369
026600940211     D  NRG                  370    370
026700940211     D  NCL                  371    371
026800940211     D  SFLNRR               378    379B 0
026900090324      * riga cursore window
027000090324     D  NRgWin               382    382
027100090324      * colonna cursore window
027200090324     D  NClWin               383    383
027300940211     D*-------------
027400940211     D* posizione cursore
027500940211     D CURSOR          DS
027600940211     D  RIRI                   1      2B 0 INZ
027700940211     D  R$$                    2      2
027800940211     D  COCO                   3      4B 0 INZ
027900940211     D  C$$                    4      4
028000940127     D*-------------
028100940127     D* Reperimento nome PGM
028200940127     D STATUS         SDS           333
028300940127     D  DSPGM            *PROC
028400950411     D  PARMS            *PARMS
028500101209c2176D  SDSUSR               254    263
028600940207     D*-------------
028700940616     D* Costanti
028800940616     D* nr.di righe del sfl (SFLPAG)
028900090330     D SFLPAG          C                   CONST(16)
029000940616     D*-------------
029100940506     D* Tasti di funzione
029200940506     D F01             C                   CONST(X'31')
029300940506     D F02             C                   CONST(X'32')
029400940506     D F03             C                   CONST(X'33')
029500940506     D F04             C                   CONST(X'34')
029600940506     D F05             C                   CONST(X'35')
029700940506     D F06             C                   CONST(X'36')
029800940506     D F07             C                   CONST(X'37')
029900940506     D F08             C                   CONST(X'38')
030000940506     D F09             C                   CONST(X'39')
030100940506     D F10             C                   CONST(X'3A')
030200940506     D F11             C                   CONST(X'3B')
030300940506     D F12             C                   CONST(X'3C')
030400940506     D F13             C                   CONST(X'B1')
030500940506     D F14             C                   CONST(X'B2')
030600940506     D F15             C                   CONST(X'B3')
030700940506     D F16             C                   CONST(X'B4')
030800940506     D F17             C                   CONST(X'B5')
030900940506     D F18             C                   CONST(X'B6')
031000940506     D F19             C                   CONST(X'B7')
031100940506     D F20             C                   CONST(X'B8')
031200940506     D F21             C                   CONST(X'B9')
031300940506     D F22             C                   CONST(X'BA')
031400940506     D F23             C                   CONST(X'BB')
031500940506     D F24             C                   CONST(X'BC')
031600940506     D ENTER           C                   CONST(X'F1')
031700940506     D ROLDWN          C                   CONST(X'F4')
031800940506     D ROLLUP          C                   CONST(X'F5')
031900961017     D*-------------
032000021010     D* Attributi video
032100021010     D Normale         C                   Const(X'32')
032200021010     D Protetto        C                   Const(X'A0')
032300021010     D*-------------
032400950524     D* Sottotitolo di testata
032500070524     D SOTTIT          C                   CONST(' ')
032600070524     D*SOTTIT          C                   CONST('PROMSG    *LIBL-
032700070524     D*                                         COSxxxx')
032800960109     D*-------------
032900950606     D* Variabili per gestione videate
033000950606     D $Gest           S              2A
033100950606     D $Fine           S              1A
033200950606     D $InzD1          S              1A
033300950606     D $InzS1          S              1A
033400950606     D $InzD2          S              1A
033500130911C2226D $InzS2          S              1A
033600950606     D $LastG          S              2A
033700950606     D $LastV          S              2A
033800950606     D $EndFile        S              1A
033900130911C2226D $EndFilePre     S              1A
034000950606     D $Esci           S              1A
034100950606     D $RcdOK          S              1A
034200130911C2226D $RcdOKPre       S              1A
034300950606     D $Unique         S              1A
034400950606     D*-------------
034500950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
034600950606     D S1Nrr           S                   like(C1Rcd)
034700130911C2226D S2Nrr           S                   like(C2Rcd)
034800950606     D WSfl            S                   like(C1Nrr)
034900130911C2226D WSfl2           S                   like(C1Nrr)
035000950606     D WMax            S                   like(C1Rcd)
035100130911C2226D WMaxS2          S                   like(C2Rcd)
035200950606     D WPag            S                   like(C1Rcd)
035300130911C2226D WPag2           S                   like(C1Rcd)
035400950606     D WInzS1          S                   like($InzS1)
035500130911C2226D WInzS2          S                   like($InzS2)
035600090323    >D WOpr            S                   like(OprL76)
035700090323     D CurR            S              3S 0
035800090323     D CurC            S              3S 0
035900950606     D Win             S             99A
036000950606     D W1Pos           S                   like(C1Pos)
036100130911C2226D W2Pos           S                   like(C2Pos)
036200950606     D WINKR           S              1A
036300970912     D WExf            S              1A
036400070711     D ZDateISO        S               D
036500070525     D*-------------
036600070525     D* Variabili specifiche del programma
036700950606     D*-------------
036800100420     D WDtParDa        S                   like(Zdateiso)
036900100420     D WDtParA         S                   like(Zdateiso)
037000100401     D WDtScaDa        S                   like(Zdateiso)
037100100401     D WDtScaA         S                   like(Zdateiso)
037200100401     D WDtAttDa        S                   like(Zdateiso)
037300100401     D WDtAttA         S                   like(Zdateiso)
037400140319     D WDtChiDa        S                   like(Zdateiso)
037500140319     D WDtChiA         S                   like(Zdateiso)
037600100416     D WDtSolDa        S                   like(Zdateiso)
037700100416     D WDtSolA         S                   like(Zdateiso)
037800070525     D Wdate           S               D
037900130918C2226D WInUni          S           4000
038000130913  "  D WExi            S           1024
038100130913  "  D* ds nota di chiusura fase
038200130913  "  D d5nota          DS           400
038300130913  "  D d5nota1                 1     36
038400130913  "  D d5nota2                37     88
038500130913  "  D d5nota3                89    140
038600130913  "  D d5nota4               141    192
038700130913  "  D d5nota5               193    244
038800130913  "  D d5nota6               245    296
038900130913  "  D d5nota7               297    348
039000130913C2226D d5nota8               349    400
039100070525     D*-------------
039200950606     D*  Indici
039300070524     D X               S              5S 0
039400061116     D Y               S              5S 0
039500960117     D* Indice della schiera per le opzioni personalizzate
039600960117     D PosOpz          S              2  0
039700960117     D* numero max opzioni
039800960117     D NumOpz          S              2  0 Inz($DIMOP)
039900960117     D* numero effettive opzioni
040000960117     D OpzLen          S              2  0
040100070525     D*-------------
040200070525     D* Stringa per SQL
040300130919C2226X*** SQ              DS          5000
040400130919C2226D SQ              DS          5000
040500070531     D*-------------
040600070531     D $A              s             79
040700070531     D $B              s             79
040800070725     D*-------------
040900070725     D* DS Esterna gestione messaggi                                            *
041000070725     D XMSGDS        E DS                  inz                                  *
041100070725     D  STS          E                     EXTFLD(MSGSTS)                       *
041200070725     D                                     DIM(333)                             *
041300070725     D  JBA          E                     EXTFLD(MSGJBA)                       *
041400070725     D                                     DIM(502)                             *
041500070725     D  MSGRSP       E                     INZ('N')
041600070725     D  MSGEMV       E                     INZ('S')
041700070725     D  MSGMSG       E                     INZ(0)
041800070725     D  MSGRGP       E                     INZ(10)
041900070725     D  MSGCNL       E                     INZ('N')
042000070725     D  MSGVID       E                     INZ('N')
042100070725     D  MSGOPE       E                     INZ('N')
042200070725     D  MSGSTP       E                     INZ('N')
042300070725     D  MSGLCK       E                     INZ('N')
042400070725     D*---------------
042500090409    9DPuntini          C                   const('...................')
042600100407     D*  bottone presente
042700100407     D PRESENTE        C                   CONST(X'A1')
042800100407     D PRESERED        C                   CONST(X'A9')
042900100407     D PRESEPNK        C                   CONST(X'B9')
043000100408     D PRESEBLU        C                   CONST(X'BB')
043100100412     D PRESEHIG        C                   CONST(X'23')
043200100511     D PRESETRQ        C                   CONST(X'B1')
043300110412D2680D WWMSG           s                   like(Txtmsg)
043400110412  "  D* schiera costanti
043500110412D2680D MSG             S             27    DIM(10) CTDATA PERRCD(1)             messaggi possibili
043600940127     C*----------------------------------------------------*
043700940127     C*                MAIN LINE PROGRAM
043800940127     C*----------------------------------------------------*
043900940127     C*
044000940223     C* inizializzazione variabili
044100940223     C                   EXSR      INZVAR
044200940223     C*
044300940223     C     $FINE         DOWEQ     *OFF
044400940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
044500940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
044600940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
044700130911C2226C     $GEST         CASEQ     'S2'          GESS2                          Lista
044800941027     C                   ENDCS
044900941027     C                   ENDDO
045000940325     C* fine programma
045100940325     C                   EXSR      ENDPGM
045200940225     C*
045300940325     C/EJECT
045400940325     C************************************************************
045500940325     C* FINE PROGRAMMA
045600940325     C************************************************************
045700940325     C     ENDPGM        BEGSR
045800940325     C*
045900940325     C* passaggio dati al pgm chiamante
046000090323    >C                   MOVEL     WOPR          OPRL76                                     ne S1
046100090323    >C                   MOVEL     NDCL76DS      KPJBU
046200940325     C*
046300940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
046400090323    >C     OPZL76        IFEQ      *BLANK
046500940325     C                   SETON                                            LR
046600940325     C* altrimenti chiudo in RT
046700940325     C                   ELSE
046800940325     C                   SETON                                            RT
046900940325     C                   ENDIF
047000940616     C                   RETURN
047100940325     C*
047200940325     C                   ENDSR
047300940224     C/EJECT
047400940201     C************************************************************
047500940201     C* RICERCHE
047600940201     C************************************************************
047700940127     C     SEARCH        BEGSR
047800940127     C*
047900090324     C* determino Riga/Colonna del cursore
048000090324     c                   if        $lastV='D2'
048100090324     C                   MOVE      NRGWIN        R$$
048200090324     C                   MOVE      NCLWIN        C$$
048300090324     C                   else
048400090324     C                   MOVE      NRG           R$$
048500090324     C                   MOVE      NCL           C$$
048600090324     c                   endif
048700940127     C                   Z-ADD     RIRI          CURR
048800940127     C                   Z-ADD     COCO          CURC
048900090323     C*
049000140318brt  C     D1KCC         IFeq      *BLANKS
049100140318brt  C                   evaL      D1KCC = 'C'
049200140318brt  C                   ENDIF
049300140318     C*
049400090323     C     D1KCC         IFNE      *BLANKS
049500090323     C     D1KCC         ANDNE     'C     '
049600090323     C     D1KCC         ANDNE     'F     '
049700090323     C                   CALL      'XALLINEA'
049800090323     C                   PARM                    D1KCC
049900090323     C                   PARM      6             LEN               5 0
050000090323     C                   PARM                    XSCINP
050100090323     C                   ENDIF
050200090323     C*
050300090323     C     D1KSC         IFNE      *BLANKS
050400090323     C                   CALL      'XALLINEA'
050500090323     C                   PARM                    D1KSC
050600090323     C                   PARM      8             LEN               5 0
050700090323     C                   PARM                    XSCINP
050800090323     C                   ENDIF
050900100402     C*
051000100402     C* agente recupero crediti
051100100402     C     D1KSCAGE      IFNE      *BLANKS
051200100402     C                   CALLB     'XALLINEA'
051300100402     C                   PARM                    D1KSCAGE
051400100402     C                   PARM      8             LEN               5 0
051500100402     C                   PARM                    XSCINP
051600100402     C                   ENDIF
051700090323     C*
051800090323   B1C                   SELECT
051900090324    >C* Ricerca unita
052000090324     C     H1NMFL        wheneq    'D1UNITA'
052100090324     C                   RESET                   Ana009ds
052200090324     C                   MOVE      '01'          opz009
052300090324     C                   MOVE      xscsoc        soc009
052400090324     C                   MOVEL     Ana009ds      KPJBU
052500090324     C                   CALL      'ANA009R'
052600090324     C                   PARM                    KPJBA
052700090324     C                   MOVEL     KPJBU         Ana009ds
052800090324     C*
052900090324     C     uni009        IFNE      *BLANK
053000090324     C                   MOVE      uni009        d1unita
053100090324     C                   ENDIF
053200090324     C* Ricerca capoconto
053300090323     C     H1NMFL        WHENEQ    'D1KCC'
053400090323     C                   RESET                   A200DS
053500090323     C                   MOVEL     '01'          OPZ200
053600090323     C                   MOVE      D1KCC         KCC200
053700090323     C                   MOVEL     A200DS        KPJBU
053800090323     C                   CALL      'ANA200R'
053900090323     C                   PARM                    KPJBA
054000090323     C                   MOVEL     KPJBU         A200DS
054100090323     C*
054200090323     C     KCC200        IFNE      *BLANK
054300090323     C                   MOVEL     KCC200        D1KCC
054400090323     C                   ENDIF
054500090323     C*
054600090324     C* Ricerca sottoconto
054700090323     C     H1NMFL        WHENEQ    'D1KSC'
054800090323     C                   RESET                   A201DS
054900090323     C                   MOVE      XSCSOC        SOC201
055000090323     C                   MOVE      D1KCC         KCC201
055100090323     C                   MOVE      D1KsC         KSC201
055200090323     C                   MOVEL     A201DS        KPJBU
055300090323     C                   CALL      'ANA201R'
055400090323     C                   PARM                    KPJBA
055500090323     C                   MOVEL     KPJBU         A201DS
055600090323     C*
055700090323     C     KSC201        IFNE      *BLANK
055800090323     C                   MOVEL     KCC201        D1KCC
055900090323     C                   MOVEL     KSC201        D1KSC
056000090323     C                   ENDIF
056100140317     c*
056200140317     C* Fasi selezione tipo
056300140317     C     H1NMFL        WHENEQ    'D1FASIAPE'
056400140317     C                   RESET                   ANA964DS
056500140317     C                   MOVE      '01'          OPZ964
056600140317     C                   MOVEL     'ANDIZ000'    TRC964
056700140317     C                   MOVEL     'DSPSELFASI'  CAM964
056800140317     C                   MOVEL     *ON           ALL964
056900140317     C                   CLEAR                   KPJBU
057000140317     C                   MOVEL     ANA964DS      KPJBU
057100140317     C                   CALL      'ANA964R'
057200140317     C                   PARM                    KPJBA
057300140317     C                   MOVEL     KPJBU         ANA964DS
057400140317     C     Ret964        IFNE      '2'
057500140317     C                   MOVEL     Val964        D1FASIAPE
057600140317     C                   MOVEL     DES964        D1FASIAPED
057700140317     C                   ENDIF
057800100402     C*  Attributo
057900100402     C     H1NMFL        WHENEQ    'D1ATTRIB'
058000100402     C                   RESET                   XTABDS
058100100402     C                   MOVE      '01'          XTAOPZ
058200100402     C                   MOVE      XSCSOC        XTAAZI
058300100402     C                   MOVE      XSCLIN        XTALIN
058400100402     C                   Z-ADD     CURR          XTARIG
058500100402     C                   Z-ADD     CURC          XTACOL
058600100402     c                   move      posattr057    xtapos
058700100402     C                   CLEAR                   KPJBU
058800100402     C                   MOVEL     XTABDS        KPJBU
058900100402     C                   CALL      'ANG0A1R'
059000100402     C                   PARM                    KPJBA
059100100402     C                   MOVEL     KPJBU         XTABDS
059200100402     C     XTAret        IFNE      '2'
059300100402     C                   MOVE      XTAKEY        D1Attrib
059400100402     C                   ENDIF
059500140428     C*  Stato del Credito - attributo 03
059600140513     C     H1NMFL        WHENEQ    'D1STACREDA'
059601140513     C                   RESET                   XTABDS
059800140428     C                   MOVE      '01'          XTAOPZ
059900140428     C                   MOVE      XSCSOC        XTAAZI
060000140428     C                   MOVE      XSCLIN        XTALIN
060100140428     C                   Z-ADD     CURR          XTARIG
060200140428     C                   Z-ADD     CURC          XTACOL
060300140428     c                   move      '03'          xtapos
060400140428     C                   CLEAR                   KPJBU
060500140428     C                   MOVEL     XTABDS        KPJBU
060600140428     C                   CALL      'ANG0A1R'
060700140428     C                   PARM                    KPJBA
060800140428     C                   MOVEL     KPJBU         XTABDS
060900140428     C     XTAret        IFNE      '2'
061000140428     C                   MOVE      XTAKEY        wrk4              4
061100140513     C                   MOVEL     wrk4          D1STACREDA
061200140428     C                   ENDIF
061201140513     C*  Stato del Credito - attributo 03
061202140513     C     H1NMFL        WHENEQ    'D1STACREA'
061203140513     C                   RESET                   XTABDS
061204140513     C                   MOVE      '01'          XTAOPZ
061205140513     C                   MOVE      XSCSOC        XTAAZI
061206140513     C                   MOVE      XSCLIN        XTALIN
061207140513     C                   Z-ADD     CURR          XTARIG
061208140513     C                   Z-ADD     CURC          XTACOL
061209140513     c                   move      '03'          xtapos
061210140513     C                   CLEAR                   KPJBU
061211140513     C                   MOVEL     XTABDS        KPJBU
061212140513     C                   CALL      'ANG0A1R'
061213140513     C                   PARM                    KPJBA
061214140513     C                   MOVEL     KPJBU         XTABDS
061215140513     C     XTAret        IFNE      '2'
061216140513     C                   MOVE      XTAKEY        wrk4              4
061217140513     C                   MOVEL     wrk4          D1STACREA
061218140513     C                   ENDIF
061220140513     C* - Agente
061400100402     C     H1NMFL        WHENEQ    'D1KSCAGE'
061500100402     C                   RESET                   A400DS
061600100402     C                   MOVEL     '01'          OPZ400
061700100402     C                   MOVE      '3'           AGE400
061800100402     C                   MOVEL     A400DS        KPJBU
061900100402     C                   CALL      'ANA400R'
062000100402     C                   PARM                    KPJBA
062100100402     C                   MOVEL     KPJBU         A400DS
062200100402     C*
062300100402     C     KSC400        IFNE      *BLANK
062400100402     C                   MOVEL     KSC400        D1KSCAGE
062500100402     C                   ENDIF
062600090324     C*
062700090324     C* Ricerca destinatario
062800090324     C     H1NMFL        WHENEQ    'D1DESTIN '
062900090324    >C                   RESET                   ANA001DS
063000090324    >C                   MOVEL     '01'          OPZ001
063100090324    >C                   MOVEL     ANA001DS      KPJBU
063200090324    >C                   CALL      'ANA001R'
063300090324     C                   PARM                    KPJBA
063400090324    >C                   MOVEL     KPJBU         ANA001DS
063500090324     C     RET001        IFNE      '2'
063600090324     c                   eval      D1Destin = Prf001
063700090324     C                   ENDIF
063800100407     C*  Azione
063900100407     C     H1NMFL        WHENEQ    'D1AZIONE'
064000100407     C                   RESET                   XTABDS
064100100407     C                   MOVE      '01'          XTAOPZ
064200100407     C                   MOVE      XSCSOC        XTAAZI
064300100407     C                   MOVE      XSCLIN        XTALIN
064400100407     C                   Z-ADD     CURR          XTARIG
064500100407     C                   Z-ADD     CURC          XTACOL
064600100407     C                   CLEAR                   KPJBU
064700100407     C                   MOVEL     XTABDS        KPJBU
064800100407     C                   CALL      'ANG047R'
064900100407     C                   PARM                    KPJBA
065000100407     C                   MOVEL     KPJBU         XTABDS
065100100407     C     XTAret        IFNE      '2'
065200100407     C                   MOVE      XTAKEY        D1AZIONE
065300100407     C                   ENDIF
065301140530     C*  ABC
065302140530     C     H1NMFL        WHENEQ    'D1ABC'
065303140530     C                   RESET                   XTABDS
065304140530     C                   MOVE      '01'          XTAOPZ
065305140530     C                   MOVE      XSCSOC        XTAAZI
065306140530     C                   MOVE      XSCLIN        XTALIN
065307140530     C                   Z-ADD     CURR          XTARIG
065308140530     C                   Z-ADD     CURC          XTACOL
065309140530     C                   CLEAR                   KPJBU
065310140530     C                   MOVEL     XTABDS        KPJBU
065311140530     C                   CALL      'ANGC48R'
065312140530     C                   PARM                    KPJBA
065313140530     C                   MOVEL     KPJBU         XTABDS
065314140530     C     XTAret        IFNE      '2'
065315140530     C                   MOVE      XTAKEY        D1ABC
065316140530     C                   ENDIF
065400090324     c*
065500090324     C* Ordinamento
065600090324     C     H1NMFL        WHENEQ    'D2ORD'
065700090324     C                   RESET                   ANA964DS
065800090324     C                   MOVE      '01'          OPZ964
065900090324     C                   MOVEL     'ANDIZ000'    TRC964
066000100401     C                   MOVEL     'DSPORDRCA'   CAM964
066100090324     C                   MOVEL     *oFF          ALL964
066200090324     C                   CLEAR                   KPJBU
066300090324     C                   MOVEL     ANA964DS      KPJBU
066400090324     C                   CALL      'ANA964R'
066500090324     C                   PARM                    KPJBA
066600090324     C                   MOVEL     KPJBU         ANA964DS
066700090324     C     Ret964        IFNE      '2'
066800090324     C                   MOVEL     Val964        D2ORD
066900090324     C                   MOVEL     DES964        D2ORDD
067000090324     C                   ENDIF
067100130912C2226C* Raggruppamento
067200130912  "  C     H1NMFL        WHENEQ    'D2RGRPRE'
067300130912  "  C                   RESET                   ANA964DS
067400130912  "  C                   MOVE      '01'          OPZ964
067500130912  "  C                   MOVEL     'ANDIZ000'    TRC964
067600130912  "  C                   MOVEL     'DSPRGRSCL'   CAM964
067700130912  "  C                   MOVEL     *oFF          ALL964
067800130912  "  C                   CLEAR                   KPJBU
067900130912  "  C                   MOVEL     ANA964DS      KPJBU
068000130912  "  C                   CALL      'ANA964R'
068100130912  "  C                   PARM                    KPJBA
068200130912  "  C                   MOVEL     KPJBU         ANA964DS
068300130912  "  C     Ret964        IFNE      '2'
068400130912  "  C                   MOVEL     Val964        D2RGRPRE
068500130912  "  C                   MOVEL     DES964        D2RGRPRED
068600130912C2226C                   ENDIF
068700090323     C*
068800940223     C* ricerca non abilitata in questa posizione
068900941118   X1C                   OTHER
069000940929     C                   SETON                                        9899
069100940127     C*
069200941118   E1C                   ENDSL
069300950612     C*
069400950612     C* imposto pos. del cursore
069500950612     C                   Z-ADD     CURR          H1RIGA
069600950612     C                   Z-ADD     CURC          H1COLO
069700940506     C                   MOVE      *HIVAL        $LASTV
069800950612     C*
069900940127     C                   ENDSR
070000940131     C/EJECT
070100940224     C************************************************************
070200940131     C* RIEMPIMENTO OPZIONI
070300940224     C************************************************************
070400940131     C     RIEOPZ        BEGSR
070500940223     C*
070600940223     C* Input :
070700940223     C* - $TX = schiera con tutti i testi disponibili
070800940223     C* - $FL = schiere con i flag di validità associati ai testi
070900940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
071000940223     C*          opzione usato
071100940223     C* - WMXO = variabile contenente il numero massimo di testi
071200940223     C*          opzione usabili
071300940721     C* - $L01 = Lunghezza prima riga
071400940721     C* - $L02 = Lunghezza seconda riga
071500940223     C* Output :
071600940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
071700940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
071800941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
071900940223     C*
072000950522     C                   CALLB     'X04OPMSG'
072100940223     C* input
072200940223     C                   PARM                    $TX
072300940223     C                   PARM                    $FL
072400940225     C                   PARM                    WCOZ              3 0
072500940223     C                   PARM                    WMXO              3 0
072600940223     C* output
072700940223     C                   PARM                    WOPZ1            76
072800940223     C                   PARM                    WOPZ2            76
072900941214     C                   PARM                    FF23              1
073000950301     C* input
073100950726     C                   PARM                    $L01              3 0
073200950726     C                   PARM                    $L02              3 0
073300940223     C*
073400060224     C                   PARM                    $ALLOPT         960
073500060224     C*
073600940131     C                   ENDSR
073700940506     C/EJECT
073800940506     C************************************************************
073900940506     C* RIEMPIMENTO TASTI FUNZIONE
074000940506     C************************************************************
074100940506     C     RIEKEY        BEGSR
074200940506     C*
074300940506     C* Input :
074400940506     C* - $FX = schiera con tutti i testi disponibili
074500940506     C* - $FC = schiere con i flag di validità associati ai testi
074600940506     C* - $FR = schiere con i flag di REVERSE IMAGE
074700940506     C* - $ULK = ultimo testo caricato
074800940721     C* - $L01 = Lunghezza prima riga
074900940721     C* - $L02 = Lunghezza seconda riga
075000940506     C* Output :
075100940506     C* - $ULK
075200940609     C* - $KEY1= 1° riga di tasti funzione
075300940609     C* - $KEY2= 2° riga di tasti funzione
075400940506     C*
075500950522     C                   CALLB     'XKEYMSG'
075600940506     C* input
075700940506     C                   PARM                    $FX
075800940506     C                   PARM                    $FC
075900940506     C                   PARM                    $FR
076000940506     C                   PARM                    $ULK              3 0
076100940506     C* output
076200940506     C                   PARM                    $KEY1            79
076300940506     C                   PARM                    $KEY2            79
076400950301     C* input
076500950726     C                   PARM                    $L01              3 0
076600950726     C                   PARM                    $L02              3 0
076700940506     C*
076800060224     C                   PARM                    $ALLFUNCT       480
076900060224     C*
077000940506     C                   ENDSR
077100940224     C/EJECT
077200940224     C************************************************************
077300940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
077400940224     C************************************************************
077500940224     C     GESPIE        BEGSR
077600940224     C*
077700940224     C                   SELECT
077800940224     C* Parzializzazioni
077900940224     C     $GEST         WHENEQ    'D1'
078000940506     C     $ULKD1        IFEQ      0
078100940506     C* riempimento opzioni
078200940506     C                   EXSR      F24D1
078300940506     C                   ELSE
078400940506     C                   WRITE     FMTZ1
078500940506     C                   ENDIF
078600130911C2226C* PreLista
078700130911  "  C     $GEST         WHENEQ    'S2'
078800130911  "  C     $ULKS2        IFEQ      0
078900130911  "  C                   EXSR      F24S2
079000130911  "  C                   ELSE
079100130911  "  C                   WRITE     FMTZ2
079200130911C2226C                   ENDIF
079300940506     C* Lista
079400940506     C     $GEST         WHENEQ    'S1'
079500940506     C     $ULKS1        IFEQ      0
079600940506     C                   EXSR      F24S1
079700940506     C                   ELSE
079800940506     C                   WRITE     FMTZ2
079900940506     C                   ENDIF
080000940916     C* Modifica valori assunti
080100940916     C     $GEST         WHENEQ    'D2'
080200940916     C     $ULKD2        IFEQ      0
080300940916     C                   EXSR      F24D2
080400940916     C                   ELSE
080500940916     C                   WRITE     FMTZ3
080600940916     C                   ENDIF
080700940506     C                   ENDSL
080800940506     C*
080900940224     C                   ENDSR
081000940224     C/EJECT
081100940224     C************************************************************
081200940224     C* GESTIONE PARZIALIZZAZIONI
081300940224     C************************************************************
081400940224     C     GESD1         BEGSR
081500940224     C* inizializzazione videata
081600940223     C     $INZD1        IFEQ      *ON
081700940127     C                   EXSR      INZD1
081800940223     C                   MOVE      *OFF          $INZD1
081900941027     C                   ENDIF
082000940223     C* emissione piede videata se proveniente da altra
082100940223     C* salvataggio valore formato di provenienza
082200940202     C     $LASTV        IFNE      'D1'
082300940223     C* salvo stato F17 per sapere cosa fare in caso di F12
082400940223     C                   MOVE      *INKR         WINKR
082500940202     C                   WRITE     FMTT1
082600940224     C                   EXSR      GESPIE
082700940202     C                   MOVE      $LASTV        $LASTG
082800940202     C                   MOVE      'D1'          $LASTV
082900940707     C                   ENDIF
083000940707     C*
083100940707     C* se esistono errori sulla videata
083200940707     C* emetto la write del formato a indicatori spenti per vedere
083300940707     C* le eventuali decodifiche
083400940707     C     *IN99         IFEQ      *ON
083500940707     C                   MOVEA     *IN           WIN
083600940707     C                   MOVE      *ALL'0'       IN5098           49
083700940707     C                   MOVEA     IN5098        *IN(50)
083800940707     C                   WRITE     FMTD1
083900940707     C                   MOVEA     WIN           *IN
084000940707     C                   ENDIF
084100940131     C*
084200940131     C                   EXFMT     FMTD1
084300960605     C*
084400960605     C* azzero la posizione del cursore
084500960605     C                   Z-ADD     0             H1RIGA
084600960605     C                   Z-ADD     0             H1COLO
084700940506     C*
084800951005     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
084900951005     C* INPUT
085000951005     C* $FC1 = Abilitazioni videata D1
085100951005     C* OUTPUT
085200951005     C* *IN97 + *IN99 Errori
085300951005     C                   MOVEA     $FC1          $FC
085400951005     C                   EXSR      CTRKEY
085500940506     C*
085600940506     C     *IN99         IFEQ      *OFF
085700940131     C*
085800940117     C                   SELECT
085900940916     C* F2=Decodifica
086000940916     C     $TASTO        WHENEQ    F02
086100940916     C                   EXSR      F02D1
086200940131     C* F3=Fine
086300940506     C     $TASTO        WHENEQ    F03
086400940307     C                   EXSR      F03D1
086500940131     C* F4=Ricerca
086600940506     C     $TASTO        WHENEQ    F04
086700940127     C                   EXSR      SEARCH
086800940916     C* F5=Refresh
086900940916     C     $TASTO        WHENEQ    F05
087000940916     C                   EXSR      F05D1
087100941206     C* F8=Successivo
087200941206     C     $TASTO        WHENEQ    F08
087300941206     C                   EXSR      F08D1
087400940131     C* F12=Ritorno
087500940506     C     $TASTO        WHENEQ    F12
087600940307     C                   EXSR      F12D1
087700940131     C* F18=Modifica valori assunti
087800940506     C     $TASTO        WHENEQ    F18
087900940307     C                   EXSR      F18D1
088000940224     C* F24=Altri tasti
088100940506     C     $TASTO        WHENEQ    F24
088200940309     C                   EXSR      F24D1
088300940131     C*
088400940117     C                   OTHER
088500940127     C                   EXSR      CTRD1
088600940127     C     *IN99         IFEQ      *OFF
088700940131     C* definisco l'ordinamento
088800950410     C                   EXSR      DEFORD
088900950410     C* richiamo la lista
089000130912C2226C                   If        $Pre  = *on
089100130911  "  C                   MOVE      'S2'          $GEST
089200130911  "  C                   MOVE      *ON           $INZS2
089300130911  "  C                   MOVE      *BLANK        W2POS
089400130911C2226C                   Else
089500940127     C                   MOVE      'S1'          $GEST
089600940223     C                   MOVE      *ON           $INZS1
089700940302     C                   MOVE      *BLANK        W1POS
089800130911C2226C                   ENDIF
089900940211     C                   ENDIF
090000940224     C*
090100940117     C                   ENDSL
090200940224     C*
090300940506     C                   ENDIF
090400950525     C*
090500940117     C                   ENDSR
090600940224     C/EJECT
090700940224     C************************************************************
090800940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
090900940224     C************************************************************
091000940127     C     INZD1         BEGSR
091100940318     C*
091200940318     C                   CLEAR                   FMTD1
091300131001BRT  c                   move      xscon         d1on3
091400131001BRT  c                   move      xscoff        d1off3
091500140310BRT  c                   move      xscon         d1on4
091600140310BRT  c                   move      xscoff        d1off4
091700140314     c***********        move      xscon         d1stato
091800140314     c                   move      *blank        d1FasiApe
091900131001BRT  c                   move      xscon         d1EsclImp
092000140310BRT  c                   move      xscoff        d1Insoluti
092100090324     c*
092200130905C2226X**Impostazione indicatore 11 per gestione campo unità nei solleciti
092300130905C2226X****               if        Gesuni057='D'
092400130905C2226C* Impostazione indicatore 11 per gestione campo unità recupero cred.
092500130905C2226C                   if        GesuniR057='D'
092600090324     C                   eval      *in11=*on
092700130910C2226C                   exsr      CarUniAbi
092800090324     C                   else
092900090324     C                   eval      *in11=*off
093000090324     C                   endif
093100090409      * Impostazione indicatore 17 per gestione campo attributo contabile
093200090409     c                   if        posattr057 <> *blanks
093300090409     C                   eval      *in17=*on
093400090409     C                   else
093500090409     C                   eval      *in17=*off
093600090409     C                   endif
093700941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
093800941013     C*    parzializzazioni
093900941104     C                   SELECT
094000941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
094100950606     C     $Unique       WHENEQ    *ON
094200070524    >C****               MOVE      UQ1001        D1KCC
094300941104     C* il pgm è richiamato e
094400941104     C* sono stati passati dei valori di parzializzazione ma non
094500941104     C* la chiave univoca
094600090323    >C     OPZL76        WHENNE    *BLANK
094700950403     C* imposto i valori passati sui campi a video
094800090323    >C                   MOVE      UnitaL76      D1Unita
094900090323    >C                   MOVE      KccL76        D1kcc
095000090323    >C                   MOVE      KScL76        D1Ksc
095100100401     c                   move      AttribL76     D1Attrib
095200100401     c                   move      KscAgeL76     D1kscAge
095300100420     c                   if        DtParDaL76 <> *loval
095400100420     c     *jobrun       move      DtParDaL76    D1DtParDA
095500100420     c                   endif
095600100420     c                   if        DtParAL76  <> *loval
095700100420     c     *jobrun       move      DtParAL76     D1DtParA
095800100420     c                   endif
095900100401     c                   if        DtScadDL76 <> *loval
096000100401     c     *jobrun       move      DtScadDL76    D1DtScaDA
096100100401     c                   endif
096200100401     c                   if        DtScadAL76 <> *loval
096300100401     c     *jobrun       move      DtScadAL76    D1DtScaA
096400100401     c                   endif
096500100416     c                   if        DtSolDaL76 <> *loval
096600100416     c     *jobrun       move      DtSolDaL76    D1DtSolDA
096700100416     c                   endif
096800100416     c                   if        DtSolAL76  <> *loval
096900100416     c     *jobrun       move      DtSolAL76     D1DtSolA
097000100416     c                   endif
097100100401     c                   move      FaseDAL76     D1FaseDa
097200100401     c                   move      FaseAL76      D1FaseA
097300090323     c                   move      DestinL76     d1Destin
097400100401     c                   if        DtAttDaL76 <> *loval
097500100401     c     *jobrun       move      DtAttDaL76    D1dtAttDa
097600090323     c                   endif
097700100401     c                   if        DtAttAl76  <> *loval
097800100401     c     *jobrun       move      DtAttAL76     D1dtAttA
097900090323     c                   endif
098000941104     C* il pgm non è richiamato
098100090323    >C     OPZL76        WHENEQ    *BLANK
098200950403     C* imposto dei valori di default
098300100401     c                   move      '9'           D1FaseA
098400131018     c                   move      udate         D1dtAttA
098500941115     C                   ENDSL
098600090409     c*
098700090409     C* decodifica posizione attributo
098800090409     c                   if        PosAttr057 <> *blanks
098900090409     C                   MOVE      *ALL'0'       XTBKEY
099000090409    >C                   MOVE      PosAttr057    XTBKEY
099100090409     C                   MOVE      '0'           XTBALC
099200090409     C                   MOVE      '1'           XTBRIC
099300090409     C                   MOVE      '0A2'         XTBCOD
099400090409     C                   CALLB     'XATB'
099500090409     C                   PARM                    XATBDS
099600090409     C     XTBERR        IFEQ      '0'
099700090409     C                   MOVEL     XTBUNI        Ang0a2ds
099800090409     C     DES0A2        CAT       Puntini:1     D1posattrD
099900090409     c                   move      ':'           D1PosAttrD
100000090409     c                   endif
100100090409     c                   endif
100200131129C2232C* Intestazione campo libero se presente nel parametro modulo
100300131129  "  C                   If        LiberoI057 <> *blanks
100400131129  "  C                   Movel(P)  LiberoI057    D1LiberoI
100500131129  "  C                   Else
100600131129  "  C                   Eval      D1LiberoI = 'Campo libero................:'
100700131129C2232C                   Endif
100800940224     C*
100900940117     C                   ENDSR
101000940916     C************************************************************
101100940916     C* GESTIONE F02 VIDEO D1
101200940916     C************************************************************
101300940916     C     F02D1         BEGSR
101400940916     C*
101500940916     C* per decodificare la videata richiamo la routine dei ctrl
101600940916     C* e poi disattivo gli eventuali messaggi di errore
101700940916     C                   EXSR      CTRD1
101800940916     C*
101900940916     C                   MOVE      *ALL'0'       IN5098
102000940916     C                   MOVEA     IN5098        *IN(50)
102100940916     C*
102200940916     C                   ENDSR
102300940224     C/EJECT
102400940131     C************************************************************
102500940307     C* GESTIONE F03 VIDEO D1
102600940131     C************************************************************
102700940307     C     F03D1         BEGSR
102800940131     C*
102900940307     C                   MOVE      *ON           $FINE
103000090323    >C                   MOVE      '1'           RETL76
103100940325     C* fine programma
103200940325     C                   EXSR      ENDPGM
103300940307     C*
103400940307     C                   ENDSR
103500940916     C/EJECT
103600940916     C************************************************************
103700940916     C* GESTIONE F05 VIDEO D1
103800940916     C************************************************************
103900940916     C     F05D1         BEGSR
104000940916     C*
104100940916     C                   MOVE      *ON           $INZD1
104200940916     C*
104300940916     C                   ENDSR
104400941206     C************************************************************
104500941206     C* GESTIONE F08 VIDEO D1
104600941206     C************************************************************
104700941206     C     F08D1         BEGSR
104800941206     C*
104900941206     C                   MOVE      *ON           $FINE
105000090323    >C                   MOVE      '0'           RETL76
105100941206     C*
105200941206     C                   ENDSR
105300950301     C/EJECT
105400940307     C************************************************************
105500940307     C* GESTIONE F12 VIDEO D1
105600940307     C************************************************************
105700940307     C     F12D1         BEGSR
105800940307     C*
105900940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
106000940307     C*  per F12 torno alla lista
106100940307     C     $LOP          IFEQ      2
106200940307     C     WINKR         OREQ      *ON
106300130923C2226X**                 MOVE      'S1'          $GEST
106400130923C2226C                   MOVE      $LastG        $GEST
106500940307     C*  altrimenti, per F12 torno a menu
106600940307     C                   ELSE
106700940307     C                   MOVE      *ON           $FINE
106800090323    >C                   MOVE      '2'           RETL76
106900941027     C                   ENDIF
107000940307     C*
107100940307     C                   ENDSR
107200940307     C/EJECT
107300940307     C************************************************************
107400940307     C* GESTIONE F18 VIDEO D1
107500940307     C************************************************************
107600940307     C     F18D1         BEGSR
107700940307     C*
107800940307     C                   MOVE      'D2'          $GEST
107900940307     C                   MOVEL     *ON           $INZD2
108000940307     C*
108100940307     C                   ENDSR
108200940307     C/EJECT
108300940307     C************************************************************
108400940307     C* GESTIONE F24 VIDEO D1
108500940609     C* input
108600940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
108700940509     C* $FX1 =Testi tasti funzione
108800940509     C* $FC1 = Abilitazioni
108900940509     C* $FR1 = Inversioni di fondo
109000940307     C************************************************************
109100940307     C     F24D1         BEGSR
109200940506     C*
109300940506     C                   MOVEA     $FX1          $FX
109400940506     C                   MOVEA     $FC1          $FC
109500940506     C                   MOVEA     $FR1          $FR
109600940506     C                   Z-ADD     $ULKD1        $ULK
109700950726     C                   Z-ADD     $D1L01        $L01
109800950726     C                   Z-ADD     $D1L02        $L02
109900940506     C                   EXSR      RIEKEY
110000940506     C                   Z-ADD     $ULK          $ULKD1            3 0
110100940720     C                   MOVEA     $FC           $FC1
110200070618     C                   MOVEL     $KEY1         Z1KE
110300060224     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
110400940506     C                   MOVEL     $KEY2         Z1KE2
110500070618     c* riga unica tasti
110600070618     c                   eval      $A = %triml(z1ke)
110700070618     c                   eval      $B = %triml(z1ke2)
110800070618     c     $A            cat(P)    $B:0          z1ke1
110900940506     C                   WRITE     FMTZ1
111000940307     C*
111100940307     C                   ENDSR
111200940307     C/EJECT
111300940307     C************************************************************
111400940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
111500950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
111600960109     C* 93 94 96 97 98
111700940307     C************************************************************
111800940307     C     CTRD1         BEGSR
111900940307     C*
112000940127     C                   SETOFF                                       99
112100070525     C* non imposto pos. del cursore
112200070525     C                   Z-ADD     0             H1RIGA
112300070525     C                   Z-ADD     0             H1COLO
112400940131     C*
112500090324     C* Unità
112600090324     C                   exsr      CtrUnita
112700090324     C* Conto
112800090324     C                   exsr      CtrConto
112900100402     C* Attributo contabile
113000100402     C                   exsr      CtrAttr
113100140428     C* Stato del credito
113200140428     C                   exsr      CtrStaCRe
113300100402     C* Agente
113400100402     C                   exsr      CtrkscAge
113500100420     C* Date partita
113600100420     C                   exsr      CtrDatePar
113700100402     C* Date scadenza
113800100402     C                   exsr      CtrDateSca
113900100416     C* Date sollecito
114000100416     C                   exsr      CtrDateSol
114100100402     C* Fasi
114200100402     C                   exsr      CtrFasi
114300100407     C* Azione
114400100407     C                   exsr      CtrAzione
114401140530     C* abc
114402140530     C                   exsr      CtrABC
114403140530     C* Destinatario
114600090324     C                   exsr      CtrDestin
114700100402     C* Date attività
114800100402     C                   exsr      CtrDateAtt
114900140319     C* Data chiusura attività
115000140319     C                   exsr      CtrDateChiu
115100140317     C* fasi aperte/CHIUSE/TUTTE
115200140317     C                   exsr      CtrSELFASI
115300100413     C* partite aperte
115400140314     c*                  if        D1Stato <> xscon and
115500140314     c*                            D1Stato <> xscoff
115600140314     c*                  seton                                        5799
115700140314     c*                  endif
115800131001BRT  C* esclusione impegni (98)
115900131001 "   c                   if        D1EsclImp <> xscon and
116000131001 "   c                             D1EsclImp <> xscoff
116100131001 "   c                   seton                                        5899
116200131001BRT  c                   endif
116300070531     c*
116400140310BRT  C* Solo Insoluti
116500140310 "   c                   if        D1Insoluti <> xscon and
116600140310 "   c                             D1Insoluti <> xscoff
116700140310 "   c                   seton                                        6699
116800140310BRT  c                   endif
116900140310     c*
117000940117     C                   ENDSR
117100140317     C*----------------------------------------------------------
117200140317     C* Selezione fasi
117300140317     C*----------------------------------------------------------
117400140317     C     CtrSelFasi    BEGSR
117500140317     C*
117600140317     C                   RESET                   X07DS
117700140317     C                   MOVE      '1'           X07RIC
117800140317     C                   MOVEL     'ANDIZ000'    X07TRC
117900140317     C                   MOVEL     'DSPSELFASI'  X07CAM
118000140317     C                   MOVEL     D1FASIAPE     X07VAL
118100140317     C                   MOVE      *ON           X07ALL
118200140317     C                   CALLB     'X07VALR'
118300140317     C                   PARM                    X07DS
118400140317   B3C     X07ERR        IFEQ      '0'
118500140317     C                   MOVEL     X07DES        D1fasiaped
118600140317     c                   else
118700140317     C                   SETON                                        6199
118800140317     C                   ENDIF
118900140317     c*
119000140317     C                   ENDSR
119100090324     C*----------------------------------------------------------
119200090324     C* Controllo unita
119300090324     C*----------------------------------------------------------
119400090324     C     CtrUnita      BEGSR
119500090324     C*
119600090324     C                   CLEAR                   D1UnitaD
119700090324     C     D1Unita       IFNE      *BLANKS
119800090324     C     *in11         ANDEQ     *on
119900090324     C                   MOVE      xscsoc        UNISOCieta
120000090324     C                   MOVE      D1Unita       UNIUNIta
120100090324     C     K02Uni01      CHAIN     ANUNI01L                           21
120200090324     C     *IN21         IFEQ      *ON
120300090324     C                   SETON                                        6299
120400090324     C                   ELSE
120500090324     C                   MOVEL     UNIDESbrev    D1UnitaD
120600090324     C                   ENDIF
120700130909C2226C* Controllo autorizzazione all'unità
120800130909  "  C     *in62         Ifeq      *off
120900130909  "  C                   RESET                   XAutUniDS
121000130909  "  C                   EVAL      XAUPrf = KNmUs
121100130909  "  C                   EVAL      XAUSoc = XscSoc
121200130909  "  C                   EVAL      XAUUni = d1unita
121300130909  "  C                   CALL      'XAUTUNI'
121400130909  "  C                   PARM                    XAutUniDS
121500130909  "  C     XAUAbl        IFEQ      *OFF
121600130909  "  C                   SETON                                        7599
121700130909  "  C                   ENDIF
121800130909C2226C                   endif
121900090324     C                   ENDIF
122000090324     c*
122100090324     C                   ENDSR
122200090324     C*----------------------------------------------------------
122300090324     C* Controllo conto
122400090324     C*----------------------------------------------------------
122500090324     C     CtrConto      BEGSR
122600090324     C*
122700090324     C* Controllo del conto (niente obbligatorio)
122800090324     C                   CLEAR                   D1Descr
122900140317BRT  C                   CLEAR                   D1Kcc
123000090324 B1  c                   if        D1Kcc <> *blanks or
123100090324     c                             D1Ksc <> *blanks
123200090324     c*
123300090324     c                   if        d1kcc=*blanks
123400090324     c                   eval      d1kcc='C'
123500090324     c                   endif
123600090324     C     D1kcc         IFNE      'C'
123700090324     C     D1kcc         ANDNE     'F'
123800090324     C                   CALL      'XALLINEA'
123900090324     C                   PARM                    D1kcc
124000090324     C                   PARM      6             LEN               5 0
124100090324     C                   PARM                    XSCINP
124200090324     C                   ENDIF
124300090324     C*
124400090324     C                   CALL      'XALLINEA'
124500090324     C                   PARM                    D1KSC
124600090324     C                   PARM      8             LEN               5 0
124700090324     C                   PARM                    XSCINP
124800090324     C*
124900090324     C                   MOVE      D1KCC         $kcc
125000090324     C                   MOVE      D1KSC         $ksc
125100090324     C*
125200090324     C* reperisco capoconto se cliente/fornitore
125300090324     C     D1KCC         IFEQ      'C       '
125400090324     C     D1KCC         OREQ      'F       '
125500090324     C                   MOVE      XSCSOC        XCAPSOCIET        3
125600090324     C                   MOVE      D1KCC         XCAPKCC           6
125700090324     C                   MOVE      D1KSC         XCAPKSC           8
125800090324     C                   CALLB     'XCAPCLIFOR'
125900090324     C                   PARM                    XCAPSOCIET
126000090324     C                   PARM                    XCAPKCC
126100090324     C                   PARM                    XCAPKSC
126200090324     C                   MOVE      XCAPKCC       D1KCC
126300090324     C                   MOVE      XCAPKCC       $kcc
126400090324     C                   MOVE      XCAPKsC       $ksc
126500090324     C                   ENDIF
126600090324     C*
126700090324     C* Richiamo modulo controllo conto
126800090324     C                   EXSR      CALLMVC002
126900090324     C* Errore
127000090324     C     $Esito        IFEQ      *ON
127100090324     c                   if        d1ksc = *blanks
127200090324     C                   SETON                                        5099
127300090324     c                   else
127400090324     C                   SETON                                        505199
127500090324     c                   endif
127600090324     c*
127700090324     C                   ELSE
127800090324     C                   MOVEL     Descr002      D1Descr
127900100402     c* solo clienti
128000100402     c                   if        Snatura002 <> 'C'
128100100402     c                   if        d1ksc = *blanks
128200100402     C                   SETON                                        5399
128300100402     c                   else
128400100402     C                   SETON                                        535499
128500100402     c                   endif
128600100402     c                   endif
128700090324     C                   ENDIF
128800090324     C*
128900090324 e1  C                   ENDIF
129000090324     c*
129100090324     C                   ENDSR
129200070525     C*----------------------------------------------------------
129300070525     C* Chiamata NDMVC002
129400070525     C*----------------------------------------------------------
129500070525     C     CallMvc002    BEGSR
129600070525     C*
129700070525     C* Controllo conto
129800070525     C                   Clear                   ND002DS
129900070525     C                   EVAL      $LenOut = %Size(ND002DS)
130000070525     C                   CallB     'NDMVC002'
130100070525     C                   PARM                    xscsoc
130200070525     C                   PARM                    $kcc
130300070525     C                   PARM                    $ksc
130400100521     C                   PARM                    Wdate
130500070525     C                   PARM      *OFF          $GesMsg
130600070525     C                   PARM      *OFF          $Esito
130700070525     C                   PARM      'ND002DS'     $Struttura
130800070525     C                   PARM                    ND002DS
130900070525     C                   PARM                    $LenOut
131000070525     C*
131100070525     C                   ENDSR
131200100402     C*----------------------------------------------------------
131300100402     C* Controllo attributo contabile
131400100402     C*----------------------------------------------------------
131500100402     C     CtrAttr       BEGSR
131600100402     C*
131700100402     c* controllo nel control il codice attributo
131800100402     C                   clear                   d1attribd
131900100402     c                   if        D1attrib <> *blanks
132000100402     C                   MOVE      *ALL'0'       XTBKEY
132100100402     C                   MOVEL     PosAttr057    WRK6              6
132200100402     C                   MOVE      d1Attrib      WRK6
132300100402    >C                   MOVE      wrk6          XTBKEY
132400100402     C                   MOVE      '0'           XTBALC
132500100402     C                   MOVE      '1'           XTBRIC
132600100402     C                   MOVE      '0A1'         XTBCOD
132700100402     C                   CALLB     'XATB'
132800100402     C                   PARM                    XATBDS
132900100402     C     XTBERR        IFEQ      '0'
133000100402     C                   MOVEL     XTBUNI        Ang0a1ds
133100100402     C                   MOVEL     DES0a1        D1AttribD
133200100402     c                   else
133300100402     c                   seton                                        6899
133400100402     c                   endif
133500100402     c                   endif
133600100402     c*
133700100402     c                   ENDSR
133800100402     C/EJECT
133900140428     C*----------------------------------------------------------
134000140428     C* Controllo stato del credito
134100140428     C*----------------------------------------------------------
134200140428     C     CtrStaCre     BEGSR
134300140428     C*
134400140428     c* controllo stato del credito
134500140514     C                   clear                   d1stacreDD
134600140514     c                   if        D1stacreDa <> *blanks
134601140513     C                   MOVE      *ALL'0'       XTBKEY
134800140428     C                   MOVEL     '03'          WRK6              6
134900140514     C                   MOVEL     d1stacreDa    WRK4
134901140428     C                   MOVE      wrk4          WRK6
134902140428    >C                   MOVE      wrk6          XTBKEY
135100140428     C                   MOVE      '0'           XTBALC
135200140428     C                   MOVE      '1'           XTBRIC
135300140428     C                   MOVE      '0A1'         XTBCOD
135400140428     C                   CALLB     'XATB'
135500140428     C                   PARM                    XATBDS
135600140428     C     XTBERR        IFEQ      '0'
135700140428     C                   MOVEL     XTBUNI        Ang0a1ds
135800140514     C                   MOVEL     DES0a1        D1StaCreDD
135900140428     c                   else
136000140428     c                   seton                                        8099
136100140428     c                   endif
136200140428     c                   endif
136300140513     C*
136301140513     c* controllo stato del credito
136302140514     C                   clear                   d1stacreAD
136303140513     c                   if        D1stacreA <> *blanks
136304140513     C                   MOVE      *ALL'0'       XTBKEY
136305140513     C                   MOVEL     '03'          WRK6              6
136306140514     C                   MOVEL     d1stacreA     WRK4
136307140513     C                   MOVE      wrk4          WRK6
136308140513    >C                   MOVE      wrk6          XTBKEY
136309140513     C                   MOVE      '0'           XTBALC
136310140513     C                   MOVE      '1'           XTBRIC
136311140513     C                   MOVE      '0A1'         XTBCOD
136312140513     C                   CALLB     'XATB'
136313140513     C                   PARM                    XATBDS
136314140513     C     XTBERR        IFEQ      '0'
136315140513     C                   MOVEL     XTBUNI        Ang0a1ds
136316140514     C                   MOVEL     DES0a1        D1StaCreAD
136317140513     c                   else
136318140513     c                   seton                                        8199
136319140513     c                   endif
136320140513     c                   endif
136321140513     c*
136400140428     c                   ENDSR
136500100402     C*----------------------------------------------------------
136600100402     C* Controllo Agente
136700100402     C*----------------------------------------------------------
136800100402     C     CtrKscAge     BEGSR
136900100402     C*
137000100402     c                   clear                   d1kscageD
137100100402   1 C     D1kscage      IFNE      *blanks
137200100402     C                   CALLb     'XALLINEA'
137300100402     C                   PARM                    D1kscage
137400100402     C                   PARM      8             LEN               5 0
137500100402     C                   PARM                    XSCINP
137600100402     C* reperisco capoconto agente
137700100402     C                   EVAL      Xcapkcc  = 'F     '
137800100402     C                   MOVE      XSCSOC        XCAPSOCIET
137900100402     C                   MOVE      d1kscage      XCAPKSC
138000100402     C                   CALLB     'XCAPCLIFOR'
138100100402     C                   PARM                    XCAPSOCIET        3
138200100402     C                   PARM                    XCAPKCC           6
138300100402     C                   PARM                    XCAPKSC           8
138400100402     C* Richiamo modulo controllo conto
138500100402     c                   move      Xcapkcc       $kcc
138600100402     c                   move      Xcapksc       $ksc
138700100402     C                   EXSR      CALLMVC002
138800100402   2 C     $Esito        IFEQ      *ON
138900100402     C                   SETON                                        5599
139000100402   2xC                   ELSE
139100100402     C                   MOVEL     DESBREV002    D1kscageD
139200100402   2-C                   ENDIF
139300100402   1-C                   ENDIF
139400100402     C*
139500100402     C                   ENDSR
139600100402     c*
139700100416     c*EJECT
139800100420     C*----------------------------------------------------------
139900100420     C* Controllo DATE partita
140000100420     C*----------------------------------------------------------
140100100420     C     CtrDatePar    BEGSR
140200100420     C*
140300100420      * Data partita da a
140400100420     c                   Eval      WdtParDa= *Loval
140500100420 b1  C                   if        D1DtParDa<>   *Zeros
140600100420     C     *JobRun       Test(d)                 D1DtParDa              21
140700100420    >C     *In21         ifeq      *On
140800100420     c* non corretta
140900100420    >C                   SetOn                                        7299
141000100420     c                   else
141100100420     c* è corretta,
141200100420     c     *JobRun       move      D1DtParDa     WdtParDA
141300100420    >C                   endif
141400100420E1  >C                   endif
141500100420     C*
141600100420      * Data scadenza a
141700100420     c                   Eval      WdtParA = *hival
141800100420 b1  C                   if        D1DtParA  <>   *Zeros
141900100420     C     *JobRun       Test(d)                 D1DtParA               21
142000100420    >C     *In21         ifeq      *On
142100100420     c* non corretta
142200100420    >C                   SetOn                                        7399
142300100420     c                   else
142400100420     c* è corretta,
142500100420     c     *JobRun       move      D1DtParA      WdtParA
142600100420    >C                   endif
142700100420E1  >C                   endif
142800100420     c*
142900100420     c* limiti errati
143000100420     c                   if        WdtParDa > WdtParA
143100100420    >C                   SetOn                                        7499
143200100420     c                   endif
143300100420     c
143400100420     C                   ENDSR
143500100420     c*EJECT
143600100402     C*----------------------------------------------------------
143700100402     C* Controllo DATE scadenza
143800100402     C*----------------------------------------------------------
143900100402     C     CtrDateSca    BEGSR
144000100402     C*
144100100402      * Data scadenza  da
144200100402     c                   Eval      WdtScaDa= *Loval
144300100402 b1  C                   if        D1DtScaDa<>   *Zeros
144400100402     C     *JobRun       Test(d)                 D1DtScaDa              21
144500100402    >C     *In21         ifeq      *On
144600100402     c* non corretta
144700100402    >C                   SetOn                                        6999
144800100402     c                   else
144900100402     c* è corretta,
145000100402     c     *JobRun       move      D1DtScaDa     WdtScaDA
145100100402    >C                   endif
145200100402E1  >C                   endif
145300100402     C*
145400100402      * Data scadenza a
145500100402     c                   Eval      WdtScaA = *hival
145600100402 b1  C                   if        D1DtScaA  <>   *Zeros
145700100402     C     *JobRun       Test(d)                 D1DtScaA               21
145800100402    >C     *In21         ifeq      *On
145900100402     c* non corretta
146000100402    >C                   SetOn                                        7099
146100100402     c                   else
146200100402     c* è corretta,
146300100402     c     *JobRun       move      D1DtScaA      WdtScaA
146400100402    >C                   endif
146500100402E1  >C                   endif
146600100402     c*
146700100402     c* limiti errati
146800100402     c                   if        WdtScaDa > WdtScaA
146900100402    >C                   SetOn                                        7199
147000100402     c                   endif
147100100402     c
147200100402     C                   ENDSR
147300100416     c*EJECT
147400100416     C*----------------------------------------------------------
147500100416     C* Controllo DATE sollecito
147600100416     C*----------------------------------------------------------
147700100416     C     CtrDateSol    BEGSR
147800100416     C*
147900100416      * Data sollecito da
148000100416     c                   Eval      WdtSolDa= *Loval
148100100416 b1  C                   if        D1DtSolDa<>   *Zeros
148200100416     C     *JobRun       Test(d)                 D1DtSolDa              21
148300100416    >C     *In21         ifeq      *On
148400100416     c* non corretta
148500100416    >C                   SetOn                                        5899
148600100416     c                   else
148700100416     c* è corretta,
148800100416     c     *JobRun       move      D1DtSolDa     WdtSolDA
148900100416    >C                   endif
149000100416E1  >C                   endif
149100100416     C*
149200100416      * Data sollecito a
149300100416     c                   Eval      WdtSolA = *hival
149400100416 b1  C                   if        D1DtSolA  <>   *Zeros
149500100416     C     *JobRun       Test(d)                 D1DtSolA               21
149600100416    >C     *In21         ifeq      *On
149700100416     c* non corretta
149800100416    >C                   SetOn                                        5999
149900100416     c                   else
150000100416     c* è corretta,
150100100416     c     *JobRun       move      D1DtSolA      WdtSolA
150200100416    >C                   endif
150300100416E1  >C                   endif
150400100416     c*
150500100416     c* limiti errati
150600100416     c                   if        WdtSolDa > WdtSolA
150700100416    >C                   SetOn                                        6099
150800100416     c                   endif
150900100416     c
151000100416     C                   ENDSR
151100100402     C*----------------------------------------------------------
151200100402     C* Controllo fasi
151300100402     C*----------------------------------------------------------
151400100402     C     CtrFasi       BEGSR
151500100402     C*
151600100402     c                   if        D1FaseDa > d1FaseA
151700100402     c                   seton                                        6799
151800100402     c                   endif
151900100402     c*
152000100402     C                   ENDSR
152100100407     C*----------------------------------------------------------
152200100407     C* Controllo azione
152300100407     C*----------------------------------------------------------
152400100407     C     CtrAzione     BEGSR
152500100407     C*
152600100407     c                   clear                   D1AzioneD
152700100407     c                   if        d1Azione <> *blanks
152800100407     C                   MOVE      *ALL'0'       XTBKEY
152900100407    >C                   MOVE      D1Azione      XTBKEY
153000100407     C                   MOVE      '0'           XTBALC
153100100407     C                   MOVE      '1'           XTBRIC
153200100407     C                   MOVE      '047'         XTBCOD
153300100407     C                   CALLB     'XATB'
153400100407     C                   PARM                    XATBDS
153500100407     C     XTBERR        IFEQ      '0'
153600100407     C                   MOVEL     XTBUNI        Ang047ds
153700100407     c                   movel     Des047        D1AzioneD
153800100407     c                   else
153900100407     c                   seton                                        5699
154000100407     c                   endif
154100100407     c                   endif
154200100407     c*
154300100407     C                   ENDSR
154301140530     C*----------------------------------------------------------
154302140530     C* Controllo abc
154303140530     C*----------------------------------------------------------
154304140530     C     CtrABC        BEGSR
154305140530     C*
154306140530     c                   clear                   D1AbcDes
154307140530     c                   if        d1Abc <> *blanks
154308140530     C                   MOVE      *ALL'0'       XTBKEY
154309140530    >C                   MOVE      D1ABC         XTBKEY
154310140530     C                   MOVE      '0'           XTBALC
154311140530     C                   MOVE      '1'           XTBRIC
154312140530     C                   MOVE      'C48'         XTBCOD
154313140530     C                   CALLB     'XATB'
154314140530     C                   PARM                    XATBDS
154315140530     C     XTBERR        IFEQ      '0'
154316140530     C                   MOVEL     XTBUNI        AngC48ds
154317140530     c                   movel     DesC48        D1abcDes
154318140530     c                   else
154319140530     c                   seton                                        8299
154320140530     c                   endif
154321140530     c                   endif
154322140530     c*
154323140530     C                   ENDSR
154400090324     C*----------------------------------------------------------
154500090324     C* Controllo destinatario
154600090324     C*----------------------------------------------------------
154700090324     C     CtrDestin     BEGSR
154800090324     C*
154900090324      * Controllo codice destinatario
155000090324     c                   if        d1destin  <> *blanks
155100090324     C                   CLEAR                   AUTDS
155200090324     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
155300090324     C                   Z-ADD     0             XCAGRP                         GRUPPO
155400090324     C                   MOVEL     d1Destin      XCAPRF                         PROFILO
155500090324    >C                   MOVE      'CU'          XCATCK                         TIPO CTRL
155600090324    >C                   MOVE      ' '           XCANAU                         RICH.XNOAUT
155700090324     C                   EVAL      Lungutn= %Size(AnUtn00f)
155800090324     C                   CALLB     'XCHKAUT'
155900090324     C                   PARM                    AUTDS
156000090324     C                   PARM                    PARMOK
156100090324     C                   PARM                    ANUTNParm
156200090324     C                   PARM      71            LungUTN
156300090324b1   C     XCARTN        IFne      0
156400090324     c                   seton                                        6599
156500090324     C                   ENDIF
156600090324     c                   endif
156700090324     C*
156800090324     C                   ENDSR
156900090324     C*----------------------------------------------------------
157000100402     C* Controllo data attività
157100090324     C*----------------------------------------------------------
157200100402     C     CtrDateAtt    BEGSR
157300090324     C*
157400100402      * Data attività da
157500100401     c                   Eval      WdtAttda= *Loval
157600100401 b1  C                   if        D1dtAttDa<>   *Zeros
157700100401     C     *JobRun       Test(d)                 D1dtAttDa              21
157800090324    >C     *In21         ifeq      *On
157900090324     c* non corretta
158000090324    >C                   SetOn                                        5299
158100090324     c                   else
158200090324     c* è corretta,
158300100401     c     *JobRun       move      D1dtAttDA     WdtAttDa
158400090324    >C                   endif
158500090324E1  >C                   endif
158600090324     C*
158700100402      * Data attività a
158800100401     c                   Eval      WdtAttA = *hival
158900100401 b1  C                   if        D1dtAttA <>   *Zeros
159000100401     C     *JobRun       Test(d)                 D1dtAttA               21
159100090324    >C     *In21         ifeq      *On
159200090324     c* non corretta
159300090324    >C                   SetOn                                        6399
159400090324     c                   else
159500090324     c* è corretta,
159600100401     c     *JobRun       move      D1dtAttA      WdtAttA
159700090324    >C                   endif
159800090324E1  >C                   endif
159900090324     c*
160000090324     c* limiti errati
160100100401     c                   if        WdtAttDa > WdtAttA
160200090324    >C                   SetOn                                        6499
160300090324     c                   endif
160400090324     c*
160500090324     C                   ENDSR
160600140319     C*----------------------------------------------------------
160700140319     C* Controllo data chiusura
160800140319     C*----------------------------------------------------------
160900140319     C     CtrDateChiu   BEGSR
161000140319     C*
161100140319      * Data chiusura da
161200140319     c                   Eval      WdtChida= *Loval
161300140319 b1  C                   if        D1dtChiDa<>   *Zeros
161400140319     C     *JobRun       Test(d)                 D1dtChiDa              21
161500140319    >C     *In21         ifeq      *On
161600140319     c* non corretta
161700140319    >C                   SetOn                                        7699
161800140319     c                   else
161900140319     c* è corretta,
162000140319     c     *JobRun       move      D1dtChiDA     WdtChiDa
162100140319    >C                   endif
162200140319E1  >C                   endif
162300140319     C*
162400140319      * Data attività a
162500140319     c                   Eval      WdtChiA = *hival
162600140319 b1  C                   if        D1dtChiA <>   *Zeros
162700140319     C     *JobRun       Test(d)                 D1dtChiA               21
162800140319    >C     *In21         ifeq      *On
162900140319     c* non corretta
163000140319    >C                   SetOn                                        7799
163100140319     c                   else
163200140319     c* è corretta,
163300140319     c     *JobRun       move      D1dtChiA      WdtChiA
163400140319    >C                   endif
163500140319E1  >C                   endif
163600140319     c*
163700140319     c* limiti errati
163800140319     c                   if        WdtChiDa > WdtChiA
163900140319    >C                   SetOn                                        7899
164000140319     c                   endif
164100140319     c*
164200140319     C                   ENDSR
164300090409     C*
164400950410     C************************************************************
164500950410     C* DEFINIZIONE DELL'ORDINAMENTO
164600950410     C************************************************************
164700950410     C     DEFORD        BEGSR
164800950410     C*
164900950410     C* definisco l'ordinamento della lista a seconda dei valori
165000950410     C* impostati nel video di parzializzazione
165100950410     C*
165200950410     C                   Z-ADD     1             $ORD
165300070524     C*
165400950410     C                   ENDSR
165500960202     C/EJECT
165600960202     C************************************************************
165700960202     C* Chiamata programma con tasto personalizzato
165800960202     C************************************************************
165900960202     C     FPerS1        BEGSR
166000960202     C*
166100960202     C* salvo la DS di ingresso
166200090323    >C                   MOVEL     NDCL76DS      $SaveDS
166300960202     C*
166400090323    >C                   MOVEL     NDCL76DS      KPJBU
166500960202     C                   CALL      $PGF2(PosFun)                        22
166600960202     C                   PARM                    KPJBA
166700960202     C                   PARM                    NOMPGM
166800960202     C*
166900960202     C* ripristino la DS di ingresso
167000090323    >C                   MOVEL     $SaveDS       NDCL76DS
167100960202     C*
167200960202     C                   ENDSR
167300130911C2226C/EJECT
167400130911  "  C************************************************************
167500130911  "  C* Chiamata programma con tasto personalizzato
167600130911  "  C************************************************************
167700130911C2226C     FPerS2        BEGSR
167800130911     C*
167900130911     C* salvo la DS di ingresso
168000130911    >C                   MOVEL     NDCL76DS      $SaveDS
168100130911     C*
168200130911    >C                   MOVEL     NDCL76DS      KPJBU
168300130911     C                   CALL      $PGF2(PosFun)                        22
168400130911     C                   PARM                    KPJBA
168500130911     C                   PARM                    NOMPGM
168600130911     C*
168700130911     C* ripristino la DS di ingresso
168800130911    >C                   MOVEL     $SaveDS       NDCL76DS
168900130911     C*
169000130911C2226C                   ENDSR
169100940224     C/EJECT
169200940127     C************************************************************
169300940131     C* GESTIONE LISTA
169400940127     C************************************************************
169500940127     C     GESS1         BEGSR
169501140604     C*
169600940223     C* inizializzazione videata
169700940223     C     $INZS1        IFEQ      *ON
169800940127     C                   EXSR      INZS1
169900940223     C                   MOVE      *OFF          $INZS1
170000940127     C                   ENDIF
170100940223     C* emissione piede videata se proveniente da altra
170200940223     C* salvataggio valore formato di provenienza
170300940202     C     $LASTV        IFNE      'S1'
170400940202     C                   WRITE     FMTT1
170500940224     C                   EXSR      GESPIE
170600940202     C                   MOVE      $LASTV        $LASTG
170700940202     C                   MOVE      'S1'          $LASTV
170800941027     C                   ENDIF
170900940223     C*
171000940223     C     WMAX          IFEQ      0
171100940211     C                   WRITE     FMTD3                                        NO RCD
171200940211     C                   ENDIF
171300960202     C* Inizializzazione indice tasti funzione personalizzati
171400960202     C                   Eval      PosFun = 25
171500940127     C*
171600940131     C                   EXFMT     FMTC1
171700960605     C*
171800960605     C* azzero la posizione del cursore
171900960605     C                   Z-ADD     0             H1RIGA
172000960605     C                   Z-ADD     0             H1COLO
172100970912     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
172200970912     C* di refresh dello sfondo
172300970912     C                   MOVE      '1'           WExf
172400960605     C*
172500940204     C     C1NRR         IFNE      0
172600940204     C                   Z-ADD     C1NRR         WSFL
172700940204     C                   ENDIF
172800951020     C                   IF        SFLNRR > 0
172900951020     C                   Z-ADD     SFLNRR        C1RCD
173000951020     C                   ENDIF
173100960122     C* inizializzazione indice opzioni personalizzate
173200960122     C                   Eval      PosOpz = 0
173300951005     C* CONTROLLO ABILITAZIONE TASTI
173400951005     C* INPUT
173500951005     C* $FC2 = Abilitazioni videata S1
173600951005     C* OUTPUT
173700951005     C* *IN97 + *IN99 Errori
173800951005     C                   MOVEA     $FC2          $FC
173900951005     C                   EXSR      CTRKEY
174000941205     C*
174100940506     C     *IN99         IFEQ      *OFF
174200940127     C*
174300941027     C                   SELECT
174400940127     C* F3=Fine
174500940506     C     $TASTO        WHENEQ    F03
174600940309     C                   EXSR      F03S1
174700130924C2226C* F5=Refresh
174800130924  "  C     $TASTO        WHENEQ    F05
174900130924C2226C                   EXSR      F05S1
175000941206     C* F8=Successivo
175100941206     C     $TASTO        WHENEQ    F08
175200941206     C                   EXSR      F08S1
175300940131     C* F12=Ritorno
175400940506     C     $TASTO        WHENEQ    F12
175500940309     C                   EXSR      F12S1
175600090325     C* F13=Selezione multipla
175700090325     C     $TASTO        WHENEQ    F13
175800130924C2226C                   Clear                   $tastoSav2
175900090325     C                   Eval      $TastoSav=$Tasto
176000130924C2226C                   Eval      PrimaVolta = 0
176100090325     C                   EXSR      F13S1
176200940131     C* F17=Parzializzazioni
176300940506     C     $TASTO        WHENEQ    F17
176400940309     C                   EXSR      F17S1
176500940131     C* F18=Modifica valori assunti
176600940506     C     $TASTO        WHENEQ    F18
176700940309     C                   EXSR      F18S1
176800940224     C* F23=Altre opzioni
176900940506     C     $TASTO        WHENEQ    F23
177000940309     C                   EXSR      F23S1
177100940224     C* F24=Altri tasti
177200940506     C     $TASTO        WHENEQ    F24
177300940309     C                   EXSR      F24S1
177400940127     C* ROLL-UP
177500090330     C     $TASTO        WHENEQ    ROLLUP
177600090330     C                   EXSR      ROLS1
177700960202     C* Personalizzata
177800960202     C     $PGF2(PosFun) Whenne    *BLANK
177900960202     C                   EXSR      FperS1
178000940127     C*
178100941027     C                   OTHER
178200940127     C* CONTROLLO DATI
178300940131     C                   EXSR      CTRC1
178400940201     C     *IN99         IFEQ      *OFF
178500940131     C                   EXSR      CTRS1
178600941027     C                   ENDIF
178700941205     C*
178800941027     C                   ENDSL
178900940127     C*
179000940506     C                   ENDIF
179100941205     C*
179200940127     C                   ENDSR
179300940224     C/EJECT
179400940127     C************************************************************
179500940131     C* INIZIALIZZAZIONE LISTA
179600940127     C************************************************************
179700940127     C     INZS1         BEGSR
179800090325     C*appoggio x F13
179900090325     C                   Clear                   $TastoSav
180000090325     C                   eval      PrimaVolta=0
180100130924     C                   Clear                   $TastoSav2
180200940302     C* pulizia SFL
180300940128     C                   SETOFF                                         3031
180400940131     C                   WRITE     FMTC1
180500940128     C                   SETON                                          31
180600940127     C*
180700940128     C* riempimento opzioni
180800940223     C                   MOVEA     $TX1          $TX
180900940223     C                   MOVEA     $FL1          $FL
181000940309     C                   Z-ADD     0             WCOZ
181100960117     C                   Z-ADD     0             WMXO
181200940225     C*  equivale al num.di elementi della schiera $TX1
181300960117     C                   Z-add     OpzLen        WMXO
181400950906     C                   Z-ADD     $C1Op1        $L01
181500950906     C                   Z-ADD     $C1Op2        $L02
181600940128     C                   EXSR      RIEOPZ
181700941214     C                   Z-ADD     23            X
181800941214     C                   MOVE      FF23          $FC2(X)
181900130911     C                   MOVEL     WOPZ1         C1OPZ
182000060224     C                   MOVEL     $ALLOPT       H1ALLOPT
182100940608     C                   MOVEL     WOPZ2         C1OPZ2
182200940225     C                   Z-ADD     WCOZ          C1COZ
182300070531     c* riga unica opzioni
182400070531     c                   eval      $A = %triml(c1opz)
182500070531     c                   eval      $B = %triml(c1opz2)
182600070531     c     $A            cat(P)    $B:2          C1opz1
182700100402     c*
182800100402     c                   eval      C1Unita   =D1Unita
182900100402     c                   eval      C1kcc     =D1kcc
183000100402     c                   eval      C1PosAttrD=D1PosAttrD
183100100402     c                   eval      C1attrib=D1Attrib
183200100402     c                   eval      C1attribD=D1AttribD
183300130911C2226c                   eval      C1Importo=0
183400130912  "  C                   If        $pre = *on
183500130911  "  c                   eval      C1Importo=S2Importo
183600130911C2226C                   Endif
183700940128     C*
183800940128     C* CARICAMENTO SFL A PAGINE
183900940128     C*
184000940201     C                   Z-ADD     0             S1NRR
184100940128     C                   Z-ADD     0             WMAX
184200940224     C*
184300940224     C* Posizionamento su file pilota
184400940302     C                   CLEAR                   C1POS
184500941214     C                   EXSR      SETANA
184600940302     C* ripristino i valori del posizionamento
184700940302     C                   MOVE      W1POS         C1POS
184800940131     C*
184900940201     C* Lettura file pilota
185000941214     C                   EXSR      REDANA
185100940131     C*
185200970318     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
185300970318     C                   IF        $LastV = 'D1'
185400970318     C                   Z-ADD     SFLPAG        WPAG
185500970318     C                   ENDIF
185600940127     C                   EXSR      ROLS1
185700970318     C* imposto il caricamento sfl del numero massimo di righe per pagina
185800970318     C                   Z-ADD     SFLPAG        WPAG
185900940127     C*
186000940127     C                   ENDSR
186100940127     C/EJECT
186200940127     C************************************************************
186300940131     C* CARICAMENTO PAGINA LISTA
186400940127     C************************************************************
186500940127     C     ROLS1         BEGSR
186600940127     C*
186700940128     C                   SETOFF                                       32
186800940223     C                   Z-ADD     0             Y
186900940127     C                   Z-ADD     WMAX          S1NRR
187000940127     C*
187100940127     C* Leggo dal file anagrafico per caricare la lista
187200940127     C*
187300950606     C     $EndFile      DOWEQ     *OFF
187400940201     C     Y             ANDLT     WPAG
187500090325     c     S1nrr         andlt     9999
187600940127     C*
187700940131     C                   EXSR      RIES1
187800940127     C*
187900940127     C                   ADD       1             S1NRR
188000940127     C                   ADD       1             Y
188100940127     C*
188200940131     C                   WRITE     FMTS1
188300940131     C*
188400941214     C                   EXSR      REDANA
188500940128     C*
188600941027     C                   ENDDO
188700940127     C*
188800940223     C                   Z-ADD     S1NRR         WMAX                 30
188900940127     C*
189000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
189100940127     C*
189200940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
189300940127     C                   MVR                     RESTO             3 0
189400940131     C     PAGINE        MULT      SFLPAG        C1RCD
189500941027     C     RESTO         IFGT      0
189600940127     C                   ADD       1             C1RCD
189700941027     C                   ELSE
189800940407     C                   SUB       SFLPAG        C1RCD
189900940407     C                   ADD       1             C1RCD
190000941027     C                   ENDIF
190100940128     C*
190200940127     C                   ENDSR
190300940224     C/EJECT
190400940224     C************************************************************
190500940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
190600940224     C************************************************************
190700941214     C     SETANA        BEGSR
190800940224     C*
190900070524     c                   exsr      InzSql
191000940224     C*
191100940224     C                   ENDSR
191200940128     C/EJECT
191300940128     C************************************************************
191400940131     C* LETTURA RCD ARCHIVIO PILOTA
191500940128     C************************************************************
191600941214     C     REDANA        BEGSR
191700940128     C*
191800950606     C                   MOVEL     *OFF          $EndFile
191900940131     C                   MOVEL     *OFF          $RCDOK
192000970910     C*
192100950606     C     $EndFile      DOUEQ     *ON
192200940131     C     $RCDOK        OREQ      *ON
192300070525     C*
192400070525     c                   exsr      FetchSql
192500070525     c*
192600070525     C                   if        SqlCod<0 or SqlCod=100
192700070525     C                   MOVE      *ON           *IN21
192800070525     C                   ELSE
192900070525     C                   MOVE      *OFF          *IN21
193000070525     C                   END
193100940131     C*
193200950606     C                   MOVE      *IN21         $EndFile
193300940131     C*
193400950606     C     $EndFile      IFEQ      *OFF
193500100407     C                   EXSR      SELANA
193600941027     C                   ENDIF
193700940131     C*
193800941027     C                   ENDDO
193900940131     C*
194000000208     C                   MOVE      $EndFile      *IN33                          SflEnd
194100940131     C*
194200940131     C                   ENDSR
194300940224     C/EJECT
194400130911C2226C************************************************************
194500130911  "  C* GESTIONE PRELISTA
194600130911  "  C************************************************************
194700130911C2226C     GESS2         BEGSR
194800130911     C* inizializzazione videata
194900130911     C     $INZS2        IFEQ      *ON
195000130911     C                   EXSR      INZS2
195100130911     C                   MOVE      *OFF          $INZS2
195200130911     C                   ENDIF
195300130911     C* emissione piede videata se proveniente da altra
195400130911     C* salvataggio valore formato di provenienza
195500130911     C     $LASTV        IFNE      'S2'
195600130911     C                   WRITE     FMTT1
195700130911     C                   EXSR      GESPIE
195800130911     C                   MOVE      $LASTV        $LASTG
195900130911     C                   MOVE      'S2'          $LASTV
196000130911     C                   ENDIF
196100130911     C*
196200130911     C     WMAXS2        IFEQ      0
196300130911     C                   WRITE     FMTD3                                        NO RCD
196400130911     C                   ENDIF
196500130911     C* Inizializzazione indice tasti funzione personalizzati
196600130911     C                   Eval      PosFun = 25
196700130911     C*
196800130911     C                   EXFMT     FMTC2
196900130911     C*
197000130911     C* azzero la posizione del cursore
197100130911     C                   Z-ADD     0             H1RIGA
197200130911     C                   Z-ADD     0             H1COLO
197300130911     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
197400130911     C* di refresh dello sfondo
197500130911     C                   MOVE      '1'           WExf
197600130911     C*
197700130911     C     C2NRR         IFNE      0
197800130911     C                   Z-ADD     C2NRR         WSFL2
197900130911     C                   ENDIF
198000130911     C                   IF        SFLNRR > 0
198100130911     C                   Z-ADD     SFLNRR        C2RCD
198200130911     C                   ENDIF
198300130911     C* inizializzazione indice opzioni personalizzate
198400130911     C                   Eval      PosOpz = 0
198500130911     C* CONTROLLO ABILITAZIONE TASTI
198600130911     C* INPUT
198700130911     C* $FC2 = Abilitazioni videata S1
198800130911     C* OUTPUT
198900130911     C* *IN97 + *IN99 Errori
199000130911     C                   MOVEA     $FC2          $FC
199100130911     C                   EXSR      CTRKEY
199200130911     C*
199300130911     C     *IN99         IFEQ      *OFF
199400130911     C*
199500130911     C                   SELECT
199600130911     C* F3=Fine
199700130911     C     $TASTO        WHENEQ    F03
199800130911     C                   EXSR      F03S2
199900130911     C* F8=Successivo
200000130911     C     $TASTO        WHENEQ    F08
200100130911     C                   EXSR      F08S2
200200130911     C* F12=Ritorno
200300130911     C     $TASTO        WHENEQ    F12
200400130911     C                   EXSR      F12S2
200500130911     C* F13=Selezione multipla
200600130911     C     $TASTO        WHENEQ    F13
200700130911     C                   EXSR      F13S2
200800130911     C* F17=Parzializzazioni
200900130911     C     $TASTO        WHENEQ    F17
201000130911     C                   EXSR      F17S2
201100130911     C* F18=Modifica valori assunti
201200130911     C     $TASTO        WHENEQ    F18
201300130911     C                   EXSR      F18S2
201400130911     C* F23=Altre opzioni
201500130911     C     $TASTO        WHENEQ    F23
201600130911     C                   EXSR      F23S2
201700130911     C* F24=Altri tasti
201800130911     C     $TASTO        WHENEQ    F24
201900130911     C                   EXSR      F24S2
202000130911     C* ROLL-UP
202100130911     C     $TASTO        WHENEQ    ROLLUP
202200130911     C                   EXSR      ROLS2
202300130911     C* Personalizzata
202400130911     C     $PGF2(PosFun) Whenne    *BLANK
202500130911     C                   EXSR      FperS2
202600130911     C*
202700130911     C                   OTHER
202800130911     C* CONTROLLO DATI
202900130911     C                   EXSR      CTRC2
203000130911     C     *IN99         IFEQ      *OFF
203100130911     C                   EXSR      CTRS2
203200130911     C                   ENDIF
203300130911     C*
203400130911     C                   ENDSL
203500130911     C*
203600130911     C                   ENDIF
203700130911     C*
203800130911C2226C                   ENDSR
203900130911C2226C/EJECT
204000130911  "  C************************************************************
204100130911  "  C* INIZIALIZZAZIONE PRELISTA
204200130911  "  C************************************************************
204300130911C2226C     INZS2         BEGSR
204400130911     C* pulizia SFL
204500130911     C                   SETOFF                                         3031
204600130911     C                   WRITE     FMTC2
204700130911     C                   SETON                                          31
204800130911     C*
204900130911     C* riempimento opzioni
205000130911     C                   MOVEA     $TX1          $TX
205100130912     C                   MOVEA     $FL2          $FL
205200130911     C                   Z-ADD     0             WCOZ
205300130911     C                   Z-ADD     0             WMXO
205400130911     C*  equivale al num.di elementi della schiera $TX1
205500130911     C                   Z-add     OpzLen        WMXO
205600130911     C                   Z-ADD     $C2Op1        $L01
205700130911     C                   Z-ADD     $C2Op2        $L02
205800130911     C                   EXSR      RIEOPZ
205900130911     C                   Z-ADD     23            X
206000130911     C                   MOVE      FF23          $FC2(X)
206100130912     C                   MOVEL     WOPZ1         C2OPZ
206200130912     C                   MOVEL     $ALLOPT       H1ALLOPT
206300130912     C                   MOVEL     WOPZ2         C2OPZ2
206400130912     C                   Z-ADD     WCOZ          C2COZ
206500130912     C* riga unica opzioni
206600130912     C                   eval      $A = %triml(c2opz)
206700130912     C                   eval      $B = %triml(c2opz2)
206800130912     C     $A            cat(P)    $B:2          C2opz1
206900130912     C*
207000130912     C                   eval      C2Unita   =D1Unita
207100130912     C                   eval      C2kcc     =D1kcc
207200130912     C                   eval      C2PosAttrD=D1PosAttrD
207300130912     C                   eval      C2attrib=D1Attrib
207400130912     C                   eval      C2attribD=D1AttribD
207500130912     C* Intestazione a seconda del raggruppamento
207600130912     C                   If        $RgrPre = 'P'
207700130912     C                   Eval      C2CodiceD ='P.Iva/Codice fiscale'
207800130912     C                   Else
207900130912     C                   Eval      C2CodiceD ='Soggetto            '
208000130912     C                   Endif
208100130912     C*
208200130912     C* CARICAMENTO SFL A PAGINE
208300130912     C*
208400130912     C                   Z-ADD     0             S2NRR
208500130912     C                   Z-ADD     0             WMAXS2
208600130912     C*
208700130911     C* Posizionamento su file pilota
208800130911     C                   CLEAR                   C2POS
208900130911     C                   EXSR      SETANAPRE
209000130911     C* ripristino i valori del posizionamento
209100130911     C                   MOVE      W2POS         C2POS
209200130911     C*
209300130911     C* Lettura file pilota
209400130911     C                   EXSR      REDANAPRE
209500130911     C*
209600130911     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
209700130911     C                   IF        $LastV = 'D1'
209800130911     C                   Z-ADD     SFLPAG        WPAG2
209900130911     C                   ENDIF
210000130911     C                   EXSR      ROLS2
210100130911     C* imposto il caricamento sfl del numero massimo di righe per pagina
210200130911     C                   Z-ADD     SFLPAG        WPAG2
210300130911     C*
210400130911C2226C                   ENDSR
210500130911  "  C/EJECT
210600130911  "  C************************************************************
210700130911  "  C* CARICAMENTO PAGINA LISTA
210800130911  "  C************************************************************
210900130912C2226C     ROLS2         BEGSR
211000130912     C*
211100130912     C                   SETOFF                                       32
211200130912     C                   Z-ADD     0             Y
211300130912     C                   Z-ADD     WMAXS2        S2NRR
211400130912     C*
211500130912     C* Leggo dal file anagrafico per caricare la lista
211600130912     C*
211700130912     C     $EndFilePre   DOWEQ     *OFF
211800130912     C     Y             ANDLT     WPAG2
211900130912     C     S2nrr         andlt     9999
212000130912     C*
212100130912     C                   EXSR      RIES2
212200130912     C*
212300130912     C                   ADD       1             S2NRR
212400130912     C                   ADD       1             Y
212500130912     C*
212600130912     C                   WRITE     FMTS2
212700130912     C*
212800130912     C                   EXSR      REDANAPRE
212900130911     C*
213000130911     C                   ENDDO
213100130911     C*
213200130911     C                   Z-ADD     S2NRR         WMAXS2               30
213300130911     C*
213400130911     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
213500130911     C*
213600130911     C     S2NRR         DIV       SFLPAG        PAGINE            4 0
213700130911     C                   MVR                     RESTO             3 0
213800130911     C     PAGINE        MULT      SFLPAG        C2RCD
213900130911     C     RESTO         IFGT      0
214000130911     C                   ADD       1             C2RCD
214100130911     C                   ELSE
214200130911     C                   SUB       SFLPAG        C2RCD
214300130911     C                   ADD       1             C2RCD
214400130911     C                   ENDIF
214500130911     C*
214600130911C2226C                   ENDSR
214700130911  "  C/EJECT
214800130911  "  C************************************************************
214900130911  "  C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
215000130911  "  C************************************************************
215100130911C2226C     SETANAPRE     BEGSR
215200130911     C*
215300130911     C                   exsr      InzSqlpre
215400130918     C*
215500130919     C                   exsr      Cartab
215600130919     C*
215700130919     C                   exsr      InzSqlTab
215800130919     C*
215900130918C2226C                   ENDSR
216000130918  "  C/EJECT
216100130918  "  C************************************************************
216200130919  "  C* CARICAMENTO tabella
216300130918  "  C************************************************************
216400130919C2226C     CARTAB        BEGSR
216500130920      *
216600130920     C                   exsr      clocurtab
216700130920      *
216800130920     C/EXEC SQL
216900130920     C+ DROP TABLE qtemp/NDRCATEMP
217000130920     C/END-EXEC
217100130920      *
217200130920     C                   if        SqlCod<0
217300130920     C                             and SqlCod<>-204
217400130920     C                   dump(a)
217500130920     C                   exsr      endpgm
217600130920     C                   endif
217700130919     C/EXEC SQL
217800130919     C+ CREATE TABLE QTEMP/NDRCATEMP (KCCPRE CHAR ( 6) NOT NULL WITH
217900130919     C+ DEFAULT, PARTIVAPRE CHAR ( 20) NOT NULL WITH DEFAULT, CDFISCPRE
218000130919     C+ CHAR ( 20) NOT NULL WITH DEFAULT, SOGGPRE CHAR ( 8) NOT NULL WITH
218100130919     C+ DEFAULT, DESCRPRE CHAR ( 44) NOT NULL WITH DEFAULT, IMPPRE DEC (
218200130919     C+ 19, 3) NOT NULL WITH DEFAULT)
218300130919     C/END-EXEC
218400130919     C                   if        SqlCod<0
218500130919     C                   dump(a)
218600130919     C                   exsr      endpgm
218700130919     C                   endif
218800130918     C*
218900130919     C                   movel     *off          $First
219000130918     C                   Clear                   $PKcc
219100130918     C                   Clear                   $PPartiva
219200130918     C                   Clear                   $PCdFisc
219300130918     C                   Clear                   $PSogg
219400130919     C                   Clear                   ImpPre
219500130919     C                   Clear                   DescrPre
219600130918     C*
219700130918     C                   DO        *hival
219800130918     C*
219900130918     C                   exsr      FetchSqlPre
220000130918     C*
220100130918     C                   If        SqlCod<0 or SqlCod=100
220200130918     C                   LEAVE
220300130918     C                   End
220400130918     C*
220500130918     C                   Reset                   NDC981DS
220600130919     C                   Eval      Societa981 = ds2Societa
220700130919     C                   Eval      Kcc981 = ds2Kcc
220800151027     C***                Eval      UniTa981 = ds2Unita
220801151027     C                   clear                   UniTa981
220900130918     C                   Eval      Partiva981 =  Ds2Partiva
221000130918     C                   Eval      Cdfisc981  =  Ds2CdFisc
221100130918     C                   Eval      Sogg981    =  Ds2Sogg
221200130919     C                   Call      'NDC981R'
221300130919     C                   parm                    Ndc981DS
221400130918     C*
221500130919     C                   If        Errore981 =  *On or
221600130919     C                             impscad981 = 0
221700130918     C                   Iter
221800130918     C                   Endif
221900131021      * Nuovo Filtro su importo scaduto
222000131022     C***                If        impscad981 < d1impda or
222100131022     C***                          d1impa > *zeros and
222200131022     C***                          impscad981 > d1impa
222300131022     C***                Iter
222400131022     C***                Endif
222500130919     C*
222600130919     C                   If        $First = *off
222700130919     C                   Movel     Ds2Kcc        $PKcc
222800130919     C                   Movel     Ds2Partiva    $PPartiva
222900130919     C                   Movel     Ds2CdFisc     $PCdFisc
223000130919     C                   Movel     Ds2Sogg       $PSogg
223100130919     C                   Eval      $First = *on
223200130919     C                   Endif
223300130918     C*
223400130919     C                   Select
223500130919     C*
223600130918     C                   When      $RgrPre = 'P'
223700130919     C*
223800130918     C                   IF        Ds2Kcc + Ds2Partiva + Ds2CdFisc  <>
223900130918     C                             $PKcc + $PPartiva  + $PCdFisc
224000130919     C                   Exsr      InsertTab
224100130919     C                   Movel     Ds2Kcc        $PKcc
224200130918     C                   Movel     Ds2Partiva    $PPartiva
224300130918     C                   Movel     Ds2CdFisc     $PCdFisc
224400130919     C                   Movel     Ds2Sogg       $PSogg
224500130919     C                   Clear                   Imppre
224600130919     C                   Clear                   descrpre
224700130918     C                   Endif
224800130918     C*
224900130918     C                   Other
225000130919     C*
225100130918     C                   IF        Ds2Kcc + Ds2Sogg <>
225200130918     C                             $PKcc + $PSogg
225300130919     C                   Exsr      InsertTab
225400130919     C                   Movel     Ds2Kcc        $PKcc
225500130918     C                   Movel     Ds2Partiva    $PPartiva
225600130918     C                   Movel     Ds2CdFisc     $PCdFisc
225700130919     C                   movel     Ds2Sogg       $PSogg
225800130919     C                   Clear                   Imppre
225900130919     C                   Clear                   descrpre
226000130919     C                   Endif
226100130918     C*
226200130918     C                   EndSl
226300130918     C*
226400130918     C                   Add       ImpScad981    ImpPre
226500130919     C                   movel     Ds2descr      descrpre
226600130918     C*
226700130918     C                   ENDDO
226800130920     C* Anche l'ultimo record
226900130920     C                   If        $First = *on
227000130920     C                   Exsr      InsertTab
227100130920     C                   Endif
227200130918     C*
227300130911C2226C                   ENDSR
227400130911  "  C/EJECT
227500130911  "  C************************************************************
227600130919  "  C* Inserimento tabella
227700130911  "  C************************************************************
227800130919C2226C     InsertTab     BEGSR
227900130919     C*
228000130919     C/EXEC SQL
228100130919     C+ INSERT INTO QTEMP/NDRCATEMP VALUES(:$pKcc,:$pPartiva,
228200130919     C+ :$pcdfisc, :$pSogg, :Descrpre, :imppre)
228300130919     C/END-EXEC
228400130919     C                   if        SqlCod<0
228500130919     C                   dump(a)
228600130919     C                   exsr      endpgm
228700130919     C                   endif
228800130919C2226C                   ENDSR
228900130919  "  C/EJECT
229000130919  "  C************************************************************
229100130919  "  C* LETTURA RCD ARCHIVIO PILOTA
229200130919  "  C************************************************************
229300130919C2226C     REDANAPRE     BEGSR
229400130911     C*
229500130911     C                   MOVEL     *OFF          $EndFilePre
229600130911     C                   MOVEL     *OFF          $RCDOKPre
229700130911     C*
229800130911     C     $EndFilePre   DOUEQ     *ON
229900130911     C     $RCDOKPRE     OREQ      *ON
230000130919     C*
230100130919     C                   exsr      FetchSqlTab
230200130919     C*
230300130919     C                   if        SqlCod<0 or SqlCod=100
230400130919     C                   MOVE      *ON           *IN21
230500130919     C                   ELSE
230600130919     C                   MOVE      *OFF          *IN21
230700130919     C                   END
230800130919     C*
230900130919     C                   MOVE      *IN21         $EndFilePre
231000130912     C*
231100130912     C                   If        $EndFilePre = *off
231200130919     C                   Eval      $rcdOkPre = *on
231300130919     C                   Endif
231400130912     C*
231500130912     C                   ENDDO
231600130912     C*
231700130912     C                   MOVE      $EndFilePre   *IN34                          SflEnd
231800130912     C*
231900130912C2226C                   ENDSR
232000130912  "  C/EJECT
232100130911  "  C************************************************************
232200130911  "  C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL S2
232300130911  "  C************************************************************
232400130911C2262C     CLCPAG2       BEGSR
232500130911     C* Input :
232600130911     C* - WSFL2= numero dell'ultimo rcd su cui era POSIZIONATO il
232700130911     C*          cursore
232800130911     C* - SFLPAG = numero rcd per pagina sfl
232900130911     C* Output :
233000130911     C* - WPAG2= pagina fino a cui deve essere ricaricato il sfl
233100130911     C*
233200130911     C     WSFL2         DIV       SFLPAG        PAGINE            4 0
233300130911     C                   MVR                     RESTO             3 0
233400130911     C     RESTO         IFGT      0
233500130911     C                   ADD       1             PAGINE
233600130911     C                   ENDIF
233700130911     C     PAGINE        MULT      SFLPAG        WPAG2
233800130911     C*
233900130911C2226C                   ENDSR
234000130911C2226C/EJECT
234100940224     C************************************************************
234200940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
234300940224     C************************************************************
234400940224     C     CLCPAG        BEGSR
234500940224     C* Input :
234600940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
234700940224     C*          cursore
234800940224     C* - SFLPAG = numero rcd per pagina sfl
234900940224     C* Output :
235000940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
235100940224     C*
235200940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
235300940224     C                   MVR                     RESTO             3 0
235400940224     C     RESTO         IFGT      0
235500940224     C                   ADD       1             PAGINE
235600940224     C                   ENDIF
235700940224     C     PAGINE        MULT      SFLPAG        WPAG
235800940224     C*
235900940224     C                   ENDSR
236000940131     C/EJECT
236100940131     C************************************************************
236200940131     C* SELEZIONI SULLA RIGA LETTA
236300940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
236400940314     C* non rientra nei parametri di parzializzazione
236500940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
236600940314     C* superiore stabilito nei parametri
236700940314     C*
236800940131     C************************************************************
236900941214     C     SELANA        BEGSR
237000940131     C*
237100940202     C                   MOVEL     *ON           $RCDOK
237200070119     C                   MOVE      *hival        ALL9             15
237300100407     c* aggancio file per partita per reperire altri dati
237400100407     C                   clear                   SAvv              1
237500100407     C                   clear                   Ssol              1
237600100407     C                   clear                   SImp              1
237700100407     C                   clear                   SImpSCa           1
237800100511     C                   clear                   STel              1
237900100511     C                   clear                   Smail             1
238000130923C2226C                   clear                   SAtt              1
238100130923C2226C                   clear                   SAttSCa           1
238200100407     c                   exsr      LeggiRCa
238300970515     C*
238400940202     C                   ENDSR
238500940131     C/EJECT
238600100407     C************************************************************
238700100407     C* leggo rca per la partita in lettura
238800100407     C************************************************************
238900100407     C     LeggiRca      BEGSR
239000100407     c*
239100100407     c                   eval      RcaSocieta=DsSocieta
239200100407     c                   eval      RcaPosiz  =DsPosiz
239300100407     c                   eval      RcaKcc    =DsKcc
239400100407     c                   eval      RcaKsc    =DsKsc
239500100407     c                   eval      RcaDtPar  =DsDtPar
239600100407     c                   eval      RcaNrPar  =DsNrPar
239700100407     c                   eval      RcaSerPar =DsSerPar
239800100413     c     K07Rca01      setll     NdRca01L
239900100407     c                   do        *hival
240000100413     c     K07Rca01      reade     NdRca01L
240100100413     c                   if        %eof(NdRca01L)
240200100407     c                   leave
240300100407     c                   endif
240400100511     c* vedo quale è il tipo di avvitivà da svolgere
240500100511     C                   MOVE      *ALL'0'       XTBKEY
240600100511    >C                   MOVE      RcaAzione     XTBKEY
240700100511     C                   MOVE      '0'           XTBALC
240800100511     C                   MOVE      '1'           XTBRIC
240900100511     C                   MOVE      '047'         XTBCOD
241000100511     C                   CALLB     'XATB'
241100100511     C                   PARM                    XATBDS
241200100511     C     XTBERR        IFEQ      '0'
241300100511     C                   MOVEL     XTBUNI        Ang047ds
241400100407     c*
241500100511     c* avviso stampato
241600100520     c                   if        Attivit47='A'  and RcaStato<> *blanks
241700100407     c                   eval      SAvv  = *on
241800100407     c                   endif
241900100511     c* sollecito stampato
242000100520     c                   if        Attivit47='S'  and RcaStato <> *blanks
242100100407     c                   eval      SSol  = *on
242200100407     c                   endif
242300100511     c* telefonata fatta
242400100511     c                   if        Attivit47='T'  and RcaStato <> *blanks
242500100511     c                   eval      STel  = *on
242600100511     c                   endif
242700100511     c* lettera o e-mail fatte
242800100511     c                   if        Attivit47='L'  and RcaStato <> *blanks
242900100511     c                             or
243000100511     c                             (Attivit47='E'  and RcaStato <> *blanks)
243100100511     c                   eval      SMail = *on
243200100511     c                   endif
243300100524     c* impegno presente NON chiuso, attivo
243400100524     c                   if        Attivit47='I' and RcaStato = *blanks
243500100407     c                   eval      SImp  = *on
243600100407     c                   endif
243700100524     c* impegno scaduto presente, non chiuso
243800100524     c                   if        Attivit47='X' and RcaStato = *blanks
243900100524     c                   eval      SImpSca = *on
244000100407     c                   endif
244100130923C2226c* se attività libera ne verifico la presenza nel file
244200130923  "  C* ndatt00f
244300130923  "  C                   eval      ATTSocieta = RcaSocieta
244400130923  "  C                   eval      ATTKcc     = RcaKcc
244500130923  "  C                   eval      ATTKsc     = RcaKsc
244600130923  "  C                   eval      ATTDtPar   = RcaDtPar
244700130923  "  C                   eval      ATTNrPar   = RcaNrPar
244800130923  "  C                   eval      ATTSerPar  = RcaSerPar
244900130923  "  C                   eval      ATTDtSca   = RcaDtSca
245000130923  "  C                   eval      ATTProgr   = *Hival
245100130923  "  C     K08Att01      setgt     NdATT01l
245200130923  "  C     K07Att01      ReadPe    NdATT01l
245300130923  "  C                   If        Not %eof(NdAtt01l)
245400130923  "  C                   If        AttDtAtt > zdateIso
245500130923  "  C                   Eval      SAtt = *on
245600130923  "  C                   Else
245700130923  "  C                   eval      SAttSca = *on
245800130923  "  C                   Endif
245900130923C2226C                   Endif
246000100407     c*
246100100511     c                   endif
246200100407     c                   enddo
246300100407     c*
246400100407     c                   ENDSR
246500940131     C************************************************************
246600940131     C* RIEMPIMENTO RIGA LISTA
246700940131     C************************************************************
246800940131     C     RIES1         BEGSR
246900940131     C*
247000940131     C                   CLEAR                   FMTS1
247100100407     c                   move      SAvv          H1Avv
247200100407     c                   move      SSol          H1Sol
247300100407     c                   move      SImp          H1Imp
247400100407     c                   move      SImpSca       H1ImpSca
247500100511     c                   move      STel          H1Tel
247600100512     c                   move      Smail         H1in15
247700100419     c                   move      DsDivisa      H1Divisa
247800130923C2226c                   move      SAtt          H1Att
247900130923C2226c                   move      SAttSca       H1AttSca
248000950301     C*
248100950301     C* riempio i campi del sfl, compresi gli hidden con i valori
248200950301     C* del rcd letto
248300100407    >C                   MOVEL     DsKcc         H1Kcc
248400100407    >C                   MOVEL     DsKsc         S1Ksc
248500100402     C* Richiamo modulo controllo conto
248600100407     c                   move      Dskcc         $kcc
248700100407     c                   move      Dsksc         $ksc
248800100402     C                   EXSR      CALLMVC002
248900100402     C     $Esito        IFne      *ON
249000100402     C                   MOVEL     DEScr002      s1descr
249100100521     c                   if        DtAnn002 <=Zdateiso and
249200100521     c                             DtAnn002 <> *loval
249300100521     c                   move      *on           H1in18
249400100521    -C                   endif
249500100402    -C                   ENDIF
249600100402     c*
249700100407     c                   if        DsDtPar    <> *loval
249800100407     c     *jobrun       move      DsDtPar       s1dtPar
249900100402     c                   endif
250000100407     c                   z-add     DsNrPar       S1NrPar
250100100407     c                   move      DsSerPar      S1SerPar
250200100407     c*
250300100407     c*
250400100407     c                   move      DskscAge      s1kscAge
250500100402   1 C     s1kscage      IFNE      *blanks
250600100402     C                   CALLb     'XALLINEA'
250700100402     C                   PARM                    s1kscage
250800100402     C                   PARM      8             LEN               5 0
250900100402     C                   PARM                    XSCINP
251000100402     C* reperisco capoconto agente
251100100402     C                   EVAL      Xcapkcc  = 'F     '
251200100402     C                   MOVE      XSCSOC        XCAPSOCIET
251300100402     C                   MOVE      s1kscage      XCAPKSC
251400100402     C                   CALLB     'XCAPCLIFOR'
251500100402     C                   PARM                    XCAPSOCIET        3
251600100402     C                   PARM                    XCAPKCC           6
251700100402     C                   PARM                    XCAPKSC           8
251800100402     C* Richiamo modulo controllo conto
251900100402     c                   move      Xcapkcc       $kcc
252000100402     c                   move      Xcapksc       $ksc
252100100402     C                   EXSR      CALLMVC002
252200100402   2 C     $Esito        IFne      *ON
252300100402     C                   MOVEL     DESBREV002    s1kscageD
252400100402   2-C                   ENDIF
252500100402   1-C                   ENDIF
252600950301     C*
252700090325     C                   Eval      S1opz    =    Wopz
252800090325     C                   if        S1opz     <>  0
252900090325     C                   SetOn                                        32
253000090325     C                   endif
253100100407     c*
253200090325     c*
253300950301     C* imposto quegli attributi del sfl che sono modificabili
253400950301     C* al richiamo del determinato pgm
253500950301     C                   EXSR      ATRS1
253600940131     C*
253700940131     C                   ENDSR
253800950301     C/EJECT
253900950301     C************************************************************
254000950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
254100950301     C************************************************************
254200950301     C     ATRS1         BEGSR
254300950301     C*
254400971120     C* in questa routine bisogna attivare indicatori o campi programma
254500971120     C* che gestiscono gli attributi di uno o più campi di questa videata;
254600971120     C* attenzione : non è questo il posto in cui eseguire controlli,
254700971120     C*  dovrebbero già arrivare dei flag
254800970618     C*
254900100407     C                   MOVEL     X'A0'         H1AvvA
255000100407     C                   MOVEL     X'A0'         H1SolA
255100100407     C                   MOVEL     X'A0'         H1ImpA
255200100407     C                   MOVEL     X'A0'         H1ImpScaA
255300100511     C                   MOVEL     X'A0'         H1TelA
255400130923C2226C                   MOVEL     X'A0'         H1AttA
255500130923C2226C                   MOVEL     X'A0'         H1AttScaA
255600100412     c*
255700100407     c                   if        H1Avv=*on
255800100407     c                   move      Presente      H1AvvA
255900100407     c                   endif
256000950301     C*
256100100407     c                   if        H1Sol=*on
256200100408     c                   move      PreseBlu      H1SolA
256300100407     c                   endif
256400100511     C*
256500100511     c                   if        H1Tel=*on
256600100511     c                   move      PreseHig      H1TelA
256700100511     c                   endif
256800100511     C*
256900100512     c                   move      H1in15        *in15
257000100407     C*
257100100407     c                   if        H1Imp=*on
257200100407     c                   move      PresePnk      H1ImpA
257300100407     c                   endif
257400100407     C*
257500100407     c                   if        H1ImpSca=*on
257600100407     c                   move      PreseREd      H1ImpScaA
257700100407     c                   endif
257800100521     c* cliente annullato
257900100521     c                   move      H1in18        *in18
258000130923C2226C*
258100130923  "  C* Presenza di attività manuali in essere o scadute
258200130923  "  C                   if        H1Att=*on
258300130923  "  C                   move      PresePnk      H1AttA
258400130923  "  C                   endif
258500130923  "  C*
258600130923  "  C                   if        H1AttSca=*on
258700130923  "  C                   move      PreseREd      H1AttScaA
258800130923C2226c                   endif
258900100407     C*
259000950301     C                   ENDSR
259100941206     C/EJECT
259200941206     C************************************************************
259300941206     C* GESTIONE F08 VIDEO S1
259400941206     C************************************************************
259500941206     C     F08S1         BEGSR
259600941206     C*
259700941206     C                   MOVE      *ON           $FINE
259800090323    >C                   MOVE      '0'           RETL76
259900941206     C*
260000941206     C                   ENDSR
260100940309     C/EJECT
260200940309     C************************************************************
260300940309     C* GESTIONE F03 VIDEO S1
260400940309     C************************************************************
260500940309     C     F03S1         BEGSR
260600940309     C*
260700940309     C                   MOVE      *ON           $FINE
260800090323    >C                   MOVE      '1'           RETL76
260900940325     C* fine programma
261000940325     C                   EXSR      ENDPGM
261100940309     C*
261200940309     C                   ENDSR
261300130924C2226C************************************************************
261400130924  "  C* GESTIONE F05 VIDEO S1
261500130924  "  C************************************************************
261600130924C2226C     F05S1         BEGSR
261700130924     C*
261800130924     C                   MOVE      *ON           $INZS1
261900130924     C                   MOVE      *ON           WINZS1
262000130924     C*
262100130924C2226C                   ENDSR
262200940309     C************************************************************
262300940309     C* GESTIONE F12 VIDEO S1
262400940309     C************************************************************
262500940309     C     F12S1         BEGSR
262600940309     C*
262700940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
262800940309     C     $LOP          IFEQ      1
262900130912C2226C                   IF        $Pre = *on
263000130911  "  C                   MOVE      'S2'          $GEST
263100130923  "  C                   move      *ON           $INZS2                         iniz.sfl fine ciclo
263200130923  "  c                   move      *on           Winzs2
263300130911C2226C                   Else
263400940309     C                   MOVE      'D1'          $GEST
263500130911C2226C                   Endif
263600940309     C*  se la 1a videata è questa, per F12 torno a menu
263700940309     C                   ELSE
263800130912C2226C                   IF        $Pre = *on
263900130911  "  C                   MOVE      'S2'          $GEST
264000130911C2226C                   Else
264100940309     C                   MOVE      *ON           $FINE
264200090323    >C                   MOVE      '2'           RETL76
264300130911C2226C                   ENDIF
264400941027     C                   ENDIF
264500940309     C*
264600940309     C                   ENDSR
264700940309     C/EJECT
264800090325     C************************************************************
264900090325     C* GESTIONE F13 VIDEO S1
265000090325     C************************************************************
265100090325     C     F13S1         BEGSR
265200090325     C*
265300090325     C* E' simile alla funzione F13 del PDM: riporta l'opzione della
265400090325     C* riga del sfl su cui è posizionato il cursore dalla riga
265500090325     C* successiva fino alla fine del sfl stesso,
265600090325     C* senza sovrapporsi alle opzioni preesistenti.
265700090325     C* il controllo lo faccio solo se il sfl NON è vuoto
265800090325   1bC                   If        WMax    <>    0
265900090325     C* determino Riga/Colonna del cursore
266000090325     C                   MOVE      NRG           R$$
266100090325     C                   MOVE      NCL           C$$
266200090325     C                   Z-ADD     RIRI          CURR
266300090325     C                   Z-ADD     COCO          CURC
266400090325     C*
266500090325     C* se sono posizionato sul sfl
266600090325     C* salvo il numero del record su cui sono posizionato
266700090325     c                   Z-Add     C1Nrr         WC1Rcd
266800090325     C* leggo il rcd
266900090325     C     C1NrR         CHAIN     FMTS1                              21
267000090325     C* salvo l'opzione e il numero relativo
267100090325     C                   If        S1Opz   >     0
267200090325     C                   Z-Add     S1Opz         WOpz              2 0
267300090325     c                   Z-Add     C1Nrr         $X
267400090325     C* attivo SflNxtChg
267500090325     C                   SetOn                                        32
267600090325     C                   UpDate    FmtS1
267700090325     C                   SetOff                                       32
267800090325     C*
267900090325     C* mi posiziono sul record successivo
268000090325     C                   Add       1             $X
268100090325     c*
268200090325     C                   DoW       $X      <=    WMax
268300090325     C     $X            Chain     FmtS1                              21
268400090325     C   21              Leave
268500090325     C                   if        S1Opz    =    0
268600090325     C* imposto l'opzione solo se è possibile eseguirla
268700090325     c*
268800090325     C                   Z-Add     WOpz          S1Opz
268900090325     C                   endif
269000090325     C*                  endif
269100090325     C* attivo SflNxtChg
269200090325     C                   SetOn                                        32
269300090325     C                   UpDate    FmtS1
269400090325     C                   SetOff                                       32
269500090325     C*
269600090325     C                   Add       1             $X
269700090325     C                   enddo
269800090325     c*
269900090325     C                   Eval      WPag     =    9999
270000090325     C*
270100090325     C                   ExSr      RolS1
270200090325     C* reimposto sflpag
270300090325     C                   Eval      WPag     =    SflPag
270400090325     C*
270500090325     C* ripristino la posizione iniziale del record
270600090325     c                   Z-Add     WC1Rcd        C1Rcd
270700090325     C* ripristino il campo di salvataggio dell'opzione per non sporca
270800090325     C* re il campo S1Opz in caso di ricaricamento del sfl
270900090325     C                   Z-Add     0             WOpz
271000090325     C*
271100090325     C                   endif
271200090325     C*
271300090325     C                   endif
271400090325     C*
271500090325     C*
271600090325     C                   ENDSR
271700090325     C/EJECT
271800940309     C************************************************************
271900940309     C* GESTIONE F17 VIDEO S1
272000940309     C************************************************************
272100940309     C     F17S1         BEGSR
272200940309     C*
272300940309     C                   MOVE      'D1'          $GEST
272400941104     C* se il pgm è stato richiamato con chiave univoca
272500941104     C* e dopo è stata richiesta una parzializzazione
272600941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
272700941104     C* valore; propongo come ordinamento il 1°)
272800950606     C     $Unique       IFEQ      *ON
272900950606     C                   MOVE      *OFF          $Unique
273000090323    >C***                MOVE      *BLANK        UQ1L76
273100941027     C                   ENDIF
273200940831     C     $ORD          IFEQ      99
273300940831     C                   Z-ADD     1             $ORD
273400941104     C                   MOVE      *ON           $INZD1
273500941027     C                   ENDIF
273600940309     C*
273700940309     C                   ENDSR
273800940309     C/EJECT
273900940309     C************************************************************
274000940309     C* GESTIONE F18 VIDEO S1
274100940309     C************************************************************
274200940309     C     F18S1         BEGSR
274300940309     C*
274400940309     C                   MOVE      'D2'          $GEST
274500940309     C                   MOVEL     *ON           $INZD2
274600940831     C     $ORD          IFEQ      99
274700940831     C                   Z-ADD     1             $ORD
274800941027     C                   ENDIF
274900940309     C*
275000940309     C                   ENDSR
275100940309     C/EJECT
275200940309     C************************************************************
275300940309     C* GESTIONE F23 VIDEO S1
275400940309     C************************************************************
275500940309     C     F23S1         BEGSR
275600940309     C*
275700950906     C                   Z-ADD     $C1Op1        $L01
275800950906     C                   Z-ADD     $C1Op2        $L02
275900940309     C                   EXSR      RIEOPZ
276000941214     C                   Z-ADD     23            X
276100941214     C                   MOVE      FF23          $FC2(X)
276200070531     C                   MOVEL     WOPZ1         C1OPz
276300060224     C                   MOVEL     $ALLOPT       H1ALLOPT
276400940608     C                   MOVEL     WOPZ2         C1OPZ2
276500940309     C                   Z-ADD     WCOZ          C1COZ
276600070531     c* riga unica opzioni
276700070531     c                   eval      $A = %triml(c1opz)
276800070531     c                   eval      $B = %triml(c1opz2)
276900070531     c     $A            cat(P)    $B:2          C1opz1
277000940309     C*
277100940309     C                   ENDSR
277200940309     C/EJECT
277300940309     C************************************************************
277400940309     C* GESTIONE F24 VIDEO S1
277500940309     C************************************************************
277600940309     C     F24S1         BEGSR
277700940506     C*
277800940506     C                   MOVEA     $FX2          $FX
277900940506     C                   MOVEA     $FC2          $FC
278000940506     C                   MOVEA     $FR2          $FR
278100940506     C                   Z-ADD     $ULKS1        $ULK
278200950726     C                   Z-ADD     $S1L01        $L01
278300950726     C                   Z-ADD     $S1L02        $L02
278400940506     C                   EXSR      RIEKEY
278500940506     C                   Z-ADD     $ULK          $ULKS1            3 0
278600940720     C                   MOVEA     $FC           $FC2
278700070618     C                   MOVEL     $KEY1         Z2KE
278800060224     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
278900940506     C                   MOVEL     $KEY2         Z2KE2
279000070618     c* riga unica tasti
279100070618     c                   eval      $A = %triml(z2ke)
279200070618     c                   eval      $B = %triml(z2ke2)
279300070618     c     $A            cat(P)    $B:0          z2ke1
279400940506     C                   WRITE     FMTZ2
279500940506     C*
279600940309     C                   ENDSR
279700940128     C/EJECT
279800940128     C************************************************************
279900940131     C* CONTROLLO TESTATA LISTA
280000940128     C************************************************************
280100940131     C     CTRC1         BEGSR
280200940128     C*
280300940201     C                   MOVE      *OFF          *IN99
280400940131     C*
280500940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
280600940202     C* L'INIZIALIZZAZIONE LISTA
280700940202     C*
280800941214     C     C1POS         IFNE      W1POS
280900940202     C                   EXSR      POSS1
281000941027     C                   ENDIF
281100940202     C*
281200940202     C                   ENDSR
281300940202     C/EJECT
281400940202     C************************************************************
281500940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
281600940202     C************************************************************
281700940202     C     POSS1         BEGSR
281800940202     C*
281900940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
282000940202     C*
282100941118   B1C     WMAX          IFGT      0
282200940202     C                   READC     FMTS1                                  21
282300941118   B2C     *IN21         DOWEQ     *OFF
282400940202     C     *IN99         ANDEQ     *OFF
282500940202     C*
282600941118   B3C     S1OPZ         IFNE      0
282700960109     C                   SETON                                        92  99
282800941118   E3C                   ENDIF
282900940202     C* se rilevata opzione attiva, attivo sflnxtchg
283000941118   B3C     *IN99         IFEQ      *ON
283100940202     C                   MOVE      *ON           *IN32
283200950301     C*
283300950301     C* imposto quegli attributi del sfl che sono modificabili
283400950301     C* al richiamo del determinato pgm
283500941118     C                   EXSR      ATRS1
283600941118     C*
283700940202     C                   UPDATE    FMTS1
283800941118   X3C                   ELSE
283900940202     C                   READC     FMTS1                                  21
284000941118   E3C                   ENDIF
284100940202     C*
284200941118   E2C                   ENDDO
284300940202     C*
284400941118   E1C                   ENDIF
284500940202     C*
284600940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
284700940221     C* AL VALORE DI POSIZIONAMENTO :
284800940221     C* se questo è maggiore del valore del rispettivo campo di
284900940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
285000940221     C* con i valori di parzializzazione.
285100940221     C*
285200941118   B1C     *IN99         IFEQ      *OFF
285300940721    >C                   MOVE      *BLANK        W1POS
285400940202     C*
285500950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
285600950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
285700950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
285800950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
285900950525     C* verificata la condizione suddetta avverrà con posizionamento a
286000950525     C* partire dal valore di parzializzazione
286100950525     C*
286200941118   B2C                   SELECT
286300940202     C*
286400940202     C     $ORD          WHENEQ    1
286500070524    >C     C1POS         IFGT      D1KSC
286600951005    >C                   MOVEL     C1POS         W1POS
286700070524    >C                   CALLB     'XALLINEA'
286800070524    >C                   PARM                    W1Pos
286900070524    >C                   PARM      8             LEN               5 0
287000070524    >C                   PARM                    XSCINP
287100941027     C                   ENDIF
287200950525     C*
287300950525     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
287400950525     C*
287500941118   E2C                   ENDSL
287600950525     C*
287700950525     C* richiedo l'inizializzazione sfl
287800950525     C                   MOVE      *ON           $INZS1
287900940202     C*
288000941118   E1C                   ENDIF
288100940202     C*
288200940131     C                   ENDSR
288300940131     C/EJECT
288400940131     C************************************************************
288500940131     C* CONTROLLO OPZIONI LISTA
288600940131     C************************************************************
288700940131     C     CTRS1         BEGSR
288800940131     C*
288900940202     C                   MOVEL     *OFF          $ESCI
289000940201     C                   SETOFF                                       99
289100940131     C                   Z-ADD     0             S1OPZ
289200940131     C*
289300940127     C* Leggo il sfl solo se ci sono rcd
289400941118   B1C     WMAX          IFGT      0
289500940131     C                   READC     FMTS1                                  21
289600940127     C*
289700940131     C* esce se fine sfl o errore che richiede l'uscita
289800941118   B2C     *IN21         DOWEQ     *OFF
289900940131     C     $ESCI         ANDEQ     *OFF
290000940201     C                   Z-ADD     S1NRR         C1RCD
290100940131     C* ctrl su riga
290200940131     C                   EXSR      RECS1
290300940131     C* gestione opzioni
290400941118   B3C     S1OPZ         IFNE      0
290500940201     C     *IN99         ANDEQ     *OFF
290600940131     C                   EXSR      OPZS1
290700941118   E3C                   ENDIF
290800940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
290900941118   B3C     *IN99         IFEQ      *ON
291000940131     C                   MOVE      *ON           *IN32
291100941118   E3C                   ENDIF
291200950301     C*
291300950301     C* imposto quegli attributi del sfl che sono modificabili
291400950301     C* al richiamo del determinato pgm
291500941118     C                   EXSR      ATRS1
291600941118     C*
291700940131     C                   Z-ADD     0             S1OPZ
291800940223     C*
291900940127     C                   UPDATE    FMTS1
292000941125     C                   MOVE      *OFF          *IN32
292100940223     C*
292200940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
292300941118   B3C     $ESCI         IFEQ      *OFF
292400940131     C                   READC     FMTS1                                  21
292500940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
292600941118   B4C     *IN21         IFEQ      *ON
292700940201     C                   MOVE      WINZS1        $INZS1
292800010620     C                   MOVE      *off          WINZS1
292900980603     C* se è avvenuta almeno una modifica
293000980603   B5C                   IF        $INZS1 = *on
293100940204     C* calcolo pagina a cui deve posizionarsi
293200940728     C     WSFL          IFGT      0
293300940728     C                   EXSR      CLCPAG
293400940728     C                   ELSE
293500940728     C                   Z-ADD     SFLPAG        WPAG
293600940728     C                   ENDIF
293700980603   E5C                   ENDIF
293800941118   E4C                   ENDIF
293900980603   E3C                   ENDIF
294000940131     C*
294100941118   E2C                   ENDDO
294200940127     C*
294300941118   E1C                   ENDIF
294400940131     C*
294500940127     C                   ENDSR
294600940127     C/EJECT
294700940127     C************************************************************
294800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
294900940127     C************************************************************
295000940131     C     RECS1         BEGSR
295100940131     C*
295200940201     C* reset indicatori DSPATR
295300940201     C                   MOVE      *ALL'0'       IN4049           10
295400940201     C                   MOVEA     IN4049        *IN(40)
295500940201     C*
295600940131     C* Controllo se Utente Autorizzato alla scelta
295700940202     C     S1OPZ         IFNE      0
295800940131     C                   Z-ADD     1             X
295900940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
296000960117     C* mi salvo l'indice dell' opzione
296100960117     C   21              Z-add     X             PosOpz            2 0
296200940211     C     *IN21         IFEQ      *OFF
296300940223     C     $FL1(X)       OREQ      'N'
296400940131     C                   MOVE      *ON           $ESCI
296500960109     C                   SETON                                        409199
296600940131     C                   ENDIF
296700940202     C                   ENDIF
296800940131     C*
296900970424     C*
297000940131     C                   ENDSR
297100941118     C/EJECT
297200941118     C************************************************************
297300941118     C* GESTIONE OPZIONI LISTA
297400941118     C************************************************************
297500941118     C     OPZS1         BEGSR
297600950301     C*
297700950301     C                   Z-ADD     RIRI          CURR
297800950301     C                   Z-ADD     COCO          CURC
297900941118     C*
298000941027     C                   SELECT
298100090330     C* se opz=Scheda cliente
298200090330     C* richiamo il pgm di gestione
298300090330     C     S1OPZ         WHENEQ    1
298301140604      *
298302140604      * Ottimizzo scrittura x20scadr
298303140604     C                   if        %lookup(s1ksc:sksaveksc) = 0
298307140604     C                   eval      sksaveksc(nextksc)=s1ksc
298308140604     C                   add       1             nextksc
298309140604     C                   eval      wrtX20L77 = *on
298310140604     C                   else
298311140604     C                   eval      wrtX20L77 = *off
298312140604     C                   ENDIF
298313140604      *
298400090331     c                   move      '02'          opzL77
298500090401     c                   move      xscsoc        SocietaL77
298600151022    >C                   MOVE      *blanks       UnitaL77
298601151022    >C*****                   MOVE      C1Unita       UnitaL77
298700090330    >C                   MOVE      h1kcc         kccl77
298800090330    >C                   MOVE      s1ksc         kscl77
298900100408     c     *jobrun       move      s1dtpar       dtParl77
299000100408     c                   z-add     s1nrpar       NrparL77
299100100408     c                   move      s1serpar      SerParL77
299200100419     c                   move      H1divisa      DivisaL77
299300140314brt  c**                 if        d1stato = xscon
299400140314brt  c**                 eval      StatoL77 = *on
299500140314brt  c**                 else
299600140314brt  c**                 eval      StatoL77 = *off
299700140314brt  c**                 endif
299800140331brt  c                   eval      StatoL77 = d1fasiape
299900090330     C                   movel     ndcl77ds      kpjbu
300000090330     C                   CALL      'NDCL77R'                            21
300100090330     C                   PARM                    KPJBA
300200100426c2156C                   movel     kpjbu         ndcl77ds
300300100426c2156c                   exsr      RTNL77
300400940831     C*
300500101209c2176C* se opz=Chiusura scadenze partita
300600101209  "  C     S1OPZ         WHENEQ    15
300700101209c2176c                   exsr      AggRca
300800070531     C* se opz=Interrogazioni
300900070531     C* richiamo il pgm di gestione
301000090402     C     S1OPZ         WHENEQ    16
301100070531     C                   reset                   ana399ds
301200070531     C                   eval      A39societa = XSCSOC
301300070531     C                   eval      A39unita = *blanks
301400070531     C                   eval      A39ctb = 'CG'
301500070531     C                   eval      A39kcc = h1kcc
301600070531     C                   eval      A39ksc = s1ksc
301700070531     C                   movel     ana399ds      kpjbu
301800070531     C                   CALL      'ANA399R'                            21
301900070531     C                   PARM                    KPJBA
302000070531     C     A39ERRORI     IFEQ      '0'
302100070531     C                   endif
302200130912C2226C*
302300130912  "  C* se selezionata opzione personalizzata, richiamo il programma
302400130912  "  C* corrispondente
302500130912  "  C     $PGO1(PosOpz) Whenne    *Blank
302600130912  " >C                   RESET                   NDCL77DS
302700130912  "  c                   move      '02'          opzL77
302800130912  "  c                   move      xscsoc        SocietaL77
302900130912  " >C                   MOVE      C1Unita       UnitaL77
303000130912  " >C                   MOVE      h1kcc         kccl77
303100130912  " >C                   MOVE      s1ksc         kscl77
303200130912  "  c     *jobrun       move      s1dtpar       dtParl77
303300130912  "  c                   z-add     s1nrpar       NrparL77
303400130912  "  c                   move      s1serpar      SerParL77
303500130912  "  c                   move      H1divisa      DivisaL77
303600140314br"t c**                 if        d1stato = xscon
303700140314br"t c**                 eval      StatoL77 = *on
303800140314br"t c**                 else
303900140314br"t c**                 eval      StatoL77 = *off
304000140314br"t c**                 endif
304100140331brt  c                   eval      StatoL77 = d1fasiape
304200130912  "  C                   movel     ndcl77ds      kpjbu
304300130912  "  C*
304400130912  "  C                   CALL      $PGO1(PosOpz)                        22
304500130912  "  C                   PARM                    KPJBA
304600130912  "  C                   PARM                    NOMPGM
304700130912  "  C*
304800130912  " >C                   MOVEL     KPJBU         NDCL77DS
304900130912  "  C* ritorno da PGM
305000130912  " >C                   EXSR      RTNL77
305100130912C2226C*
305200070531     C                   MOVE      *HIVAL        $LASTV
305300940225     C*
305400950620     C* se altre opzioni
305500941027     C                   OTHER
305600940225     C*
305700941027     C                   ENDSL
305800940225     C*
305900940131     C                   ENDSR
306000130911C2226C************************************************************
306100130911  "  C* RIEMPIMENTO RIGA PRELISTA
306200130911  "  C************************************************************
306300130911C2226C     RIES2         BEGSR
306400130911     C*
306500130911     C                   CLEAR                   FMTS2
306600130911     C*
306700130911     C* riempio i campi del sfl, compresi gli hidden con i valori
306800130911     C* del rcd letto
306900130911     C*
307000130912     C                   Select
307100130912     C                   When      $RgrPre = 'P'
307200130918     C                   Eval      H2Kcc = KccPre
307300130918     C                   Eval      H2PartIva = PartIvaPre
307400130918     C                   Eval      H2CdFisc  = CdFiscPre
307500130918     C                   If        PartivaPre <> *blanks
307600130918     C                   Movel     PartIvaPre    S2Codice
307700130911     C                   Else
307800130918     C                   Movel     CdFiscPre     S2Codice
307900130911     C                   Endif
308000130918     C                   Movel     DescrPre      S2Descr
308100130918     C                   Z-ADD     ImpPre        S2Importo
308200130912     C                   Other
308300130919     C                   Eval      H2Kcc   = KccPre
308400130919     C                   Eval      H2Sogg  = SoggPre
308500130918     C                   Movel     H2Sogg        S2Codice
308600130918     C                   Movel     DescrPre      S2Descr
308700130918     C                   Z-ADD     ImpPre        S2Importo
308800130912     C                   EndSl
308900130911     C*
309000130911     C                   Eval      S2opz    =    Wopz
309100130911     C                   if        S2opz     <>  0
309200130911     C                   SetOn                                        32
309300130911     C                   endif
309400130911     c*
309500130911     c*
309600130911     C* imposto quegli attributi del sfl che sono modificabili
309700130911     C* al richiamo del determinato pgm
309800130911     C                   EXSR      ATRS2
309900130911     C*
310000130911     C                   ENDSR
310100130911C2226C/EJECT
310200130911  "  C************************************************************
310300130911  "  C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
310400130911  "  C************************************************************
310500130911C2226C     ATRS2         BEGSR
310600130911     C*
310700130911     C* in questa routine bisogna attivare indicatori o campi programma
310800130911     C* che gestiscono gli attributi di uno o più campi di questa videata;
310900130911     C* attenzione : non è questo il posto in cui eseguire controlli,
311000130911     C*  dovrebbero già arrivare dei flag
311100130911     C*
311200130911     C                   ENDSR
311300130911C2226C/EJECT
311400130911  "  C************************************************************
311500130911  "  C* GESTIONE F08 VIDEO S2
311600130911  "  C************************************************************
311700130911C2226C     F08S2         BEGSR
311800130911     C*
311900130911     C                   MOVE      *ON           $FINE
312000130911    >C                   MOVE      '0'           RETL76
312100130911     C*
312200130911C2226C                   ENDSR
312300130911  "  C/EJECT
312400130911  "  C************************************************************
312500130911  "  C* GESTIONE F03 VIDEO S2
312600130911  "  C************************************************************
312700130911C2226C     F03S2         BEGSR
312800130911     C*
312900130911     C                   MOVE      *ON           $FINE
313000130911    >C                   MOVE      '1'           RETL76
313100130911     C* fine programma
313200130911     C                   EXSR      ENDPGM
313300130911     C*
313400130911C2226C                   ENDSR
313500130911  "  C************************************************************
313600130911  "  C* GESTIONE F12 VIDEO S2
313700130911  "  C************************************************************
313800130911C2226C     F12S2         BEGSR
313900130911     C*
314000130911     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
314100130911     C     $LOP          IFEQ      1
314200130911     C                   MOVE      'D1'          $GEST
314300130911     C*  se la 1a videata è questa, per F12 torno a menu
314400130911     C                   ELSE
314500130911     C                   MOVE      *ON           $FINE
314600130911    >C                   MOVE      '2'           RETL76
314700130911     C                   ENDIF
314800130911     C*
314900130911C2226C                   ENDSR
315000130911  "  C/EJECT
315100130911  "  C************************************************************
315200130911  "  C* GESTIONE F13 VIDEO S1
315300130911  "  C************************************************************
315400130911C2226C     F13S2         BEGSR
315500130911     C*
315600130911     C* E' simile alla funzione F13 del PDM: riporta l'opzione della
315700130911     C* riga del sfl su cui è posizionato il cursore dalla riga
315800130911     C* successiva fino alla fine del sfl stesso,
315900130911     C* senza sovrapporsi alle opzioni preesistenti.
316000130911     C* il controllo lo faccio solo se il sfl NON è vuoto
316100130911   1bC                   If        WMaxS2  <>    0
316200130911     C* determino Riga/Colonna del cursore
316300130911     C                   MOVE      NRG           R$$
316400130911     C                   MOVE      NCL           C$$
316500130911     C                   Z-ADD     RIRI          CURR
316600130911     C                   Z-ADD     COCO          CURC
316700130911     C*
316800130911     C* se sono posizionato sul sfl
316900130911     C* salvo il numero del record su cui sono posizionato
317000130911     c                   Z-Add     C2Nrr         WC2Rcd
317100130911     C* leggo il rcd
317200130911     C     C2NrR         CHAIN     FMTS2                              21
317300130911     C* salvo l'opzione e il numero relativo
317400130911     C                   If        S2Opz   >     0
317500130911     C                   Z-Add     S2Opz         WOpzS2            2 0
317600130911     c                   Z-Add     C2Nrr         $XS2
317700130911     C* attivo SflNxtChg
317800130911     C                   SetOn                                        32
317900130911     C                   UpDate    FmtS2
318000130911     C                   SetOff                                       32
318100130911     C*
318200130911     C* mi posiziono sul record successivo
318300130911     C                   Add       1             $X
318400130911     c*
318500130911     C                   DoW       $X      <=    WMaxS2
318600130911     C     $XS2          Chain     FmtS2                              21
318700130911     C   21              Leave
318800130911     C                   if        S2Opz    =    0
318900130911     C* imposto l'opzione solo se è possibile eseguirla
319000130911     c*
319100130911     C                   Z-Add     WOpzS2        S2Opz
319200130911     C                   endif
319300130911     C*                  endif
319400130911     C* attivo SflNxtChg
319500130911     C                   SetOn                                        32
319600130911     C                   UpDate    FmtS2
319700130911     C                   SetOff                                       32
319800130911     C*
319900130911     C                   Add       1             $XS2
320000130911     C                   enddo
320100130911     c*
320200131129     C                   Eval      WPag2    =    9999
320300130911     C*
320400130911     C                   ExSr      RolS2
320500130911     C* reimposto sflpag
320600131129     C                   Eval      WPag2    =    SflPag
320700130911     C*
320800130911     C* ripristino la posizione iniziale del record
320900130911     c                   Z-Add     WC2Rcd        C2Rcd
321000130911     C* ripristino il campo di salvataggio dell'opzione per non sporca
321100130911     C* re il campo S1Opz in caso di ricaricamento del sfl
321200130911     C                   Z-Add     0             WOpzS2
321300130911     C*
321400130911     C                   endif
321500130911     C*
321600130911     C                   endif
321700130911     C*
321800130911     C*
321900130911C2226C                   ENDSR
322000130911  "  C/EJECT
322100130911  "  C************************************************************
322200130911  "  C* GESTIONE F17 VIDEO S2
322300130911  "  C************************************************************
322400130911C2226C     F17S2         BEGSR
322500130911     C*
322600130911     C                   MOVE      'D1'          $GEST
322700130911     C* se il pgm è stato richiamato con chiave univoca
322800130911     C* e dopo è stata richiesta una parzializzazione
322900130911     C* tolgo la gestione della chiave univoca (azzere/abblenco il
323000130911     C* valore; propongo come ordinamento il 1°)
323100130911     C     $Unique       IFEQ      *ON
323200130911     C                   MOVE      *OFF          $Unique
323300130911    >C***                MOVE      *BLANK        UQ1L76
323400130911     C                   ENDIF
323500130911     C     $ORD          IFEQ      99
323600130911     C                   Z-ADD     1             $ORD
323700130911     C                   MOVE      *ON           $INZD1
323800130911     C                   ENDIF
323900130911     C*
324000130911C2226C                   ENDSR
324100130911  "  C/EJECT
324200130911  "  C************************************************************
324300130911  "  C* GESTIONE F18 VIDEO S2
324400130911  "  C************************************************************
324500130911C2226C     F18S2         BEGSR
324600130911     C*
324700130911     C                   MOVE      'D2'          $GEST
324800130911     C                   MOVEL     *ON           $INZD2
324900130911     C     $ORD          IFEQ      99
325000130911     C                   Z-ADD     1             $ORD
325100130911     C                   ENDIF
325200130911     C*
325300130911C2226C                   ENDSR
325400130911  "  C/EJECT
325500130911  "  C************************************************************
325600130911  "  C* GESTIONE F23 VIDEO S2
325700130911  "  C************************************************************
325800130911C2226C     F23S2         BEGSR
325900130911     C*
326000130911     C                   Z-ADD     $C2Op1        $L01
326100130911     C                   Z-ADD     $C2Op2        $L02
326200130911     C                   EXSR      RIEOPZ
326300130911     C                   Z-ADD     23            X
326400130911     C                   MOVE      FF23          $FC2(X)
326500130911     C                   MOVEL     WOPZ1         C2OPz
326600130911     C                   MOVEL     $ALLOPT       H1ALLOPT
326700130911     C                   MOVEL     WOPZ2         C2OPZ2
326800130911     C                   Z-ADD     WCOZ          C2COZ
326900130911     c* riga unica opzioni
327000130911     c                   eval      $A = %triml(c2opz)
327100130911     c                   eval      $B = %triml(c2opz2)
327200130911     c     $A            cat(P)    $B:2          C2opz1
327300130911     C*
327400130911     C                   ENDSR
327500130911C2226C/EJECT
327600130911  "  C************************************************************
327700130911  "  C* GESTIONE F24 VIDEO S2
327800130911  "  C************************************************************
327900130911C2226C     F24S2         BEGSR
328000130911     C*
328100130911     C                   MOVEA     $FX2          $FX
328200130911     C                   MOVEA     $FC2          $FC
328300130911     C                   MOVEA     $FR2          $FR
328400130911     C                   Z-ADD     $ULKS2        $ULK
328500130911     C                   Z-ADD     $S2L01        $L01
328600130911     C                   Z-ADD     $S2L02        $L02
328700130911     C                   EXSR      RIEKEY
328800130911     C                   Z-ADD     $ULK          $ULKS2            3 0
328900130911     C                   MOVEA     $FC           $FC2
329000130911     C                   MOVEL     $KEY1         Z2KE
329100130911     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
329200130911     C                   MOVEL     $KEY2         Z2KE2
329300130911     c* riga unica tasti
329400130911     c                   eval      $A = %triml(z2ke)
329500130911     c                   eval      $B = %triml(z2ke2)
329600130911     c     $A            cat(P)    $B:0          z2ke1
329700130911     C                   WRITE     FMTZ2
329800130911     C*
329900130911     C                   ENDSR
330000130911C2226C/EJECT
330100130911  "  C************************************************************
330200130911  "  C* CONTROLLO TESTATA PRELISTA
330300130911  "  C************************************************************
330400130911C2226C     CTRC2         BEGSR
330500130911     C*
330600130911     C                   MOVE      *OFF          *IN99
330700130911     C*
330800130911     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
330900130911     C* L'INIZIALIZZAZIONE LISTA
331000130911     C*
331100130911     C     C2POS         IFNE      W2POS
331200130911     C                   EXSR      POSS2
331300130911     C                   ENDIF
331400130911     C*
331500130911     C                   ENDSR
331600130911C2226C/EJECT
331700130911  "  C************************************************************
331800130911  "  C* CONTROLLI PER POSIZIONAMENTO SU PRELISTA
331900130911  "  C************************************************************
332000130911C2226C     POSS2         BEGSR
332100130911     C*
332200130911     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
332300130911     C*
332400130911   B1C     WMAXS2        IFGT      0
332500130911     C                   READC     FMTS2                                  21
332600130911   B2C     *IN21         DOWEQ     *OFF
332700130911     C     *IN99         ANDEQ     *OFF
332800130911     C*
332900130911   B3C     S2OPZ         IFNE      0
333000130911     C                   SETON                                        92  99
333100130911   E3C                   ENDIF
333200130911     C* se rilevata opzione attiva, attivo sflnxtchg
333300130911   B3C     *IN99         IFEQ      *ON
333400130911     C                   MOVE      *ON           *IN32
333500130911     C*
333600130911     C* imposto quegli attributi del sfl che sono modificabili
333700130911     C* al richiamo del determinato pgm
333800130911     C                   EXSR      ATRS2
333900130911     C*
334000130911     C                   UPDATE    FMTS2
334100130911   X3C                   ELSE
334200130911     C                   READC     FMTS2                                  21
334300130911   E3C                   ENDIF
334400130911     C*
334500130911   E2C                   ENDDO
334600130911     C*
334700130911   E1C                   ENDIF
334800130911     C*
334900130911     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
335000130911     C* AL VALORE DI POSIZIONAMENTO :
335100130911     C* se questo è maggiore del valore del rispettivo campo di
335200130911     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
335300130911     C* con i valori di parzializzazione.
335400130911     C*
335500130911   B1C     *IN99         IFEQ      *OFF
335600130911    >C                   MOVE      *BLANK        W2POS
335700130911     C*
335800130911     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
335900130911     C* è maggiore del relativo valore di parzializzazione, nel qual caso
336000130911     C* imposto la variabile del posizionamento (W2Pos) al nuovo valore;
336100130911     C* richiedo comunque un ricaricamento del sfl che, se non si è
336200130911     C* verificata la condizione suddetta avverrà con posizionamento a
336300130911     C* partire dal valore di parzializzazione
336400130911     C*
336500130911    >C     C2POS         IFGT      *blanks
336600130911    >C                   MOVEL     C2POS         W2POS
336700130911    >C                   CALLB     'XALLINEA'
336800130911    >C                   PARM                    W2Pos
336900130911    >C                   PARM      8             LEN               5 0
337000130911    >C                   PARM                    XSCINP
337100130911     C                   ENDIF
337200130911     C*
337300130911     C* richiedo l'inizializzazione sfl
337400130911     C                   MOVE      *ON           $INZS2
337500130911     C*
337600130911   E1C                   ENDIF
337700130911     C*
337800130911     C                   ENDSR
337900130911C2226C/EJECT
338000130911  "  C************************************************************
338100130911  "  C* CONTROLLO OPZIONI LISTA
338200130911  "  C************************************************************
338300130911C2226C     CTRS2         BEGSR
338400130911     C*
338500130911     C                   MOVEL     *OFF          $ESCI
338600130911     C                   SETOFF                                       99
338700130911     C                   Z-ADD     0             S2OPZ
338800130911     C*
338900130911     C* Leggo il sfl solo se ci sono rcd
339000130911   B1C     WMAXS2        IFGT      0
339100130911     C                   READC     FMTS2                                  21
339200130911     C*
339300130911     C* esce se fine sfl o errore che richiede l'uscita
339400130911   B2C     *IN21         DOWEQ     *OFF
339500130911     C     $ESCI         ANDEQ     *OFF
339600130911     C                   Z-ADD     S2NRR         C2RCD
339700130911     C* ctrl su riga
339800130911     C                   EXSR      RECS2
339900130911     C* gestione opzioni
340000130911   B3C     S2OPZ         IFNE      0
340100130911     C     *IN99         ANDEQ     *OFF
340200130911     C                   EXSR      OPZS2
340300130911   E3C                   ENDIF
340400130911     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
340500130911   B3C     *IN99         IFEQ      *ON
340600130911     C                   MOVE      *ON           *IN32
340700130911   E3C                   ENDIF
340800130911     C*
340900130911     C* imposto quegli attributi del sfl che sono modificabili
341000130911     C* al richiamo del determinato pgm
341100130911     C                   EXSR      ATRS2
341200130911     C*
341300130911     C                   Z-ADD     0             S2OPZ
341400130911     C*
341500130911     C                   UPDATE    FMTS2
341600130911     C                   MOVE      *OFF          *IN32
341700130911     C*
341800130911     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
341900130911   B3C     $ESCI         IFEQ      *OFF
342000130911     C                   READC     FMTS2                                  21
342100130911     C* a fine ciclo ripristino il flag richiesta iniz.sfl
342200130911   B4C     *IN21         IFEQ      *ON
342300130911     C                   MOVE      WINZS2        $INZS2
342400130911     C                   MOVE      *off          WINZS2
342500130911     C* se è avvenuta almeno una modifica
342600130911   B5C                   IF        $INZS2 = *on
342700130911     C* calcolo pagina a cui deve posizionarsi
342800130911     C     WSFL2         IFGT      0
342900130911     C                   EXSR      CLCPAG
343000130911     C                   ELSE
343100130911     C                   Z-ADD     SFLPAG        WPAG2
343200130911     C                   ENDIF
343300130911   E5C                   ENDIF
343400130911   E4C                   ENDIF
343500130911   E3C                   ENDIF
343600130911     C*
343700130911   E2C                   ENDDO
343800130911     C*
343900130911   E1C                   ENDIF
344000130911     C*
344100130911     C                   ENDSR
344200130911C2226C/EJECT
344300130911  "  C************************************************************
344400130911  "  C* CONTROLLO CAMPI I/O RIGA PRELISTA
344500130911  "  C************************************************************
344600130911C2226C     RECS2         BEGSR
344700130911     C*
344800130911     C* reset indicatori DSPATR
344900130911     C                   MOVE      *ALL'0'       IN4049           10
345000130911     C                   MOVEA     IN4049        *IN(40)
345100130911     C*
345200130911     C* Controllo se Utente Autorizzato alla scelta
345300130911     C     S2OPZ         IFNE      0
345400130911     C                   Z-ADD     1             X
345500130912     C     S2OPZ         LOOKUP    $OP2(X)                                21
345600130911     C* mi salvo l'indice dell' opzione
345700130911     C   21              Z-add     X             PosOpz            2 0
345800130911     C     *IN21         IFEQ      *OFF
345900130912     C     $FL2(X)       OREQ      'N'
346000130911     C                   MOVE      *ON           $ESCI
346100130911     C                   SETON                                        409199
346200130911     C                   ENDIF
346300130911     C                   ENDIF
346400130911     C*
346500130911     C*
346600130911     C                   ENDSR
346700130911C2226C/EJECT
346800130911  "  C************************************************************
346900130911  "  C* GESTIONE OPZIONI LISTA
347000130911  "  C************************************************************
347100130911C2226C     OPZS2         BEGSR
347200130911     C*
347300130911     C                   Z-ADD     RIRI          CURR
347400130911     C                   Z-ADD     COCO          CURC
347500130911     C*
347600130911     C                   SELECT
347700130911     C* se opz=Lista
347800130911     C* Proseguo con il pgm di lista
347900130911     C     S2OPZ         WHENEQ    1
347901140604      *
347902140604     C                   if        SavePartIva = *blanks
347904140604     C                             or SavePartIva <> H2PartIva
347905140604     C                   eval      SavePartIva = H2PartIva
347906140604     C                   clear                   SkSaveKsc
347907140604     C                   z-add     1             nextksc
347908140604     C                   ENDIF
347909140604      *
348000130911     C                   MOVE      'S1'          $GEST
348100130911     C                   MOVE      *ON           $INZS1
348200130911     C                   MOVE      *BLANK        W1POS
348300130911     C                   EXSR      GESS1
348400130911     C*
348500130911     C                   MOVE      *HIVAL        $LASTV
348600130911     C*
348700130911     C* se altre opzioni
348800130911     C                   OTHER
348900130911     C*
349000130911     C                   ENDSL
349100130911     C*
349200130911     C                   ENDSR
349300101209C2176 ************************************************************
349400110412  "   * Aggiornam. x chiusura fasi (tutte le fasi aperte della partita)
349500101209  "   ************************************************************
349600101209C2176C     AggRca        Begsr
349700110412D2680C* emetto richiesta data chiusura
349800130924C2226C                   If        (PrimaVolta = 0 and
349900130924  "  C                             $Tastosav = F13) or
350000130924C2226C                             $Tastosav <> F13
350100131129D2680c                   eval      D5DtChiu   =udate
350200131129C2226c                   eval      D5DtPar    =S1DtPar
350300130924  "  c                   eval      D5NrPar    =S1NrPar
350400131129C2226c                   eval      D5SerPar   =S1SerPar
350500130923D2680c                   clear                   WWMSG
350600130923C2226c                   clear                   D5Nota
350700130923D2680c                   eval      Idmsg= Msg(1)
350800110412  "  c                   exsr      PrendiMsg
350900130924D2680c                   eval      Z5Key=txtmsg
351000130924C2226c                   eval      Idmsg= Msg(2)
351100130924  "  c                   exsr      PrendiMsg
351200130924C2226c                   eval      Z5Key=%trim(Z5Key) + ' ' +%trim(Txtmsg)
351300130924D2680c     emetti        tag
351400131129D2680c                   exfmt     Fmtd5
351500130924C2226C                   Eval      Primavolta = 1
351600130923D2680c                   if        $tasto=F12
351700130924C2226C                   If        $tastosav = F13
351800131129  "  C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
351900131129  "  c                   move      *on           Winzs1
352000130924  "  C                   Eval      $esci = *on
352100130924C2226C                   Endif
352200130924D2680c                   leavesr
352300110412  "  c                   endif
352400110412  "  C                   if        D5DtChiu <>   *Zeros
352500110412  "  C     *JobRun       Test(d)                 D5DtChiu               21
352600110412  " >C     *In21         ifeq      *On
352700110412  "  c* non corretta
352800110412  " >C                   SetOn                                        6699
352900110412  "  c                   goto      emetti
353000110412  " >C                   endif
353100110412  "  c                   else
353200110412  "  c* non immessa
353300110412  " >C                   SetOn                                        6699
353400110412  "  c                   goto      emetti
353500110412D2680C                   endif
353600131129C2226C                   If        $tasto <> F06
353700130924  "  C                   goto      emetti
353800130924  "  C                   Else
353900130924  "  C                   Eval      $tastoSav2 = F06
354000130924  "  C                   ENdif
354100131129  "  C                   ENdif
354200130924C2226C                   If        $tastosav2 = F06
354300101209     C                   eval      Rcasocieta = xscsoc
354400101209     C                   eval      RcaPosiz   = posattr057
354500101209     C                   eval      Rcakcc     = h1kcc
354600101209     C                   eval      rcaksc     = s1ksc
354700101209     C     *jobrun       move      s1dtpar       RcaDtPar
354800101209     C                   eval      rcanrpar   = s1nrpar
354900101209     C                   eval      rcaserpar  = s1serpar
355000130924     C*
355100101209    >C     K07Rca04      Setll     NDRCA04L
355200101209     c                   do        *hival
355300101209    >C     K07Rca04      reade     NDRCA04                              2221
355400101209     c   21              leave
355500101209     C*
355600101209     C* Gestione RCD allocato
3557001012093    C     *IN22         IFEQ      *ON
355800101209    >C                   MOVEL     'NDRCA00F'    MSGFIL
355900101209     C                   EXSR      MSGPGM
356000101209     c                   if        Msgrtn='C'
356100101209     c                   leave
356200101209     c                   else
356300101209     c* mi riposiziono per riprovare con la stessa scadenza
356400101209    >C     K08Rca04      Setll     NDRCA04L
356500101209     c                   iter
356600101209     c                   endif
3567001012093-   C                   ENDIF
356800101209     c*
356900101210     c* non chiude le fasi impegno
357000140619     c**                 if        RcaAzione='98'or
357100140619     c**                           RcaAzione='99'
357200140619     c**** inibito       iter
357300140619     c**                 endif
357400101210     c*
357500101209     C                   if        *in21=*off
357600101209     c                             and RcaDtChiu=*loval
357700110412D2680x*************      eval      RcaDtChiu=Zdateiso
357800110412D2680c     *jobrun       move      D5DtChiu      RcaDtChiu
357900101209     c                   eval      RcaUteChiu=sdsusr
358000101209     c                   eval      RcaStato='1'
358100130913C2226C                   If        D5Nota <> *blanks
358200130913  "  C                   eval      RcaNota = D5Nota
358300130913C2226C                   Endif
358400101209     c                   update    NdRCA04
358500101209     C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
358600101209     c                   move      *on           Winzs1
358700101209     c                   ENDIF
358800101209     c*
358900101209     c                   enddo
359000130924C2226c                   ENDIF
359100101209     c*
359200101209c2176c                   endsr
359300940223     C/EJECT
359400101209c2176C******************************************************
359500101209  "  C* PGM PER GESTIONE MESSAGGI ERRORE                   *
359600101209  "  C******************************************************
359700101209c2176C     MSGPGM        BEGSR
359800101209     C*
359900101209     C                   Z-ADD     2             MSGMSG
360000101209     C                   MOVEL     'S'           MSGRSP
360100101209     C                   MOVEL     'CR'          MSGTPR
360200101209     C                   MOVEL     'S'           MSGEMV
360300101209     C                   Z-ADD     10            MSGRGP
360400101209     C                   MOVEL     'S'           MSGLCK
360500101209     C                   MOVEA     STATUS        STS
360600101209     C                   MOVEL     'N'           MSGCNL
360700101209     C                   MOVEL     'N'           MSGVID
360800101209     C                   MOVEL     'N'           MSGOPE
360900101209     C                   MOVEL     'N'           MSGSTP
361000101209     C                   CALL      'XMSGR'
361100101209     C                   PARM                    XMSGDS
361200101209     C*
361300101209c2176C                   ENDSR
361400101209     C/EJECT
361500100426c2156C************************************************************
361600100426     C* GESTIONE RITORNO DA PGM DI GESTIONE
361700100426     C************************************************************
361800100426c2156C     RTNL77        BEGSR
361900100426     C*
362000100426     C* modo di ritorno
362100100426     C*
362200100426     C                   SELECT
362300100426     C* questi modi di utilizzo dei valori di ritorno dal
362400100426     C* pgm di manutenzione rcd di anagrafica sono delle
362500100426     C* proposte, normalmente sempre valide, ma modificabili
362600100426     C* per situazioni particolari
362700100426     C* 1 = F3
362800100426    >C     RETL77        WHENEQ    '1'
362900100426     C                   MOVE      *ON           $ESCI                          sospese READC
363000100426     C                   MOVE      *ON           $FINE                          fine pgm
363100100426    >C                   MOVE      '1'           RETL76
363200100426     C* 2 = F12
363300100426    >C     RETL77        WHENEQ    '2'
363400100426     C* se il pgm di manutenzione ha prima una guida, è necessario anche
363500100426     c* questo test, altrimenti prevale il ret002 e il sfl non viene
363600100426     C* ricaricato
363700100426    >C     OPRL77        ANDNE     '1'
363800100426     C                   MOVE      *ON           $ESCI                          sospese READC
363900100426     C*
364000100426     C                   ENDSL
364100100426     C*
364200100426     C* operazione eseguite dal pgm chiamato
364300100426     C*
364400100426     C                   SELECT
364500100426     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
364600100426    >C     OPRL77        WHENEQ    '1'
364700100426     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
364800100426     C*
364900100426     C                   ENDSL
365000100426     C*
365100100426     C* funzione non eseguibile per errore :
365200100426     C*
365300100426     C                   ENDSR
365400100426     C/EJECT
365500950301     C************************************************************
365600950301     C* AGGIORNAMENTO RIGA LISTA
365700950301     C************************************************************
365800950301     C     AGGS1         BEGSR
365900950301     C*
366000950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
366100950301     C* di manutenzione dopo una opzione 02
366200950301     C*
366300950301     C*
366400950301     C                   ENDSR
366500941124     C/EJECT
366600940131     C************************************************************
366700940131     C* GESTIONE VALORI ASSUNTI
366800940131     C************************************************************
366900940131     C     GESD2         BEGSR
367000940131     C*
367100941118   B1C     $INZD2        IFEQ      *ON
367200940131     C                   EXSR      INZD2
367300940223     C                   MOVE      *OFF          $INZD2
367400941118   E1C                   ENDIF
367500940202     C*
367600941118   B1C     $LASTV        IFNE      'D2'
367700940923     C                   EXSR      GESPIE
367800961023   B1C     $LASTV        IFNE      *HIVAL
367900940202     C                   MOVE      $LASTV        $LASTG
368000961023     C                   ENDIF
368100940202     C                   MOVE      'D2'          $LASTV
368200941118   E1C                   ENDIF
368300940131     C*
368400940707     C* se esistono errori sulla videata
368500940707     C* emetto la write del formato a indicatori spenti per vedere
368600940707     C* le eventuali decodifiche
368700941118   B1C     *IN99         IFEQ      *ON
368800940707     C                   MOVEA     *IN           WIN
368900940707     C                   MOVE      *ALL'0'       IN5098           49
369000940707     C                   MOVEA     IN5098        *IN(50)
369100940707     C                   WRITE     FMTD2
369200940707     C                   MOVEA     WIN           *IN
369300941118   E1C                   ENDIF
369400940707     C*
369500940131     C                   EXFMT     FMTD2
369600960605     C*
369700960605     C* azzero la posizione del cursore
369800960605     C                   Z-ADD     0             H1RIGA
369900960605     C                   Z-ADD     0             H1COLO
370000941205     C*
370100951005     C* CONTROLLO ABILITAZIONE TASTI
370200951005     C* INPUT
370300951005     C* $FC3 = Abilitazioni videata D2
370400951005     C* OUTPUT
370500951005     C* *IN97 + *IN99 Errori
370600951005     C                   MOVEA     $FC3          $FC
370700951005     C                   EXSR      CTRKEY
370800941205     C*
370900941205   B1C     *IN99         IFEQ      *OFF
371000941205     C*
371100941205   B2C                   SELECT
371200090324     C* F2=Decodifica
371300090324     C     $TASTO        WHENEQ    F02
371400090324     C                   EXSR      F02D2
371500940131     C* F3=Fine
371600940506     C     $TASTO        WHENEQ    F03
371700940309     C                   EXSR      F03D2
371800090324     C* F4=Ricerca
371900090324     C     $TASTO        WHENEQ    F04
372000090324     C                   EXSR      SEARCH
372100940131     C* F12=Ritorno
372200940506     C     $TASTO        WHENEQ    F12
372300940309     C                   EXSR      F12D2
372400940916     C* F24=Altri tasti
372500940916     C     $TASTO        WHENEQ    F24
372600940916     C                   EXSR      F24D2
372700940131     C*
372800940131     C                   OTHER
372900940131     C                   EXSR      CTRD2
373000941205   B3C     *IN99         IFEQ      *OFF
373100940211     C                   MOVE      $LASTG        $GEST
373200941205   B4C                   SELECT
373300940202     C     $LASTG        WHENEQ    'S1'
373400940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
373500941205   B5C     $ORD          IFNE      D2ORD
373600940223     C                   MOVE      *ON           $INZS1
373700940302     C                   MOVE      *BLANK        W1POS
373800941205   E5C                   ENDIF
373900130912C2226C     $LASTG        WHENEQ    'S2'
374000130912  "  C* se è stato modificato il raggruppamento ricarico prelista
374100130912  "  C     $RGRPRE       IFNE      D2RGRPRE
374200130912  "  C                   MOVE      *ON           $INZS2
374300130912  "  C                   MOVE      *BLANK        W2POS
374400130912C2226C                   ENDIF
374500940202     C     $LASTG        WHENEQ    'D1'
374600940318     C* per ora niente
374700941205   E4C                   ENDSL
374800940131     C* definisco i nuovi valori di default
374900940201     C                   Z-ADD     D2LOP         $LOP
375000940131     C                   Z-ADD     D2ORD         $ORD
375100130912C2226C                   If        D2Pre = XscOn
375200130912  "  C                   Movel     *on           $Pre
375300130910  "  C                   Else
375400130912  "  C                   Movel     *off          $Pre
375500130912  "  C                   Endif
375600130912C2226C                   Movel     D2RgrPre      $RgrPre
375700950322     C*
375800950322     C* aggiorno valori assunti
375900950328     C* se richiesto il salvataggio
376000950328   B4C     D2BSV         IFEQ      1
376100950322     C                   EXSR      AGGMPE
376200950322   E4C                   ENDIF
376300950322     C*
376400941205   E3C                   ENDIF
376500940131     C*
376600941205   E2C                   ENDSL
376700940131     C*
376800941205   E1C                   ENDIF
376900941205     C*
377000940131     C                   ENDSR
377100940223     C/EJECT
377200940131     C************************************************************
377300940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
377400940131     C************************************************************
377500940201     C     INZD2         BEGSR
377600940131     C*
377700940131     C                   CLEAR                   FMTD2
377800130910C2226C                   EVAL      D2On1 = Xscon
377900130912C2226C                   EVAL      D2Off1= Xscoff
378000021010     C                   EVAL      D2BSVA = Normale
378100950328     C* se sono entrato con chiave univoca
378200950328     C* impedisco il salvataggio valori assunti
378300950606     C     $Unique       IFEQ      *ON
378400021010     C                   EVAL      D2BSVA = Protetto
378500950328     C                   ENDIF
378600940201     C                   MOVEL     $LOP          D2LOP
378700940831     C                   MOVE      $ORD          D2ORD
378800130924C2226C                   If        $pre = ' '
378900130924  "  C                   Movel     *on           $pre
379000130924  "  C                   Movel     'P'           $RgrPre
379100130924  "  C                   Endif
379200130924  "  C                   If        $Pre = *on
379300130912  "  C                   Movel     XscOn         D2Pre
379400130910  "  C                   Else
379500130912  "  C                   Movel     XscOff        D2Pre
379600130920  "  C                   Endif
379700130924C2226C                   Movel     $RgrPre       D2RgrPre
379800090324     c* decodifica
379900090324     C                   EXSR      CTRD2
380000090324     C*
380100090324     C                   MOVE      *ALL'0'       IN5098
380200090324     C                   MOVEA     IN5098        *IN(50)
380300090324     C*
380400940131     C*
380500940131     C                   ENDSR
380600940309     C/EJECT
380700090324     C************************************************************
380800090324     C* GESTIONE F02 VIDEO D1
380900090324     C************************************************************
381000090324     C     F02D2         BEGSR
381100090324     C*
381200090324     C* per decodificare la videata richiamo la routine dei ctrl
381300090324     C* e poi disattivo gli eventuali messaggi di errore
381400090324     C                   EXSR      CTRD2
381500090324     C*
381600090324     C                   MOVE      *ALL'0'       IN5098
381700090324     C                   MOVEA     IN5098        *IN(50)
381800090324     C*
381900090324     C                   ENDSR
382000090324     C/EJECT
382100940309     C************************************************************
382200940309     C* GESTIONE F03 VIDEO D2
382300940309     C************************************************************
382400940309     C     F03D2         BEGSR
382500940309     C*
382600940309     C                   MOVE      *ON           $FINE
382700940325     C* fine programma
382800940325     C                   EXSR      ENDPGM
382900940309     C*
383000940309     C                   ENDSR
383100940309     C/EJECT
383200940309     C************************************************************
383300940309     C* GESTIONE F12 VIDEO D2
383400940309     C************************************************************
383500940309     C     F12D2         BEGSR
383600940309     C*
383700940309     C                   MOVE      $LASTG        $GEST
383800940309     C*
383900940309     C                   ENDSR
384000940916     C/EJECT
384100940916     C************************************************************
384200940916     C* GESTIONE F24 VIDEO D2
384300940916     C************************************************************
384400940916     C     F24D2         BEGSR
384500940916     C*
384600940916     C                   MOVEA     $FX3          $FX
384700940916     C                   MOVEA     $FC3          $FC
384800940916     C                   MOVEA     $FR3          $FR
384900940916     C                   Z-ADD     $ULKD2        $ULK
385000950726     C                   Z-ADD     $D2L01        $L01
385100950726     C                   Z-ADD     $D2L02        $L02
385200940916     C                   EXSR      RIEKEY
385300940916     C                   Z-ADD     $ULK          $ULKD2            3 0
385400940916     C                   MOVEA     $FC           $FC3
385500940916     C                   MOVEL     $KEY1         Z3KE1
385600060224     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
385700090324     C                   MOVEL     $KEY2         Z3KE2
385800940916     C                   WRITE     FMTZ3
385900940916     C*
386000940916     C                   ENDSR
386100940223     C/EJECT
386200940131     C************************************************************
386300940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
386400940131     C************************************************************
386500940131     C     CTRD2         BEGSR
386600940131     C*
386700940131     C                   SETOFF                                       99
386800940131     C*
386900940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
387000940201     C     D2LOP         IFEQ      0
387100940131     C                   SETON                                          5099
387200941027     C                   ENDIF
387300940131     C*
387400940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
387500940131     C     D2ORD         IFEQ      0
387600940131     C                   SETON                                          5199
387700941027     C                   ENDIF
387800090324     c* Controllo ordinamento
387900090324     C                   RESET                   X07DS
388000090324     C                   MOVE      '1'           X07RIC
388100090324     C                   MOVEL     'ANDIZ000'    X07TRC
388200100401     C                   MOVEL     'DSPORDRCA'   X07CAM
388300090324     C                   MOVEL     D2ORD         X07VAL
388400090324     C                   MOVE      *OFF          X07ALL
388500090324     C                   CALLB     'X07VALR'
388600090324     C                   PARM                    X07DS
388700090324   B3C     X07ERR        IFEQ      '0'
388800090324     C                   MOVEL     X07DES        D2ORDD
388900090324     c                   else
389000090324     C                   SETON                                        5299
389100090324     C                   ENDIF
389200130910C2226C* CONTROLLO FLAG PRELISTA
389300130912  "  c                   if        D2Pre <> XscOn and
389400130912  "  c                             D2Pre <> XscOff
389500130910  "  c                   seton                                        5399
389600130912  "  c                   endif
389700130912  "  C* CONTROLLO FLAG Raggruppamento obbligatorio
389800130912  "  c                   if        D2RgrPre = ' ' and
389900130912  "  c                             D2Pre = XscOn
390000130912  "  c                   seton                                        5499
390100130912  "  c                   endif
390200130912  "  c* Controllo values raggruppamento
390300130912  "  C                   RESET                   X07DS
390400130912  "  C                   MOVE      '1'           X07RIC
390500130912  "  C                   MOVEL     'ANDIZ000'    X07TRC
390600130912  "  C                   MOVEL     'DSPRGRSCL'   X07CAM
390700130912  "  C                   MOVEL     D2RGRPre      X07VAL
390800130912  "  C                   MOVE      *OFF          X07ALL
390900130912  "  C                   CALLB     'X07VALR'
391000130912  "  C                   PARM                    X07DS
391100130912  "  C     X07ERR        IFEQ      '0'
391200130912  "  C                   MOVEL     X07DES        D2RgrPreD
391300130912  "  c                   else
391400130912  "  C                   SETON                                        5599
391500130912C2226C                   ENDIF
391600940131     C*
391700940131     C                   ENDSR
391800941205     C/EJECT
391900941205     C************************************************************
392000941205     C* CONTROLLO ABILITAZIONE TASTI
392100941205     C* INPUT
392200941205     C* $TASTO =Tasto funzione premuto
392300941205     C* $FC = Tasti abilitati
392400941205     C* OUTPUT
392500941205     C* IN99 = ERrore
392600941205     C************************************************************
392700941205     C     CTRKEY        BEGSR
392800941205     C*
392900951005     C                   SETOFF                                       99
393000951005     C*
393100941205     C                   SELECT
393200941205     C     $TASTO        WHENEQ    F01
393300960202     C                   z-add     01            posfun
393400941205     C     $FC(1)        IFEQ      '0'
393500951005     C                   SETON                                        9997
393600941205     C                   ENDIF
393700941205     C     $TASTO        WHENEQ    F02
393800960202     C                   z-add     02            posfun
393900941205     C     $FC(2)        IFEQ      '0'
394000951005     C                   SETON                                        9997
394100941205     C                   ENDIF
394200941205     C     $TASTO        WHENEQ    F03
394300960202     C                   z-add     03            posfun
394400941205     C     $FC(3)        IFEQ      '0'
394500951005     C                   SETON                                        9997
394600941205     C                   ENDIF
394700941205     C     $TASTO        WHENEQ    F04
394800960202     C                   z-add     04            posfun
394900941205     C     $FC(4)        IFEQ      '0'
395000951005     C                   SETON                                        9997
395100941205     C                   ENDIF
395200941205     C     $TASTO        WHENEQ    F05
395300960202     C                   z-add     05            posfun
395400941205     C     $FC(5)        IFEQ      '0'
395500951005     C                   SETON                                        9997
395600941205     C                   ENDIF
395700941205     C     $TASTO        WHENEQ    F06
395800960202     C                   z-add     06            posfun
395900941205     C     $FC(6)        IFEQ      '0'
396000951005     C                   SETON                                        9997
396100941205     C                   ENDIF
396200941205     C     $TASTO        WHENEQ    F07
396300960202     C                   z-add     07            posfun
396400941205     C     $FC(7)        IFEQ      '0'
396500951005     C                   SETON                                        9997
396600941205     C                   ENDIF
396700941205     C     $TASTO        WHENEQ    F08
396800960202     C                   z-add     08            posfun
396900941205     C     $FC(8)        IFEQ      '0'
397000951005     C                   SETON                                        9997
397100941205     C                   ENDIF
397200941205     C     $TASTO        WHENEQ    F09
397300960202     C                   z-add     09            posfun
397400941205     C     $FC(09)       IFEQ      '0'
397500951005     C                   SETON                                        9997
397600941205     C                   ENDIF
397700941205     C     $TASTO        WHENEQ    F10
397800960202     C                   z-add     10            posfun
397900941205     C     $FC(10)       IFEQ      '0'
398000951005     C                   SETON                                        9997
398100941205     C                   ENDIF
398200941205     C     $TASTO        WHENEQ    F11
398300960202     C                   z-add     11            posfun
398400941205     C     $FC(11)       IFEQ      '0'
398500951005     C                   SETON                                        9997
398600941205     C                   ENDIF
398700941205     C     $TASTO        WHENEQ    F12
398800960202     C                   z-add     12            posfun
398900941205     C     $FC(12)       IFEQ      '0'
399000951005     C                   SETON                                        9997
399100941205     C                   ENDIF
399200941205     C     $TASTO        WHENEQ    F13
399300960202     C                   z-add     13            posfun
399400941205     C     $FC(13)       IFEQ      '0'
399500951005     C                   SETON                                        9997
399600941205     C                   ENDIF
399700941205     C     $TASTO        WHENEQ    F14
399800960202     C                   z-add     14            posfun
399900941205     C     $FC(14)       IFEQ      '0'
400000951005     C                   SETON                                        9997
400100941205     C                   ENDIF
400200941205     C     $TASTO        WHENEQ    F15
400300960202     C                   z-add     15            posfun
400400941205     C     $FC(15)       IFEQ      '0'
400500951005     C                   SETON                                        9997
400600941205     C                   ENDIF
400700941205     C     $TASTO        WHENEQ    F16
400800960202     C                   z-add     16            posfun
400900941205     C     $FC(16)       IFEQ      '0'
401000951005     C                   SETON                                        9997
401100941205     C                   ENDIF
401200941205     C     $TASTO        WHENEQ    F17
401300960202     C                   z-add     17            posfun
401400941205     C     $FC(17)       IFEQ      '0'
401500951005     C                   SETON                                        9997
401600941205     C                   ENDIF
401700941205     C     $TASTO        WHENEQ    F18
401800960202     C                   z-add     18            posfun
401900941205     C     $FC(18)       IFEQ      '0'
402000951005     C                   SETON                                        9997
402100941205     C                   ENDIF
402200941205     C     $TASTO        WHENEQ    F19
402300960202     C                   z-add     19            posfun
402400941205     C     $FC(19)       IFEQ      '0'
402500951005     C                   SETON                                        9997
402600941205     C                   ENDIF
402700941205     C     $TASTO        WHENEQ    F20
402800960202     C                   z-add     20            posfun
402900941205     C     $FC(20)       IFEQ      '0'
403000951005     C                   SETON                                        9997
403100941205     C                   ENDIF
403200941205     C     $TASTO        WHENEQ    F21
403300960202     C                   z-add     21            posfun
403400941205     C     $FC(21)       IFEQ      '0'
403500951005     C                   SETON                                        9997
403600941205     C                   ENDIF
403700941205     C     $TASTO        WHENEQ    F22
403800960202     C                   z-add     22            posfun
403900941205     C     $FC(22)       IFEQ      '0'
404000951005     C                   SETON                                        9997
404100941205     C                   ENDIF
404200941205     C     $TASTO        WHENEQ    F23
404300960202     C                   z-add     23            posfun
404400941205     C     $FC(23)       IFEQ      '0'
404500951005     C                   SETON                                        9997
404600941205     C                   ENDIF
404700941205     C     $TASTO        WHENEQ    F24
404800960202     C                   z-add     24            posfun
404900941205     C     $FC(24)       IFEQ      '0'
405000951005     C                   SETON                                        9997
405100941205     C                   ENDIF
405200941205     C                   ENDSL
405300941205     C*
405400941205     C                   ENDSR
405500940223     C/EJECT
405600940201     C************************************************************
405700940201     C* AGGIORNAMENTO VALORI ASSUNTI
405800940201     C************************************************************
405900940201     C     AGGMPE        BEGSR
406000940201     C*
406100950524     C* preparo i parametri di chiamata al driver
406200950524     C                   CLEAR                   XMPEDS
406300950524     C                   EVAL      TpChiamata = '03'
406400950524     C                   EVAL      NChiavi = 5
406500950524     C                   EVAL      VLogica = 2
406600950524     C*
406700950524     C* preparo le chiavi per la chiamata al driver
406800950524     C                   CLEAR                   XMPEDS1
406900950524     C                   MOVEL     XSCSOC        DSMPESOC
407000950524     C                   MOVEL     DSPGM         DSMPEPGM
407100950524     C                   MOVEL     KNMUS         DSMPEPRF
407200941013     C*
407300950322     C* Si memorizzano i dati di default a seconda delle modalità con
407400950322     C* cui il pgm è usato
407500950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
407600950322     C* MPETIP = 'PV'  pgm richiamato da menù
407700950322     C* MPETIP = 'PB'  x lancio Batch
407800950322     C*
407900941013     C                   SELECT
408000090323    >C     OPZL76        WHENEQ    '01'
408100950524     C                   MOVEL     'PX'          DSMPETIP
408200941013     C                   OTHER
408300950524     C                   MOVEL     'PV'          DSMPETIP
408400941013     C                   ENDSL
408500950524     C* dati
408600950524     C                   MOVEL     DSMPE         DSMPEPAR
408700950524     C*
408800950524     C                   CALLB     'XMPE'
408900950524     C                   PARM                    XMPEDS
409000950524     C                   PARM                    XMPEDS1
409100940201     C*
409200940201     C                   ENDSR
409300940223     C/EJECT
409400940131     C************************************************************
409500940131     C* OPERAZIONI INIZIALI
409600940131     C************************************************************
409700940131     C     *INZSR        BEGSR
409800070711     C     *DMY          Move      Udate         ZDateISO
409900940131     C*
410000940127     C* Reperimento parametri
410100940127     C*
410200940117     C     *ENTRY        PLIST
410300940117     C                   PARM                    KPJBA
410400950410     C                   PARM                    DSAUT
410500950410     C*
410600951010     C* testo se sono stati passati i dati di autorizzazione
410700951010     C*
410800950410     C     PARMS         IFLT      2
410900950410     C                   MOVE      *BLANK        AUTDS
411000950410     C                   ELSE
411100950410     C                   MOVEL     DSAUT         AUTDS
411200950410     C                   ENDIF
411300951206     C*
411400951206     C* Reperimento dati società
411500951206     C*
411600970424     C                   MOVEL     'SOC001'      TIPXSC
411700970424     C                   MOVEL     *BLANK        SOCXSC
411800951206     C                   EXSR      REPSOC
411900960409     C*
412000960409     C* Controlli dati società
412100960409     C*
412200960409     C     RTNXSC        IFNE      '1'
412300970424     C                   MOVEL     XSOCDS        SOC001
412400960409     C                   ELSE
412500960409     C                   MOVE      *ON           $FINE
412600090323    >C                   MOVE      '1'           ErrL76
412700960409     C                   EXSR      ENDPGM
412800960409     C                   ENDIF
412900070725     c*
413000090323     c                   open      ndcL76d                              13
413100070725     c* emetto messaggio per video non a 132
413200070725     c                   if        *in13=*on
413300070725     c                   move      'COS2651'     XmsgCd1
413400070725     C                   CALL      'XMSGR'
413500070725     C                   PARM                    XMSGDS
413600070725     C                   PARM                    XMStxt          135
413700070725     C                   PARM                    XMSGinz           1
413800070725     C                   PARM                    XMSGCd1           7
413900070725     C                   PARM                    XMSGCd2           7
414000070725     C                   PARM                    XMSGCd3           7
414100070725     C                   PARM                    XMSGSn1           7
414200070725     C                   PARM                    XMSGCdTxT        63
414300070725     C                   PARM                    MSGCdTit         14
414400070725     C                   PARM                    MSGClPrtf         1
414500070725     C                   PARM                    VarMsg1         100
414600070725     C                   PARM                    VarMsg2         100
414700070725     C                   PARM                    VarMsg3         100
414800090323    >C                   MOVE      '1'           ErrL76
414900070725     c                   exsr      endpgm
415000070725     c                   endif
415100950726     C*
415200950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
415300070618     C                   EVAL      $D1L01 = %Size(Z1Ke)
415400950726     C                   EVAL      $D1L02 = %Size(Z1Ke2)
415500070618     C                   EVAL      $S1L01 = %Size(Z2Ke)
415600950726     C                   EVAL      $S1L02 = %Size(Z2Ke2)
415700070531     C                   EVAL      $C1Op1 = %Size(C1Opz)
415800950926     C                   EVAL      $C1Op2 = %Size(C1Opz2)
415900950906     C                   EVAL      $D2L01 = %Size(Z3Ke1)
416000090324     C                   EVAL      $D2L02 = %size(z3ke2)
416100130911C2226C                   EVAL      $S2L01 = %Size(Z2Ke)
416200130911  "  C                   EVAL      $S2L02 = %Size(Z2Ke2)
416300130911  "  C                   EVAL      $C2Op1 = %Size(C2Opz)
416400130911C2226C                   EVAL      $C2Op2 = %Size(C2Opz2)
416500940225     C*
416600940223     C* Reperimento opzioni
416700940127     C*
416800940128     C                   EXSR      REPOPZ
416900950406     C*
417000940506     C* Reperimento tasti di funzione
417100940506     C*
417200940506     C                   EXSR      REPKEY
417300960117     C* Chiamata a routine per il reperimento delle opzioni
417400960117     C* personalizzate
417500960117     C                   CAllb     'X46USREXT'
417600960117     C                   PARM                    $OP1
417700960117     C                   PARM                    $TX1
417800960117     C                   PARM                    $FL1
417900960117     C                   PARM                    $PGO1
418000960117     C                   PARM      $dimOp        NumOpz
418100960202     C                   PARM                    $FX2
418200960202     C                   PARM                    $FC2
418300960202     C                   PARM                    $PGF2
418400960117     C                   PARM                    DsPgm
418500960117     C                   PARM                    xsccut
418600960117     C                   PARM                    xscgrp
418700960117     C*
418800960118     C                   Z-add     1             OPZLEN
418900960118     C     *blank        Lookup    $TX1(opzlen)                           21
419000960117     C                   if        *in21 = *on
419100960117     C                   sub       1             OPZLEN
419200960117     C                   else
419300960117     C                   Z-add     $DimOp        OPZLEN
419400960117     C                   endif
419500940127     C*
419600940117     C                   ENDSR
419700940223     C/EJECT
419800940223     C************************************************************
419900940223     C* INIZIALIZZAZIONE VARIABILI
420000940223     C************************************************************
420100940223     C     INZVAR        BEGSR
420200941215     C*
420300941215     C* Dati da PGM chiamante
420400941215     C*
420500090323    >C                   MOVEL     KPJBU         NdcL76DS
420600951206     C*
420700951206     C* Se società diversa da quella impostata nella *inzsr, rieseguo
420800951206     C* reperimento dati società
420900090323    >C                   IF        SocietaL76 <> xscsoc  and
421000090323    >C                             OpzL76 <>*blank
421100960119     C                   MOVEL     'SOC001'      TIPXSC
421200090323    >C                   MOVEL     SocietaL76    SOCXSC
421300951122     C                   EXSR      REPSOC
421400951010     C     RTNXSC        IFNE      '1'
421500970424     C                   MOVEL     XSOCDS        SOC001
421600951010     C                   ELSE
421700951010     C                   MOVE      *ON           $FINE
421800090323    >C                   MOVE      '1'           ErrL76
421900951010     C                   EXSR      ENDPGM
422000951010     C                   ENDIF
422100960410     C                   ENDIF
422200090325     C*
422300090325     C* Reperimento autorizzazioni
422400090325     C*
422500090325     C* le autorizzazioni potrebbero essere già state passate,
422600090325     C* nel qual caso non vanno reperite
422700090325    >C     AUTDS         IFEQ      *BLANK
422800090325    >C                   EXSR      REPAUT
422900090325    >C                   ELSE
423000100409    >C                   MOVEL     XCADDS        CONRECDS
423100090325     C* Se non autorizzati alla funzione oppure                              ed esco
423200100409    >C     VisATT        IFNE      *ON
423300090325     C                   MOVE      XCAFNC        XNOFNC
423400090325     C                   MOVE      XCAPRF        XNOPRF
423500090325     C                   MOVE      XCACUT        XNOCUT
423600090325     C                   MOVE      XCATCK        XNOTCK
423700090325     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
423800090325     C                   CALL      'XNOAUTR'
423900090325     C                   PARM                    XNOADS
424000090325    >C                   MOVE      '2'           ErrL76
424100090325     C                   MOVE      *ON           $fine
424200090325     C                   EXSR      ENDPGM
424300090325     C                   ENDIF
424400090325    >C                   ENDIF
424500090324     C*
424600090324     c* reperimento dati parametro modulo solleciti
424700090324     C                   movel     'NDCL1S'      xpapar
424800090324     C                   movel     *blank        xparic
424900090324     C                   CALLb     'XPAR'
425000090324     C                   PARM                    xscsoc
425100090324     C                   PARM                    XPAPAR            8
425200090324     C                   PARM                    XPAOUT
425300090324     C                   PARM                    XPAERR            1
425400090324     C                   PARM                    XPARIC            1
425500090324     C     XPAERR        IFEQ      '0'
425600090324     C                   movel(p)  XpaOut        Anp057Ds
425700090324     c                   else
425800090324     c                   seton                                        lr
425900090324     c                   return
426000090324     C                   endif
426100941026     C*
426200941026     C* Reperimento valori default utente
426300941026     C*
426400941026     C                   EXSR      REPDFT
426500940506     C*
426600940506     C* Definizione delle opzioni utilizzabili a pgm
426700940506     C*
426800940506     C                   EXSR      DEFOPZ
426900950330     C*
427000940223     C* Variabili per gestione videate
427100940223     C*
427200940223     C                   MOVE      *OFF          $FINE                          fine pgm
427300940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
427400940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
427500130911C2226C                   MOVE      *OFF          $INZS2                         inizializzione S1
427600940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
427700950606     C                   MOVE      *OFF          $EndFile                       memo fine file
427800130911C2226C                   MOVE      *OFF          $EndFilePre                    memo fine file
427900940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
428000940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
428100130911C2226C                   MOVE      *OFF          $RCDOKPre                      rcd letto valido
428200940509     C                   MOVE      *HIVAL        $LASTV
428300940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
428400940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
428500940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
428600130911C2226C                   Z-ADD     0             $ULKS2            3 0          piede da emett. S1
428700960605     C* azzero la posizione del cursore
428800960605     C                   Z-ADD     0             H1RIGA
428900960605     C                   Z-ADD     0             H1COLO
429000940223     C*
429100940318     C                   MOVE      *ON           $INZD1
429200940318     C                   MOVE      *ON           $INZS1
429300130911C2226C                   MOVE      *ON           $INZS2
429400940831     C*
429500940831     C* se pgm richiamato con chiave univoca impostata,
429600940831     C* forzo la emissione della lista e imposto l'ordinamento
429700090323    >C*    OpzL76        IFNE      *BLANK
429800070524    >C*    UQ1001        IFNE      *BLANK
429900070524     C*                  MOVE      *ON           $Unique
430000070524     C*                  Z-ADD     2             $LOP
430100070524     C*                  Z-ADD     99            $ORD
430200070524     C*                  ELSE
430300070524     C*                  MOVE      *OFF          $Unique
430400070524     C*                  ENDIF
430500070524     C*                  ENDIF
430600950330     C*
430700950330     C* Variabili appoggio
430800950330     C*
430900950330     C                   Z-ADD     SFLPAG        WPAG
431000130924C2226C                   Z-ADD     SFLPAG        WPAG2
431100950330     C                   MOVEL     *OFF          WOPR
431200950515     C                   CLEAR                   W1POS
431300941104     C*
431400941104     C* definisco qual'è la prima videata
431500950410     C* (se la prima videata risulta la lista, devo comunque caricare
431600950410     C*  la videata di parzializzazione e gestire la definizione
431700950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
431800950410     C*  passati dei dati, può variare il caricamento)
431900940223     C                   SELECT
432000940223     C     $LOP          WHENEQ    1
432100940223     C                   MOVE      'D1'          $GEST
432200940223     C     $LOP          WHENEQ    2
432300130912C2226C                   IF        $Pre = *on
432400130911  "  C                   MOVE      'S2'          $GEST
432500130911C2226C                   Else
432600940223     C                   MOVE      'S1'          $GEST
432700130911C2226C                   Endif
432800941104     C                   EXSR      INZD1
432900090325     C                   EXSR      CTRD1
433000090325     C                   MOVE      *ALL'0'       IN5098
433100090325     C                   MOVEA     IN5098        *IN(50)
433200941104     C                   MOVE      *OFF          $INZD1
433300950410     C                   EXSR      DEFORD
433400941104     C                   ENDSL
433500940223     C*
433600940223     C* Valorizzazione campi univoci testate
433700940223     C*
433800940223     C                   CLEAR                   FMTT1
433900940223     C                   MOVEL     KNSIF         NOMSIF
434000941109     C                   MOVEL     XSCDSI        NOMDIT
434100941027     C                   MOVEL     DSPGM         NOMPGM
434200950331     C* recupero il sottotitolo (se c'è)
434300950331     C                   MOVEL     *BLANK        T1TIT
434400950331     C     SOTTIT        IFNE      *BLANK
434500950331     C                   MOVEL     SOTTIT        IDMSG
434600950522     C                   CALLB     'XRTVM'
434700950331     C                   PARM                    IDMSG            27
434800950331     C                   PARM                    TXTMSG          100
434900950331     C                   PARM                    ERRMSG            1
435000950331     C                   PARM      *ON           CTRMSG            1            centratura
435100950331     C                   PARM      30            LENMSG            3 0          lun output.
435200950331     C     ERRMSG        IFNE      *ON
435300950331     C                   MOVEL     TXTMSG        T1TIT
435400950331     C                   ELSE
435500950331     C                   MOVEL     *ALL'?'       T1TIT
435600950331     C                   ENDIF
435700950331     C                   ENDIF
435800940223     C*
435900940223     C                   ENDSR
436000941209     C/EJECT
436100070525     C************************************************************
436200070525     C* Composizione stringa sql
436300070525     C************************************************************
436400070525     C     InzSql        BEGSR
436500130912     C*
436600100407     C                   EVAL      SQ='Select distinct RcaSocieta,           +
436700100407     C                                 RcaPosiz, RcaAttrib, Rcakcc, Rcaksc,  +
436800130912     C                                 RcaDtPar, RcaNrPar, RcaSerPar, RcaDivisa+
436900130912     C                                 ,RcaKscAge from NDRCA00F'
437000130912C2226C                   If        $Pre = *on
437100140310  "  C                   Eval      SQ=%trimr(SQ) + ',ANRCO00F,ANSOG00F '
437200140310      *
437300140310     C*                  if        d1insoluti = xscon
437400140310     C*                  Eval      SQ=%trimr(SQ) + ' inner join ndscd00f on +
437500140310     C*                            scdsocieta=rcasocieta and +
437600140310     C*                            scdctb = ''CG'' and scdtpscad=''0'' +
437700140310     C*                            scdkcc=rcakcc and +
437800140310     C*                            scdksc=rcaksc and +
437900140310     C*                            scddtpar=rcadtpar and +
438000140310     C*                            scdnrpar=rcanrpar and +
438100140310     C*                            scdserpar=rcaserpar and +
438200140310     C*                            scddtsca=rcadtsca '
438300140310     C*                  endif
438400140310      *
438500140310     C                   Eval      SQ=%trimr(SQ) +
438600140310  "  C                             ' Where RcaKcc=RcoKcc and RcaKsc=RcoKsc and +
438700140310  "  C                             RcoSogg = SogSogg and '
438800130918  "  C                   If        $RgrPre = 'P'
438900130911  "  C                   Cat       'SogPartiva':1Sq
439000130911  "  C                   Cat       '='         :1Sq
439100130911  "  C                   Cat       ''''        :0Sq
439200130911  "  C                   Cat       H2Partiva   :0Sq
439300130911  "  C                   Cat       ''''        :0Sq
439400130911  "  C                   Cat       'and'       :1Sq
439500130911  "  C                   Cat       'SogCdFisc' :1Sq
439600130911  "  C                   Cat       '='         :1Sq
439700130911  "  C                   Cat       ''''        :0Sq
439800130911  "  C                   Cat       H2CdFisc    :0Sq
439900130912  "  C                   Cat       ''''        :0Sq
440000130912  "  C                   Cat       'and'       :1Sq
440100130912  "  C                   Else
440200130912  "  C                   Cat       'SogSogg'   :1Sq
440300130912  "  C                   Cat       '='         :1Sq
440400130912  "  C                   Cat       ''''        :0Sq
440500130912  "  C                   Cat       H2Sogg      :0Sq
440600130912  "  C                   Cat       ''''        :0Sq
440700130912  "  C                   Cat       'and'       :1Sq
440800130912  "  C                   Endif
440900130912C2226C                   Else
441000130912     C*
441100070525     C                   Cat       'Where'     :1Sq
441200130911C2226C                   Endif
441300100401     c* CAMPI OBBLIGATORI
441400070525     c* Societa
441500100401     C                   Cat       'Rcasocieta':1Sq
441600100401     C                   Cat       '='         :1Sq
441700100401     C                   Cat       ''''        :0Sq
441800070525     C                   Cat       xscsoc      :0Sq
441900100401     C                   Cat       ''''        :0Sq
442000130910C2226X*** unita (se gestita dai solleciti)
442100130910C2226C* unita (se gestita nella attività di recupero credito)
442200100401     c                   if        D1Unita<> *blanks
442300100401     C                   Cat       'and'       :1Sq
442301151021      * felletti - usiamo unità su codice cliente per i filtri
442400151021     C****               Cat       'RcaUnita'  :1Sq
442401151021     C***                Cat       '='         :0Sq
442402151021     C                   Cat       'substr('   :1Sq
442403151021     C                   Cat       'RcaKsc'    :0Sq
442500151021     C                   Cat       ', 2 , 3)=' :0Sq
442600100401     C                   Cat       ''''        :0Sq
442700100401     C                   Cat       D1Unita     :0Sq
442800100401     C                   Cat       ''''        :0Sq
442900130910C2226C                   Else
443000130912  "  C                   if        GesuniR057='D'
443100130910  "  C                   Cat       'and'       :1Sq
443200151027  "  C**                 Cat       'RcaUnita'  :1Sq
443300151027brt  C                   Cat       'substr('   :1Sq
443301151027brt  C                   Cat       'RcaKsc'    :0Sq
443302151027brt  C                   Cat       ', 2 , 3)'  :0Sq
443303151027  "  C                   If        WinUni=' '
443400130910  "  C                   Cat       '='         :1Sq
443500130910  "  C                   Cat       ''''        :0Sq
443600130910  "  C                   Cat       ''''        :0Sq
443700130910  "  C                   Else
443800130910  "  C                   Cat       WinUni      :1Sq
443900130910  "  C                   Endif
444000130910C2226C                   Endif
444100130911     C                   endif
444200100401     c* capoconto
444300100407     c                   if        D1kcc  <> *blanks
444400100401     C                   Cat       'and'       :1Sq
444500100401     C                   Cat       'RcaKcc'    :1Sq
444600100401     C                   Cat       '='         :1Sq
444700100401     C                   Cat       ''''        :0Sq
444800100401     C                   Cat       D1kcc       :0Sq
444900100401     C                   Cat       ''''        :0Sq
445000130911C2226C                   else
445100130912  "  C                   If        $Pre = *on
445200130911  "  C                   Cat       'and'       :1Sq
445300130911  "  C                   Cat       'RcaKcc'    :1Sq
445400130911  "  C                   Cat       '='         :1Sq
445500130911  "  C                   Cat       ''''        :0Sq
445600130911  "  C                   Cat       H2kcc       :0Sq
445700130911  "  C                   Cat       ''''        :0Sq
445800130911C2226C                   endif
445900130911     C                   endif
446000100401     c*
446100100401     c* posizione attributo = a quella del parametro mod. solleciti
446200100401     C                   Cat       'and'       :1Sq
446300100401     C                   Cat       'RcaPosiz'  :1Sq
446400100401     C                   Cat       '='         :1Sq
446500100401     C                   Cat       ''''        :0Sq
446600100401     C                   Cat       PosAttr057  :0Sq
446700100401     C                   Cat       ''''        :0Sq
446800100401     c* CAMPI FACOLTATIVI
446900100401     c* attributo
447000100401     c                   if        D1attrib <> *blanks
447100100401     C                   Cat       'and'       :1Sq
447200100401     C                   Cat       'RcaAttrib' :1Sq
447300100401     C                   Cat       '='         :1Sq
447400100401     C                   Cat       ''''        :0Sq
447500100401     C                   Cat       d1Attrib    :0Sq
447600100401     C                   Cat       ''''        :0Sq
447700100401     c                   endif
447800100401     c* ksc
447900100401     c                   if        D1ksc <> *blanks
448000100401     C                   Cat       'and'       :1Sq
448100100401     C                   Cat       'Rcaksc'    :1Sq
448200100401     C                   Cat       '='         :1Sq
448300100401     C                   Cat       ''''        :0Sq
448400100401     C                   Cat       d1ksc       :0Sq
448500100401     C                   Cat       ''''        :0Sq
448600100401     c                   endif
448700100401     c* agente
448800100401     c                   if        D1kscAge <> *blanks
448900090409     C                   Cat       'and'       :1Sq
449000100401     C                   Cat       'RcakscAge' :1Sq
449100090409     C                   Cat       '='         :1Sq
449200100401     C                   Cat       ''''        :0Sq
449300100401     C                   Cat       d1kscage    :0Sq
449400100401     C                   Cat       ''''        :0Sq
449500090409     c                   endif
449600100512     c* parzializzazione fasi
449700140314     c                   select
449800140314     c                   when      D1FasiApe= ' '
449900140314     C                   Eval      Sq = %trim(sq) +
450000140314     C                             ' and RcaStato='' '''
450100140314     c                   when      D1FasiApe= '1'
450200140314     C                   Eval      Sq = %trim(sq) +
450300140314     C                             ' and RcaStato=''1'''
450400140314     c                   when      D1FasiApe= '2'
450500140314     C                   Eval      Sq = %trim(sq) +
450600140314     C                             ' and RcaStato=''2'''
450700140331     c                   when      D1FasiApe= '*'
450800140331     C                   Eval      Sq = %trim(sq) +
450900140331     C                             ' and RcaStato<>''2'''
451000140314     c                   endsl
451100140314      ***
451200140314     C***                if        d1fasiape <> *blanks
451300140314     C***                Cat       'and'       :1Sq
451400140314     C***                Cat       'RcaUteChiu':1Sq
451500140314     c**aperte
451600140314     c***                if        D1FasiApe=xscon
451700140314     C***                Cat       '='         :1Sq
451800140314     C***                Cat       ''''        :0Sq
451900140314     C***                Cat       ' '         :0Sq
452000140314     C***                Cat       ''''        :0Sq
452100140314     c***                else
452200140314     C***                Cat       '<>'        :1Sq
452300140314     C***                Cat       ''''        :0Sq
452400140314     C***                Cat       ' '         :0Sq
452500140314     C***                Cat       ''''        :0Sq
452600140314     c***                endif
452700140314     c***                endif
452800100413     c* parzializzazione partite
452900140314     C*                  Cat       'and'       :1Sq
453000140314     C*                  Cat       'RcaStato'  :1Sq
453100100413     c* aperte
453200140314     c*                  if        D1Stato=xscon
453300140314     C*                  Cat       '<>'        :1Sq
453400140314     C*                  Cat       ''''        :0Sq
453500140314     C*                  Cat       '2'         :0Sq
453600140314     C*                  Cat       ''''        :0Sq
453700140314     c*                  else
453800140314     C*                  Cat       '='         :1Sq
453900140314     C*                  Cat       ''''        :0Sq
454000140314     C*                  Cat       '2'         :0Sq
454100140314     C*                  Cat       ''''        :0Sq
454200140314     c*                  endif
454300140310      * Solo insoluti
454400140310     C**                 if        d1insoluti = xscon
454500140310     C**                 Eval      Sq = %trim(sq) +
454600140310     C**                           ' and substr(scdflg , 3 , 1)=''S'''
454700140310     C**                 endif
454800100407     c*
454900100420     c* parzializzazione data partita
455000100420     c                   exsr      ParzDtPar
455100100401     c* parzializzazione data scadenza
455200100401     c                   exsr      ParzDtSca
455300100416     c* parzializzazione data sollecito
455400100416     c                   exsr      ParzDtSol
455500100401     c* parzializzazione fase
455600100401     c                   if        D1FaseDa <> *blanks
455700100401     c                   move      D1FaseDa      FaseDa            1
455800100401     c                   move      D1FaseA       FaseA             1
455900100401     C                   Cat       'and'       :1Sq
456000100401     C                   Cat       'RcaFase'   :1Sq
456100100407     C                   Cat       'between'   :1Sq
456200100401     C                   Cat       FaseDa      :1Sq
456300100401     C                   Cat       'and'       :1Sq
456400100401     C                   Cat       FaseA       :1Sq
456500100401     c                   endif
456600100407     c* parzializzazione azione
456700100407     c                   if        D1Azione <> *blanks
456800100407     C                   Cat       'and'       :1Sq
456900100407     C                   Cat       'RcaAzione' :1Sq
457000100407     C                   Cat       '='         :1Sq
457100100407     C                   Cat       ''''        :0Sq
457200100407     C                   Cat       d1Azione    :0Sq
457300100407     C                   Cat       ''''        :0Sq
457400100407     c                   endif
457500131001BRT  C* esclusione impegni (98)
457600131001 "   c                   if        d1EsclImp = xscon
457700131001 "   C                   Cat       'and'       :1Sq
457800131001 "   C                   Cat       'RcaAzione' :1Sq
457900131001 "   C                   Cat       '<>'        :0Sq
458000131001 "   C                   Cat       '''98'''    :0Sq
458100131001BRT  C                   endif
458200131001BRT  C* parzializzazione fasi aperte BRT
458300140314     c                   select
458400140314     C                   when      d1fasiape = '1'
458500140314BRT  C                   exsr      ParzFasiBRT1
458600140314     C                   when      d1fasiape = '2'
458700140314brt  C                   exsr      ParzFasiBRT2
458800140314brt  c                   endsl
458900131129C2232C* parzializzazione campo libero
459000131129  "  C                   if        D1Libero <> *blanks
459100131129  "  C                   Cat       'and'       :1Sq
459200131129  "  C                   Cat       'RCALibero' :1Sq
459300131203  "  C                   Cat       'like ''%'  :1Sq
459400131205     C                   Eval      Sq = %trim(sq) +
459500131205     C                              %trim(%scanrpl('''':'''''':d1libero))
459600131205  "  C                   Cat       '%'''       :0Sq
459700131129C2232C                   endif
459800100401     c* parzializzazione destinatario
459900100401     c                   exsr      ParzDest
460000100401     c*
460100100401     c* parzializzazione data attività
460200100401     c                   exsr      ParzDtAtt
460300100401     C* ORDINAMENTO
460400100401     c                   cat       'Order by'  :1Sq
460500100401     c                   select
460600100401     c                   when      $ord = 1
460700100402     c                   cat       'RcaKsc,'   :1Sq
460800100402     c                   cat       'RcaDtPar'  :1Sq
460900100401     c                   when      $ord = 2
461000100402     c                   cat       'RcaDtPar,' :1Sq
461100100402     c                   cat       'RcaKsc'    :1Sq
461200100401     c                   when      $ord = 3
461300100402     c                   cat       'RcaKscAge,':1Sq
461400100401     c                   cat       'RcaKsc'    :1Sq
461500100401     c                   endsl
461600070525     C*
461700070525     C* Esecuzione SQL
461800070525     C                   Exsr      ExeSQL
461900070525     C*
462000070525     C                   ENDSR
462100070525     C/EJECT
462200130910C2226C************************************************************
462300130910  "  C* Composizione stringa sql di prelista
462400130910  "  C************************************************************
462500130910C2226C     InzSqlPre     BEGSR
462600130918     C* Selezione
462700130919     C                   EVAL      SQ='Select +
462800151022     C****                             RcaSocieta,RcaUnita,RcaKcc,SogCdFisc,+
462801151022     C                                 RcaSocieta,+
462802151022     C                                 substr(RcaKsc , 2 , 3),+
462805151022     C                                 RcaKcc,SogCdFisc,+
462806151022     C                                 SogPartIva,SogSogg,Min(SogDes)  +
463000140310     C                                 from NDRCA00F'
463100140310      *
463200140310     C                   if        d1insoluti = xscon
463300140310     C                   Eval      SQ=%trimr(SQ) + ' inner join ndscd00f on +
463400140310     C                             scdsocieta=rcasocieta and +
463500140310     C                             scdctb = ''CG'' and scdtpscad=''0'' +
463600140310     C                             and scdkcc=rcakcc and +
463700140310     C                             scdksc=rcaksc and +
463800140310     C                             scddtpar=rcadtpar and +
463900140310     C                             scdnrpar=rcanrpar and +
464000140310     C                             scdserpar=rcaserpar and +
464100140310     C                             scddtsca=rcadtsca '
464200140310     C                   endif
464201140530      *
464203140530     C                   if        d1ABC <> *blanks
464204140530     C                   Eval      SQ=%trimr(SQ) + ' inner join ancln00f on +
464205140530     C                             clnsocieta=rcasocieta and +
464206140530     C                             clnksc=rcaksc and +
464207140530     C                             clnclasabc = ''' + d1ABC + ''' and +
464209140530     C                             clnlineav='' '' and clnfiliale = 0 '
464210141204     C                   endif
464214140530     C*
464400140310     C                   EVAL      SQ=%trimr(SQ) + ' ,ANRCO00F,ANSOG00F'
464500140310      *
464600130912     C                   Cat       'Where'     :1Sq
464700130912     C* CAMPI OBBLIGATORI
464800130912     C* Societa
464900130918     C                   Cat       'RcaSocieta':1Sq
465000130912     C                   Cat       '='         :1Sq
465100130912     C                   Cat       ''''        :0Sq
465200130912     C                   Cat       xscsoc      :0Sq
465300130912     C                   Cat       ''''        :0Sq
465400130912     C* unita (se gestita nella attività di recupero credito)
465500130912     C                   if        D1Unita<> *blanks
465600130918     C                   Cat       'and'       :1Sq
465700151021     C***                Cat       'RcaUnita'  :1Sq
465800151021     C***                Cat       '='         :1Sq
465801151021     C                   Cat       'substr('   :1Sq
465802151021     C                   Cat       'RcaKsc'    :0Sq
465803151021     C                   Cat       ', 2 , 3)=' :0Sq
465900130918     C                   Cat       ''''        :0Sq
466000130918     C                   Cat       D1Unita     :0Sq
466100130918     C                   Cat       ''''        :0Sq
466200130910     C                   Else
466300130918     C* unita se gestita ma vuota, include tutte le unità
466400130910     C* abilitate per il profilo.
466500130910     C                   if        GesuniR057='D'
466600130910     C                   Cat       'and'       :1Sq
466700151027     C**                 Cat       'RcaUnita'  :1Sq
466701151027     C                   Cat       'substr('   :1Sq
466702151027     C                   Cat       'RcaKsc'    :0Sq
466703151027     C                   Cat       ', 2 , 3)'  :0Sq
466704151027     C                   If        WinUni=' '
466900130910     C                   Cat       '='         :1Sq
467000130910     C                   Cat       ''''        :0Sq
467100130910     C                   Cat       ''''        :0Sq
467200130910     C                   Else
467300130910     C                   Cat       WinUni      :1Sq
467400130910     C                   Endif
467500130910     C                   Endif
467600130912     C                   Endif
467700130912     C* capoconto
467800130912     C                   if        D1kcc  <> *blanks
467900130912     C                   Cat       'and'       :1Sq
468000130918     C                   Cat       'RcaKcc'    :1Sq
468100130912     C                   Cat       '='         :1Sq
468200130912     C                   Cat       ''''        :0Sq
468300130912     C                   Cat       D1kcc       :0Sq
468400130912     C                   Cat       ''''        :0Sq
468500130912     C                   endif
468600130918     c* posizione attributo = a quella del parametro mod. solleciti
468700130918     C                   Cat       'and'       :1Sq
468800130918     C                   Cat       'RcaPosiz'  :1Sq
468900130918     C                   Cat       '='         :1Sq
469000130918     C                   Cat       ''''        :0Sq
469100130918     C                   Cat       PosAttr057  :0Sq
469200130918     C                   Cat       ''''        :0Sq
469300130912     C*
469400130912     C* join con i file anagrafici(anrco00f/ansog00f)
469500130912     C* per reperire la partita Iva
469600130910     C                   Cat       'and'       :1Sq
469700130918     C                   Cat       'RcaSocieta':1Sq
469800130910     C                   Cat       '='         :1Sq
469900130910     C                   Cat       'RcoSocieta':0Sq
470000130910     C*
470100130910     C                   Cat       'and'       :1Sq
470200130918     C                   Cat       'RcaKcc'    :1Sq
470300130910     C                   Cat       '='         :1Sq
470400130910     C                   Cat       'RcoKcc'    :0Sq
470500130910     C*
470600130910     C                   Cat       'and'       :1Sq
470700130918     C                   Cat       'RcaKsc'    :1Sq
470800130910     C                   Cat       '='         :1Sq
470900130918     C                   Cat       'RcoKsc'    :1Sq
471000130910     C*
471100130910     C                   Cat       'and'       :1Sq
471200130910     C                   Cat       'RcoSogg'   :1Sq
471300130910     C                   Cat       '='         :1Sq
471400130910     C                   Cat       'SogSogg'   :1Sq
471500130918     C* CAMPI FACOLTATIVI
471600130918     C* attributo
471700130918     C                   if        D1attrib <> *blanks
471800130918     C                   Cat       'and'       :1Sq
471900130918     C                   Cat       'RcaAttrib' :1Sq
472000130918     C                   Cat       '='         :1Sq
472100130918     C                   Cat       ''''        :0Sq
472200130918     C                   Cat       d1Attrib    :0Sq
472300130918     C                   Cat       ''''        :0Sq
472400130918     C                   endif
472500130918     C* ksc
472600130918     C                   if        D1ksc <> *blanks
472700130918     C                   Cat       'and'       :1Sq
472800130918     C                   Cat       'Rcaksc'    :1Sq
472900130918     C                   Cat       '='         :1Sq
473000130918     C                   Cat       ''''        :0Sq
473100130918     C                   Cat       d1ksc       :0Sq
473200130918     C                   Cat       ''''        :0Sq
473300130918     C                   endif
473400130918     C* agente
473500130918     C                   if        D1kscAge <> *blanks
473600130918     C                   Cat       'and'       :1Sq
473700130918     C                   Cat       'RcakscAge' :1Sq
473800130918     C                   Cat       '='         :1Sq
473900130918     C                   Cat       ''''        :0Sq
474000130918     C                   Cat       d1kscage    :0Sq
474100130918     C                   Cat       ''''        :0Sq
474200130918     C                   endif
474300130918     C* parzializzazione fasi
474400140314     c                   select
474500140314     c                   when      D1FasiApe= ' '
474600140314     C                   Eval      Sq = %trim(sq) +
474700140314     C                             ' and RcaStato='' '''
474800140314     c                   when      D1FasiApe= '1'
474900140314     C                   Eval      Sq = %trim(sq) +
475000140314     C                             ' and RcaStato=''1'''
475100140314     c                   when      D1FasiApe= '2'
475200140314     C                   Eval      Sq = %trim(sq) +
475300140314     C                             ' and RcaStato=''2'''
475400140331     c                   when      D1FasiApe= '*'
475500140331     C                   Eval      Sq = %trim(sq) +
475600140331     C                             ' and RcaStato<>''2'''
475700140314     c                   endsl
475800140428      *
475900140513BRT  c*
475901140513  "  c                   select
476000140513  "  c                   when      D1STACREDA <> *blanks
476001140513  "  c                             and D1STACREA = *blanks
476100140428  "  C                   Eval      Sq = %trim(sq)  +
476200140521  "  C                             ' and rcoctgan03=''' + d1STACREDA + ''''
476302140513  "  c                   when      D1STACREDA <> *blanks
476303140513  "  c                             and D1STACREA <> *blanks
476304140513  "  C                   Eval      Sq = %trim(sq)  +
476305140513  "  C                             ' and rcoctgan03 between ''' +
476306140513  "  C                             d1STACREDA + ''' and ''' +
476308140513  "  C                             d1STACREA + ''''
476311140513  "  c                   when      D1STACREDA = *blanks
476312140513  "  c                             and D1STACREA <> *blanks
476313140513  "  C                   Eval      Sq = %trim(sq)  +
476314140521  "  C                             ' and rcoctgan03<=''' + d1STACREA + ''''
476315140513BRT  C                   endsl
476400140513      *
476700140314     c**                 if        D1FasiApe=XSCON
476800140314     C**                 Cat       'and'       :1Sq
476900140314     C***                Cat       'RcaUteChiu':1Sq
477000140314     C**                 Cat       'RcaStato':1  Sq
477100140314      * aperte
477200140314     c**                 if        D1FasiApe=xscon
477300140314     C**                 Cat       '='         :1Sq
477400140314     C**                 Cat       ''''        :0Sq
477500140314     C**                 Cat       ' '         :0Sq
477600140314     C**                 Cat       ''''        :0Sq
477700140314     c**                 else
477800140314     C**                 Cat       '<>'        :1Sq
477900140314     C**                 Cat       ''''        :0Sq
478000140314     C**                 Cat       ' '         :0Sq
478100140314     C**                 Cat       ''''        :0Sq
478200140314     C**                 endif
478300140314     C**                 endif
478400130918     C* parzializzazione partite
478500140314     C**                 Cat       'and'       :1Sq
478600140314     C**                 Cat       'RcaStato'  :1Sq
478700140314     C**aperte
478800140314     C**                 select
478900140314     C**                 when      D1Stato=xscon
479000140314     C**                 Cat       '<>'        :1Sq
479100140314     C**                 Cat       ''''        :0Sq
479200140314     C**                 Cat       '2'         :0Sq
479300140314     C**                 Cat       ''''        :0Sq
479400140314     C**                 when      D1Stato=xscoff
479500140314     C**                 Cat       '='         :1Sq
479600140314     C**                 Cat       ''''        :0Sq
479700140314     C**                 Cat       '2'         :0Sq
479800140314     C**                 Cat       ''''        :0Sq
479900140314     C**                 endsl
480000140310      * Solo insoluti
480100140310     C                   if        d1insoluti = xscon
480200140310     C                   Eval      Sq = %trim(sq) +
480300140310     C                             ' and substr(scdflg , 3 , 1)=''S'''
480400140310     C                   endif
480500131129C2232c* parzializzazione campo libero
480600131129  "  c                   if        D1Libero <> *blanks
480700131129  "  C                   Cat       'and'       :1Sq
480800131129  "  C                   Cat       'RCALibero' :1Sq
480900131203  "  C                   Cat       'like ''%'  :1Sq
481000131205     C                   Eval      Sq = %trim(sq) +
481100131205     C                             %trim(%scanrpl('''':'''''':d1libero))
481200131203  "  C                   Cat       '%'''       :0Sq
481300131129C2232C                   endif
481400130918     C* parzializzazione data partita
481500130918     C                   exsr      ParzDtPar
481600130918     C* parzializzazione data scadenza
481700130918     C                   exsr      ParzDtSca
481800130918     C* parzializzazione data sollecito
481900130918     C                   exsr      ParzDtSol
482000130918     C* parzializzazione fase
482100130918     C                   if        D1FaseDa <> *blanks
482200130918     C                   move      D1FaseDa      FaseDa            1
482300130918     C                   move      D1FaseA       FaseA             1
482400130918     C                   Cat       'and'       :1Sq
482500130918     C                   Cat       'RcaFase'   :1Sq
482600130918     C                   Cat       'between'   :1Sq
482700130918     C                   Cat       FaseDa      :1Sq
482800130918     C                   Cat       'and'       :1Sq
482900130918     C                   Cat       FaseA       :1Sq
483000130918     C                   endif
483100130918     C* parzializzazione azione
483200130918     c                   if        D1Azione <> *blanks
483300130918     C                   Cat       'and'       :1Sq
483400130918     C                   Cat       'RcaAzione' :1Sq
483500130918     C                   Cat       '='         :1Sq
483600130918     C                   Cat       ''''        :0Sq
483700130918     C                   Cat       d1Azione    :0Sq
483800130918     C                   Cat       ''''        :0Sq
483900130918     C                   endif
484000131001BRT  C* esclusione impegni (98)
484100131001 "   c                   if        d1EsclImp = xscon
484200131001 "   C                   Cat       'and'       :1Sq
484300131001 "   C                   Cat       'RcaAzione' :1Sq
484400131001 "   C                   Cat       '<>'        :0Sq
484500131001 "   C                   Cat       '''98'''    :0Sq
484600131001BRT  C                   endif
484700131001BRT  C* parzializzazione fasi aperte BRT
484800140314     c                   select
484900140314     C                   when      d1fasiape = '1'
485000140314BRT  C                   exsr      ParzFasiBRT1
485100140314     C                   when      d1fasiape = '2'
485200140314brt  C                   exsr      ParzFasiBRT2
485300140314brt  c                   endsl
485400130918     C* parzializzazione destinatario
485500130918     C                   exsr      ParzDest
485600130918     C*
485700130918     C* parzializzazione data attività
485800130918     C                   exsr      ParzDtAtt
485900140318     C* parzializzazione data chiusura
486000140318     C                   exsr      ParzDtChiu
486100130919     C* Raggruppamento
486200130919     C                   cat       'Group by'  :1Sq
486300130919     C                   cat       'RcaSocieta':1Sq
486400130919     C                   cat       ','         :0Sq
486500151021     C***                cat       'RcaUnita'  :0Sq
486501151021     C                   Cat       'substr('   :1Sq
486502151021     C                   Cat       'RcaKsc'    :0Sq
486503151021     C                   Cat       ', 2 , 3)'  :0Sq
486600130919     C                   cat       ','         :0Sq
486700130919     C                   cat       'RcaKcc'    :0Sq
486800130919     C                   cat       ','         :0Sq
486900130919     C                   cat       'SogCdFisc' :0Sq
487000130919     C                   cat       ','         :0Sq
487100130919     C                   cat       'SogPartIva':0Sq
487200130919     C                   cat       ','         :0Sq
487300130919     C                   cat       'SogSogg'   :0Sq
487400130919     C* ordinamento
487500130919     C                   cat       'Order by'  :1Sq
487600130919     C                   if        $RgrPre = 'P'
487700130919     C                   cat       'RcaKcc'    :1Sq
487800130919     C                   cat       ','         :0Sq
487900130919     C                   cat       'SogCdFisc' :0Sq
488000130919     C                   cat       ','         :0Sq
488100130919     C                   cat       'SogPartIva':0Sq
488200130919     C                   Else
488300130919     C                   cat       'RCaKcc'    :1Sq
488400130919     C                   cat       ','         :0Sq
488500130919     C                   cat       'SogSogg'   :0Sq
488600130919     C                   Endif
488700130912     C*
488800130910     C* Esecuzione SQL
488900130910     C                   Exsr      ExeSQLPre
489000130918C2226C*
489100130918  "  C                   ENDSR
489200130918C2226C/EJECT
489300130919C2226C************************************************************
489400130919  "  C* Composizione stringa sql di tabella in qtemp
489500130919  "  C************************************************************
489600130919C2226C     InzSqltab     BEGSR
489700130919     C* Selezione
489800131022     C                   EVAL      SQ='Select * from Qtemp/NDrcaTemp'
489900131022      *
490000131022     C                   if        d1impda > 0 or
490100131022     C                             d1impa > 0
490200131022     C                   if        d1impa = 0
490300131022     C                   eval      d1impa=*all'9'
490400131022     C                   ENDIF
490500131022      *
490600131022     C                   eval      SQ=%trim(SQ) +
490700131022     C                             ' where imppre between ' +
490800131022     C                             %char(d1impda) +
490900131022     C                             ' and ' + %char(d1impa)
491000131022     C                   ENDIF
491100131022     C
491200130919     C* ordinamento
491300130919     C                   cat       'Order by'  :1Sq
491400130919     C                   cat       'ImpPre'    :1Sq
491500130919     C                   cat       'Desc'      :1Sq
491600130919     C*
491700130919     C* Esecuzione SQL
491800130919     C                   Exsr      ExeSQLTab
491900130919C2226C*
492000130919  "  C                   ENDSR
492100130919C2226C/EJECT
492200131001BRT  C************************************************************
492300140314 "   C* SubSelect scadenze chiuse
492400131001 "   C************************************************************
492500140314 "   C     ParzFasiBRT2  BEGSR
492600131001 "    /free
492700140314 "          if d1FasiApe = '2';
492800131001 "         eval sq = %trim(sq) +
492900131001 "       ' and not exists +
493000131001 "       (select rcaksc from ndrca00f as b  +
493100140314 "       //  where b.rcautechiu ='' '' +
493200140314 "       where b.rcastato <> ''2'' +
493300131001 "       and ndrca00f.rcasocieta = b.rcasocieta +
493400131001 "       and ndrca00f.rcaposiz = b.rcaposiz +
493500131001 "       and ndrca00f.rcaunita = b.rcaunita +
493600131001 "       and ndrca00f.rcakcc = b.rcakcc +
493700131001 "       and ndrca00f.rcaksc = b.rcaksc +
493800131001 "       and ndrca00f.rcadtsca = b.rcadtsca +
493900131001 "       and ndrca00f.rcadtpar = b.rcadtpar +
494000131001 "       and ndrca00f.rcanrpar = b.rcanrpar +
494100131001 "       and ndrca00f.rcaserpar = b.rcaserpar) ';
494200131001 "       endif;
494300131001 "    /end-free
494400131001BRT  C                   ENDSR
494500140314BRT  C************************************************************
494600140314 "   C* SubSelect fasi chiuse manualmente
494700140314 "   C************************************************************
494800140314 "   C     ParzFasiBRT1  BEGSR
494900140314 "    /free
495000140314 "          if d1FasiApe = '1';
495100140314 "         eval sq = %trim(sq) +
495200140314 "       ' and not exists +
495300140314 "       (select rcaksc from ndrca00f as b  +
495400140314 "       //  where b.rcautechiu ='' '' +
495500140314 "       where b.rcastato = '' '' +
495600140314 "       and ndrca00f.rcasocieta = b.rcasocieta +
495700140314 "       and ndrca00f.rcaposiz = b.rcaposiz +
495800140314 "       and ndrca00f.rcaunita = b.rcaunita +
495900140314 "       and ndrca00f.rcakcc = b.rcakcc +
496000140314 "       and ndrca00f.rcaksc = b.rcaksc +
496100140314 "       and ndrca00f.rcadtsca = b.rcadtsca +
496200140314 "       and ndrca00f.rcadtpar = b.rcadtpar +
496300140314 "       and ndrca00f.rcanrpar = b.rcanrpar +
496400140314 "       and ndrca00f.rcaserpar = b.rcaserpar) ';
496500140314 "       endif;
496600140314 "    /end-free
496700140314BRT  C                   ENDSR
496800100420     C******************************************************
496900100420     C* Parzializzazioni data partita
497000100420     C******************************************************
497100100420     C     ParzDtPar     BEGSR
497200100420     c*
497300131001BRT  c                   if        D1DtParDa <> 0
497400131001BRT  c                             or D1DtParA <> 0
497500100420     c                   move      WdtParDa      DtPar10DA        10
497600100420     c                   move      WdtParA       DtPar10A         10
497700100420     C                   Cat       'and'       :1Sq
497800100420     C                   Cat       '(RcaDtPar' :1Sq
497900100420     C                   Cat       '>='        :1Sq
498000100420     C                   Cat       '''':0        Sq
498100100420     C                   Cat       DtPar10DA   :0Sq
498200100420     C                   Cat       '''':0        Sq
498300100420     C                   Cat       'and'       :1Sq
498400100420     C                   Cat       'RcaDtPar'  :1Sq
498500100420     C                   Cat       '<='        :1Sq
498600100420     C                   Cat       '''':0        Sq
498700100420     C                   Cat       DtPar10A    :0Sq
498800100420     C                   Cat       '''':0        Sq
498900100420     C                   Cat       ')'         :1Sq
499000131001BRT  C                   Endif
499100100420     C*
499200100420     C                   ENDSR
499300090325     C******************************************************
499400100401     C* Parzializzazioni data scadenza
499500090325     C******************************************************
499600100401     C     ParzDtSca     BEGSR
499700090325     c*
499800131001BRT  c                   if        D1DtScaDa <> 0
499900131001BRT  c                             or D1DtScaA <> 0
500000100401     c                   move      WdtScaDa      DtSca10DA        10
500100100401     c                   move      WdtScaA       DtSca10A         10
500200090325     C                   Cat       'and'       :1Sq
500300100401     C                   Cat       '(RcaDtSca' :1Sq
500400090325     C                   Cat       '>='        :1Sq
500500090325     C                   Cat       '''':0        Sq
500600100401     C                   Cat       DtSca10DA   :0Sq
500700090325     C                   Cat       '''':0        Sq
500800090325     C                   Cat       'and'       :1Sq
500900100401     C                   Cat       'RcaDtSca'  :1Sq
501000090325     C                   Cat       '<='        :1Sq
501100090325     C                   Cat       '''':0        Sq
501200100401     C                   Cat       DtSca10A    :0Sq
501300090325     C                   Cat       '''':0        Sq
501301141205     C                   Cat       'and'       :1Sq
501302141205     C                   Cat       'RcaImporto':1Sq
501303141205     C                   Cat       '>0'        :0Sq
501304141205     C                   Cat       ')':0         Sq
501500131001BRT  C                   Endif
501600090325     C*
501700090325     C                   ENDSR
501800100416     C******************************************************
501900100416     C* Parzializzazioni data sollecito
502000100416     C******************************************************
502100100416     C     ParzDtSol     BEGSR
502200100416     c*
502300131001BRT  c                   if        D1DtSolDa <> 0
502400131001BRT  c                             or D1DtSolA <> 0
502500100416     c                   move      WdtSolDa      DtSol10DA        10
502600100416     c                   move      WdtSolA       DtSol10A         10
502700100416     C                   Cat       'and'       :1Sq
502800100416     C                   Cat       '(RcaDtLet' :1Sq
502900100416     C                   Cat       '>='        :1Sq
503000100416     C                   Cat       '''':0        Sq
503100100416     C                   Cat       DtSol10DA   :0Sq
503200100416     C                   Cat       '''':0        Sq
503300100416     C                   Cat       'and'       :1Sq
503400100416     C                   Cat       'RcaDtLet'  :1Sq
503500100416     C                   Cat       '<='        :1Sq
503600100416     C                   Cat       '''':0        Sq
503700100416     C                   Cat       DtSol10A    :0Sq
503800100416     C                   Cat       '''':0        Sq
503900100416     C                   Cat       ')'         :1Sq
504000131001BRT  C                   Endif
504100100416     C*
504200100416     C                   ENDSR
504300100401     C******************************************************
504400100401     C* Parzializzazioni destinatario
504500100401     C******************************************************
504600100401     C     ParzDest      BEGSR
504700100401     c*
504800100401     c                   if        D1Destin <> *blanks
504900100401     C                   Cat       'and'       :1Sq
505000100401     C                   Cat       'RcaGrute'  :1Sq
505100100402     C                   Cat       'in('       :1Sq
505200100401     C*
505300100401     C*   leggo tutta la tabella 048 per caricare i gruppi del
505400100401     c*   destinatario
505500100401     C*
505600100401     C                   MOVE      '2'           XTBRIC
505700100401     C                   MOVE      XSCSOC        XTBAZI
505800100401     C                   MOVE      '048'         XTBCOD
505900100401     C                   MOVE      *BLANKS       XTBKEY
506000100401     C                   CALLb     'XATB'
506100100401     C                   PARM                    XATBDS
506200100401     C     XTBERR        DOWEQ     *OFF
506300100401     C                   MOVEL     XTBUNI        ang048ds
506400100401     c                   move      xtbkey        Gruppo            3
506500100401     c                   select
506600100401     c                   when      d1Destin=usr1048
506700100401     C                   Cat       ''''        :0Sq
506800100402     c                   cat       Gruppo      :0Sq
506900100401     C                   Cat       ''''        :0Sq
507000100402     C                   Cat       ','         :1Sq
507100100401     c                   when      d1Destin=usr2048
507200100401     C                   Cat       ''''        :0Sq
507300100402     c                   cat       Gruppo      :0Sq
507400100401     C                   Cat       ''''        :0Sq
507500100402     C                   Cat       ','         :1Sq
507600100401     c                   when      d1Destin=usr3048
507700100401     C                   Cat       ''''        :0Sq
507800100402     c                   cat       Gruppo      :0Sq
507900100401     C                   Cat       ''''        :0Sq
508000100402     C                   Cat       ','         :1Sq
508100100401     c                   when      d1Destin=usr4048
508200100401     C                   Cat       ''''        :0Sq
508300100402     c                   cat       Gruppo      :0Sq
508400100401     C                   Cat       ''''        :0Sq
508500100402     C                   Cat       ','         :1Sq
508600100401     c                   when      d1Destin=usr5048
508700100401     C                   Cat       ''''        :0Sq
508800100402     c                   cat       Gruppo      :0Sq
508900100401     C                   Cat       ''''        :0Sq
509000100402     C                   Cat       ','         :1Sq
509100100401     c                   when      d1Destin=usr6048
509200100401     C                   Cat       ''''        :0Sq
509300100402     c                   cat       Gruppo      :0Sq
509400100401     C                   Cat       ''''        :0Sq
509500100402     C                   Cat       ','         :1Sq
509600100401     c                   when      d1Destin=usr7048
509700100401     C                   Cat       ''''        :0Sq
509800100402     c                   cat       Gruppo      :0Sq
509900100401     C                   Cat       ''''        :0Sq
510000100402     C                   Cat       ','         :1Sq
510100100401     c                   when      d1Destin=usr8048
510200100401     C                   Cat       ''''        :0Sq
510300100402     c                   cat       Gruppo      :0Sq
510400100401     C                   Cat       ''''        :0Sq
510500100402     C                   Cat       ','         :1Sq
510600100401     c                   endsl
510700100401     C*
510800100401     C                   MOVE      '3'           XTBRIC
510900100401     C                   MOVE      XSCSOC        XTBAZI
511000100401     C                   MOVE      *ZERO         XTBKEY
511100100401     C                   MOVE      '048'         XTBCOD
511200100401     C                   CALLb     'XATB'
511300100401     C                   PARM                    XATBDS
511400100401     C*
511500100401     C                   ENDDO                                                  *
511600100401     c* chiudo stringa sql
511700100402     C                   Cat       ''''        :1Sq
511800100402     C                   Cat       ' '         :1Sq
511900100402     C                   Cat       ''''        :1Sq
512000100402     C                   Cat       ')'         :1Sq
512100100401     c
512200100401     c                   endif
512300100401     C*
512400100401     C                   ENDSR
512500100401     C******************************************************
512600100401     C* Parzializzazioni data attività
512700100401     C******************************************************
512800100401     C     ParzDtAtt     BEGSR
512900100401     c*
513000100401     c                   move      WdtAttDa      DtAtt10DA        10
513100100401     c                   move      WdtAttA       DtAtt10A         10
513200100401     C                   Cat       'and'       :1Sq
513300100401     C                   Cat       '(RcaDtAtt' :1Sq
513400100401     C                   Cat       '>='        :1Sq
513500100401     C                   Cat       '''':0        Sq
513600100401     C                   Cat       DtAtt10DA   :0Sq
513700100401     C                   Cat       '''':0        Sq
513800100401     C                   Cat       'and'       :1Sq
513900100401     C                   Cat       'RcaDtAtt'  :1Sq
514000100401     C                   Cat       '<='        :1Sq
514100100401     C                   Cat       '''':0        Sq
514200100401     C                   Cat       DtAtt10A    :0Sq
514300100401     C                   Cat       '''':0        Sq
514400100401     C                   Cat       ')'         :1Sq
514500100401     C*
514600100401     C                   ENDSR
514700140318     C******************************************************
514800140318     C* Parzializzazioni data chiusura
514900140318     C******************************************************
515000140318     C     ParzDtChiu    BEGSR
515100140318     c*
515200140318     c                   move      WdtChiDa      DtChi10DA        10
515300140318     c                   move      WdtChiA       DtChi10A         10
515400140318     C                   Cat       'and'       :1Sq
515500140319     C                   Cat       '(RcaDtChiu':1Sq
515600140318     C                   Cat       '>='        :1Sq
515700140318     C                   Cat       '''':0        Sq
515800140318     C                   Cat       DtChi10DA   :0Sq
515900140318     C                   Cat       '''':0        Sq
516000140318     C                   Cat       'and'       :1Sq
516100140319     C                   Cat       'RcaDtChiu' :1Sq
516200140318     C                   Cat       '<='        :1Sq
516300140318     C                   Cat       '''':0        Sq
516400140318     C                   Cat       DtChi10A    :0Sq
516500140318     C                   Cat       '''':0        Sq
516600140318     C                   Cat       ')'         :1Sq
516700140318     C*
516800140318     C                   ENDSR
516900070525     C******************************************************
517000070525     C* ESECUZIONE SQL
517100070525     C******************************************************
517200070525     C     EXESQL        BEGSR
517300070525     C*
517400070525     C                   EXSR      CLOCUR
517500070525     C*
517600070525     C/EXEC SQL
517700070525     C+     PREPARE S1 FROM :SQ
517800070525     C/END-EXEC
517900070525     C*
518000070525     C/EXEC SQL
518100070525     C+     DECLARE A1 CURSOR FOR S1
518200070525     C/END-EXEC
518300070525     C*
518400070525     C/EXEC SQL
518500070525     C+     OPEN A1
518600070525     C/END-EXEC
518700070525     c*
518800070525     C                   MOVE      *ON           $OPEN             1
518900070525     c*
519000070525     C                   ENDSR
519100070525     C*
519200070525     C************************************************************
519300070525     C* CHIUSURA CURSORE
519400070525     C************************************************************
519500070525     C     CLOCUR        BEGSR
519600070525     C*
519700070525     C/EXEC SQL
519800070525     C+     CLOSE A1
519900070525     C/END-EXEC
520000070525     C*
520100070525     C                   MOVE      *OFF          $OPEN
520200070525     C*
520300070525     C                   ENDSR
520400070525     C/EJECT
520500070525     C************************************************************
520600070525     C* fetch
520700070525     C************************************************************
520800070525     C     FetchSql      BEGSR
520900070525     C*
521000070525     C/EXEC SQL
521100100402     C+     FETCH A1 INTO :DsFetch
521200070525     C/END-EXEC
521300070525     c*
521400070525     C                   ENDSR
521500130910C2226C******************************************************
521600130910  "  C* ESECUZIONE SQL
521700130910  "  C******************************************************
521800130910C2226C     EXESQLPRE     BEGSR
521900130910     C*
522000130910     C                   EXSR      CLOCURPRE
522100130910     C*
522200130910     C/EXEC SQL
522300130910     C+     PREPARE S2 FROM :SQ
522400130910     C/END-EXEC
522500130910     C*
522600130910     C/EXEC SQL
522700130910     C+     DECLARE A2 CURSOR FOR S2
522800130910     C/END-EXEC
522900130910     C*
523000130910     C/EXEC SQL
523100130910     C+     OPEN A2
523200130910     C/END-EXEC
523300130910     c*
523400130910     C                   MOVE      *ON           $OPENPre          1
523500130910     c*
523600130910C2226C                   ENDSR
523700130910  "  C*
523800130910  "  C************************************************************
523900130910  "  C* CHIUSURA CURSORE
524000130910  "  C************************************************************
524100130910C2226C     CLOCURPRE     BEGSR
524200130910     C*
524300130910     C/EXEC SQL
524400130910     C+     CLOSE A2
524500130910     C/END-EXEC
524600130910     C*
524700130910     C                   MOVE      *OFF          $OPENPre
524800130910     C*
524900130910C2226C                   ENDSR
525000130910  "  C/EJECT
525100130910  "  C************************************************************
525200130910  "  C* fetch
525300130910  "  C************************************************************
525400130910C2226C     FetchSqlPre   BEGSR
525500130910     C*
525600130910     C/EXEC SQL
525700130910     C+     FETCH A2 INTO :DsFetchPre
525800130910     C/END-EXEC
525900130910     c*
526000130912C2226C                   ENDSR
526100130919C2226C******************************************************
526200130919  "  C* ESECUZIONE SQL
526300130919  "  C******************************************************
526400130919C2226C     EXESQLTAB     BEGSR
526500130919     C*
526600130919     C                   EXSR      CLOCURTAB
526700130919     C*
526800130919     C/EXEC SQL
526900130919     C+     PREPARE S3 FROM :SQ
527000130919     C/END-EXEC
527100130919     C*
527200130919     C/EXEC SQL
527300130919     C+     DECLARE A3 CURSOR FOR S3
527400130919     C/END-EXEC
527500130919     C*
527600130919     C/EXEC SQL
527700130919     C+     OPEN A3
527800130919     C/END-EXEC
527900130919     c*
528000130919     C                   MOVE      *ON           $OPENTab          1
528100130919     c*
528200130919C2226C                   ENDSR
528300130919  "  C*
528400130919  "  C************************************************************
528500130919  "  C* CHIUSURA CURSORE
528600130919  "  C************************************************************
528700130919C2226C     CLOCURTAB     BEGSR
528800130919     C*
528900130919     C/EXEC SQL
529000130919     C+     CLOSE A3
529100130919     C/END-EXEC
529200130919     C*
529300130919     C                   MOVE      *OFF          $OPENTAB
529400130919     C*
529500130919C2226C                   ENDSR
529600130919  "  C/EJECT
529700130919  "  C************************************************************
529800130919  "  C* fetch
529900130919  "  C************************************************************
530000130919C2226C     FetchSqlTAB   BEGSR
530100130919     C*
530200130919     C/EXEC SQL
530300130919     C+     FETCH A3 INTO :DsFetchTab
530400130919     C/END-EXEC
530500130919     c*
530600130919C2226C                   ENDSR
530700070525     C*
530800941209     C************************************************************
530900941209     C* REPERIMENTO DATI SOCIETA'
531000941209     C************************************************************
531100941209     C     REPSOC        BEGSR
531200941209     C*
531300950522     C                   CALLB     'XSOC'
531400941209     C                   PARM                    TIPXSC            6
531500941209     C                   PARM                    SOCXSC            3
531600941209     C                   PARM                    CDSXSC            9 0
531700941209     C                   PARM                    MODXSC            3
531800941209     C                   PARM      *BLANKS       RTNXSC            1
531900941209     C                   PARM                    XSOCDS
532000941209     C                   PARM                    KPJBA
532100941209     C*
532200941209     C                   ENDSR
532300940223     C/EJECT
532400090325     C************************************************************
532500090325     C* REPERIMENTO AUTORIZZAZIONI UTENTE
532600090325     C************************************************************
532700090325     C     REPAUT        BEGSR
532800090325     C*
532900090325     C                   CLEAR                   AUTDS
533000090325     C*
533100090325     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
533200090325     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
533300090325     C                   Z-ADD     0             XCAGRP                         GRUPPO
533400090325     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
533500100409    >C                   MOVEL     'CONREC'      XCAFNC                         FUNZIONE
533600090325    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
533700090325     c                   MOVE      'REC'         XCAVFU
533800090511    >C                   MOVE      'CC'          XCATCK                         TIPO CTRL
533900090325    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
534000090325     C*
534100090325     C                   CALLB     'XCHKAUT'
534200090325     C                   PARM                    AUTDS
534300090325     C*
534400090325     C* errori/non abilitazione
534500090325     C     XCARTN        IFNE      0
534600090325    >C                   MOVE      '2'           ErrL76
534700090325     C                   EXSR      ENDPGM
534800090325     C                   ELSE
534900100409    >C                   MOVEL     XCADDS        CONRECDS
535000090325     C* Se non autorizzati alla funzione oppure                              ed esco
535100100409    >C     visATT        IFNE      *ON
535200090325     C                   MOVE      XCAFNC        XNOFNC
535300090325     C                   MOVE      XCAPRF        XNOPRF
535400090325     C                   MOVE      XCACUT        XNOCUT
535500090325     C                   MOVE      XCATCK        XNOTCK
535600090325     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
535700090325     C                   CALL      'XNOAUTR'
535800090325     C                   PARM                    XNOADS
535900090325     C                   MOVE      '2'           ErrL76
536000090325     C                   MOVE      *ON           $fine
536100090325     C                   EXSR      ENDPGM
536200090325     C                   ENDIF
536300090325     C                   ENDIF
536400090325     C*
536500090325     C                   ENDSR
536600090325     C/EJECT
536700940127     C************************************************************
536800940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
536900940127     C************************************************************
537000940131     C     REPDFT        BEGSR
537100940127     C*
537200950524     C* preparo i parametri di chiamata al driver
537300950524     C                   CLEAR                   XMPEDS
537400950524     C                   EVAL      TpChiamata = '01'
537500950524     C                   EVAL      NChiavi = 5
537600950524     C                   EVAL      VLogica = 2
537700950524     C*
537800950524     C* preparo le chiavi per la chiamata al driver
537900950524     C                   CLEAR                   XMPEDS1
538000950524     C                   MOVEL     XSCSOC        DSMPESOC
538100950524     C                   MOVEL     DSPGM         DSMPEPGM
538200950524     C                   MOVEL     KNMUS         DSMPEPRF
538300950524     C* Si reperiscono i dati di default a seconda delle modalità con
538400950524     C* cui il pgm è usato
538500950524     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
538600950524     C* MPETIP = 'PV'  pgm richiamato da menù
538700950524     C* MPETIP = 'PB'  x lancio Batch
538800941013     C                   SELECT
538900090323    >C     OPZL76        WHENEQ    '01'
539000950524     C                   MOVEL     'PX'          DSMPETIP
539100941013     C                   OTHER
539200950524     C                   MOVEL     'PV'          DSMPETIP
539300941013     C                   ENDSL
539400950524     C*
539500950524     C                   CALLB     'XMPE'
539600950524     C                   PARM                    XMPEDS
539700950524     C                   PARM                    XMPEDS1
539800950524     C*
539900950524     C     Risultato     IFEQ      '0'
540000950524     C                   MOVEL     DSMPEPAR      DSMPE
540100940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
540200940131     C* i suoi valori di default vengono decisi a pgm
540300940131     C                   ELSE
540400090323     C                   Z-ADD     2             $LOP
540500940131     C                   Z-ADD     1             $ORD
540600130924C2226C                   eval      $PRE    = *on
540700130924C2226C                   eval      $RGRPRE = 'P'
540800941027     C                   ENDIF
540900940131     C*
541000940131     C                   ENDSR
541100940223     C/EJECT
541200940131     C************************************************************
541300940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
541400940131     C************************************************************
541500940225     C     DEFOPZ        BEGSR
541600940131     C*
541700940225     C* Questa routine deve valorizzare la schiera $FL1
541800940225     C* con S o N a seconda della utilizzabilità o meno
541900940225     C* dell'opzione correlata
542000940225     C*
542100950301     C* Inizialmente si considerano tutte le opzioni come valide
542200940317     C     1             DO        $DIMOP        X
542300960119     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
542400960117     C                   If        $TX1(X)  <> *Blank
542500940225     C                   MOVE      'S'           $FL1(X)
542600960117     C                   Else
542700960117     C                   Leave
542800960117     C                   EndIf
542900941027     C                   ENDDO
543000940715     C*
543100940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
543200941018     C* con 1 o 0 a seconda della utilizzabilità o meno
543300940715     C* del tasto funzione correlato
543400940715     C*
543500940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
543600940715     C* di compilazione
543700940715     C                   RESET                   $FC1
543800940715     C                   RESET                   $FR1
543900940715     C                   RESET                   $FC2
544000940715     C                   RESET                   $FR2
544100940919     C                   RESET                   $FC3
544200940919     C                   RESET                   $FR3
544300940225     C*
544400940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
544500940225     C* invalido alcune opzioni
544600940225     C                   SELECT
544700940225     C* Se pgm richiamato in ricerca disattivo :
544800090323    >C     OPZL76        WHENEQ    '01'
544900940603     C* F3=Fine
545000940603     C                   Z-ADD     3             X
545100941216     C                   MOVE      '0'           $FC1(X)
545200941216     C                   MOVE      '0'           $FC2(X)
545300941216     C                   MOVE      '0'           $FC3(X)
545400950116     C*
545500940225     C* Se pgm richiamato in interrogazione disattivo :
545600090323    >C     OPZL76        WHENEQ    '05'
545700940225     C* 1=Selezione
545800940225     C                   Z-ADD     1             X
545900940225     C     01            LOOKUP    $OP1(X)                                21
546000940225     C     *IN21         IFEQ      *ON
546100940225     C                   MOVE      'N'           $FL1(X)
546200940225     C                   ENDIF
546300940225     C* 2=Modifica
546400940225     C                   Z-ADD     1             X
546500940225     C     02            LOOKUP    $OP1(X)                                21
546600940225     C     *IN21         IFEQ      *ON
546700940225     C                   MOVE      'N'           $FL1(X)
546800940225     C                   ENDIF
546900940225     C* 3=Copia
547000940225     C                   Z-ADD     1             X
547100940225     C     03            LOOKUP    $OP1(X)                                21
547200940225     C     *IN21         IFEQ      *ON
547300940225     C                   MOVE      'N'           $FL1(X)
547400940225     C                   ENDIF
547500940225     C* 4=Annullamento
547600940225     C                   Z-ADD     1             X
547700940225     C     04            LOOKUP    $OP1(X)                                21
547800940225     C     *IN21         IFEQ      *ON
547900940225     C                   MOVE      'N'           $FL1(X)
548000940225     C                   ENDIF
548100940225     C*
548200090323    >C     OPZL76        WHENEQ    *BLANKS
548300940225     C* Se pgm non richiamato, disattivo subito :
548400941124     C* F8=Selezione seguente
548500941124     C                   Z-ADD     8             X
548600941206     C                   MOVE      '0'           $FC1(X)
548700941206     C                   MOVE      '0'           $FC2(X)
548800940225     C*
548900950301     C* In tutti gli altri casi, disattivo subito :
549000941228     C                   OTHER
549100941228     C*
549200940225     C                   ENDSL
549300941118     C*
549400941118     C* poi, disattivo a seconda delle autorizzazioni :
549500070524    >C*                  IF        AUT___ <> *on
549600070524    >C*                  Z-ADD     1             X
549700070524    >C*    XX            LOOKUP    $OP1(X)                                21
549800070524    >C*    *IN21         IFEQ      *ON
549900070524    >C*                  MOVE      'N'           $FL1(X)
550000070524    >C*                  ENDIF
550100070524    >C*                  ENDIF
550200940225     C*
550300940225     C                   ENDSR
550400940225     C/EJECT
550500940225     C************************************************************
550600940225     C* REPERIMENTO OPZIONI
550700940225     C************************************************************
550800940225     C     REPOPZ        BEGSR
550900940225     C*
551000940314     C     1             DO        $DIMOP        X
551100940128     C*
551200940223     C     $MS1(X)       IFNE      *BLANK
551300940128     C*
551400940223     C                   MOVEL     $MS1(X)       IDMSG
551500950522     C                   CALLB     'XRTVM'
551600940201     C                   PARM                    IDMSG            27
551700940201     C                   PARM                    TXTMSG          100
551800940201     C                   PARM                    ERRMSG            1
551900940128     C*
552000940128     C     ERRMSG        IFNE      *ON
552100940223     C                   MOVEL     TXTMSG        $TX1(X)
552200940128     C                   ELSE
552300940223     C                   MOVEL     *ALL'?'       $TX1(X)
552400941027     C                   ENDIF
552500940128     C*
552600941027     C                   ENDIF
552700940128     C*
552800941027     C                   ENDDO
552900940128     C*
553000940128     C                   ENDSR
553100940506     C/EJECT
553200940506     C************************************************************
553300940506     C* REPERIMENTO TASTI DI FUNZIONE
553400940506     C************************************************************
553500940506     C     REPKEY        BEGSR
553600940506     C                   MOVEA     $FM1          $FM
553700950721     C                   MOVEA     $FC1          $FC
553800940506     C                   EXSR      REPKKK
553900940506     C                   MOVEA     $FX           $FX1
554000940506     C*
554100940506     C                   MOVEA     $FM2          $FM
554200950721     C                   MOVEA     $FC2          $FC
554300940506     C                   EXSR      REPKKK
554400940506     C                   MOVEA     $FX           $FX2
554500940919     C*
554600940919     C                   MOVEA     $FM3          $FM
554700950721     C                   MOVEA     $FC3          $FC
554800940919     C                   EXSR      REPKKK
554900940919     C                   MOVEA     $FX           $FX3
555000940919     C                   ENDSR
555100940506     C/EJECT
555200940506     C************************************************************
555300940506     C* REPERIMENTO TASTI DI FUNZIONE
555400940506     C************************************************************
555500940506     C     REPKKK        BEGSR
555600940506     C*
555700940506     C                   CLEAR                   $FX
555800940506     C     1             DO        24            X
555900940506     C*
556000940506     C     $FM(X)        IFNE      *BLANK
556100950721     C     $Fc(X)        andne     '0'
556200940506     C*
556300940506     C                   MOVEL     $FM(X)        IDMSG
556400950522     C                   CALLB     'XRTVM'
556500940506     C                   PARM                    IDMSG            27
556600940506     C                   PARM                    TXTMSG          100
556700940506     C                   PARM                    ERRMSG            1
556800940506     C*
556900940506     C     ERRMSG        IFNE      *ON
557000940506     C                   MOVEL     TXTMSG        $FX(X)
557100940506     C                   ELSE
557200940506     C                   MOVEL     *ALL' '       $FX(X)
557300941027     C                   ENDIF
557400940506     C*
557500941027     C                   ENDIF
557600940506     C*
557700941027     C                   ENDDO
557800940506     C*
557900940506     C                   ENDSR
558000940223     C/EJECT
558100940128     C************************************************************
558200950606     C* DEFINIZIONE KLIST
558300940128     C************************************************************
558400950606     C     DefKlist      BEGSR
558500940128     C*
558600940223     C* klist
558700090324    >C* AnUni01L
558800090324    >C     K02Uni01      KLIST
558900090324    >C                   KFLD                    UNISocieta
559000090324    >C                   KFLD                    UNIUnita
559100100413    >C* NdRCa01L
559200100413    >C     K07Rca01      KLIST
559300100402    >C                   KFLD                    RcaSocieta
559400100402    >C                   KFLD                    RcaPosiz
559500100402    >C                   KFLD                    RcaKCC
559600100402    >C                   KFLD                    RcaKSC
559700100402    >C                   KFLD                    RcaDtpar
559800100402    >C                   KFLD                    RcaNrpar
559900100402    >C                   KFLD                    RcaSerPar
560000101209c2176c*
560100101209  " >C     K07Rca04      KLIST
560200101209  " >C                   KFLD                    RcaSocieta
560300101209  " >C                   KFLD                    RcaPosiz
560400101209  " >C                   KFLD                    RcaKCC
560500101209  " >C                   KFLD                    RcaKSC
560600101209  " >C                   KFLD                    RcaDtpar
560700101209  " >C                   KFLD                    RcaNrpar
560800101209  "  C                   KFLD                    RcaSerPar
560900101209  "  C*
561000101209  " >C     K08Rca04      KLIST
561100101209  " >C                   KFLD                    RcaSocieta
561200101209  " >C                   KFLD                    RcaPosiz
561300101209  " >C                   KFLD                    RcaKCC
561400101209  " >C                   KFLD                    RcaKSC
561500101209  " >C                   KFLD                    RcaDtpar
561600101209  " >C                   KFLD                    RcaNrpar
561700101209  "  C                   KFLD                    RcaSerPar
561800101209c2176C                   KFLD                    RcaDtSca
561900130923C2226C* Attività Manuali
562000131129  "  C     K08Att01      KLIST
562100130923  "  C                   KFLD                    AttSocieta
562200130923  "  C                   KFLD                    AttKCC
562300130923  "  C                   KFLD                    AttKSC
562400130923  "  C                   KFLD                    AttDtpar
562500130923  "  C                   KFLD                    AttNrpar
562600130923  "  C                   KFLD                    AttSerPar
562700130923  "  C                   KFLD                    AttDtSca
562800130923  "  C                   KFLD                    AttProgr
562900130923  "  C*
563000130923  "  C     K07Att01      KLIST
563100130923  "  C                   KFLD                    AttSocieta
563200130923  "  C                   KFLD                    AttKCC
563300130923  "  C                   KFLD                    AttKSC
563400130923  "  C                   KFLD                    AttDtpar
563500130923  "  C                   KFLD                    AttNrpar
563600130923  "  C                   KFLD                    AttSerPar
563700130923  "  C                   KFLD                    AttDtSca
563800130923C2226C*
563900110412     C                   ENDSR
564000110412D2680C************************************************************
564100110412  "  C* Chiamata a xrtvm
564200110412  "  C************************************************************
564300110412D2680C     PrendiMsg     BEGSR
564400110412     C*
564500110412     C                   CALLB     'XRTVM'
564600110412     C                   PARM                    IDMSG            27
564700110412     C                   PARM                    TXTMSG          100
564800110412     C                   PARM                    ERRMSG            1
564900110412     C                   PARM      *off          CTRMSG            1            centratura
565000110412     C                   PARM                    LENMSG            3 0          lun output.
565100110412     C                   PARM                    MSgDta          100            lun output.
565200110412     C                   PARM                    TxtMsg30         30            lun output.
565300110412     C                   PARM                    TxtMsg3000     3000            lun output.
565400110412     C     ERRMSG        IFNE      *ON
565500110412     C                   MOVEL     TxTMsg        WWMSG           100            *
565600110412     C                   ELSE
565700110412     C                   MOVEL     *ALL'?'       WWMSG
565800110412     C                   ENDIF
565900110412     C*
566000110412D2680C                   ENDSR
566100130910C2226C************************************************************
566200130910  "  C* Reperisco le unità abilititate all'utente
566300130910  "  C************************************************************
566400130910C2226C     CarUniAbi     BEGSR
566500130910     C                   Clear                   WinUni
566600130910     C     XscSoc        setll     AnUni01l
566700130910     c                   do        *hival
566800130910     c     XscSoc        reade     AnUni01l
566900130910     c                   If        %eof(AnUni01l)
567000130910     c                   Leave
567100130910     c                   Endif
567200130910     C* Controllo autorizzazione all'unità
567300130910     C                   RESET                   XAutUniDS
567400130910     C                   EVAL      XAUPrf = KNmUs
567500130910     C                   EVAL      XAUSoc = XscSoc
567600130910     C                   EVAL      XAUUni = UniUnita
567700130910     C                   CALL      'XAUTUNI'
567800130910     C                   PARM                    XAutUniDS
567900130910     C     XAUAbl        IFNE      *OFF
568000130910     C                   If        WinUni = *blanks
568100130910     C                   Eval      WInUni='in (''' + %trimr(UniUnita)
568200130910     C                             + ''''
568300130910     C                   Else
568400130910     C                   Eval      WInUni=%trimr(WinUni) + ',' +''''
568500130910     C                             + %trimr(UniUnita)
568600130910     C                             + ''''
568700130910     C                   Endif
568800130910     C                   ENDIF
568900130910     C                   Enddo
569000130910     C                   If        WinUni <> *blanks
569100130910     C                   Eval      WInUni=%trimr(WinUni) +')'
569200130910     C                   Endif
569300130910     C*
569400130910C2226C                   ENDSR
569500940223     C/EJECT
569600980319** $MS1 / $TX1
569700090323PROMSG    *LIBL     OPZ0001
569800101209PROMSG    *LIBL     OPZ0458                                          C2176
569900090406PROMSG    *LIBL     OPZ0642
570000980319** $OP1 / $FL1
57010009032301S      <Scelta
57020010120915S      <Chiusura fasi partita                                      C2176
57030009040216S      <Interrogazioni
570400130912** $OP2 / $FL2                                                       C2226
57050013091201S      <Scelta                                                     C2226
570600980319** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
570700940506PROMSG    *LIBL     KEY0001  01
570800940506PROMSG    *LIBL     KEY0002  02
570900940506PROMSG    *LIBL     KEY0003  03
571000940506PROMSG    *LIBL     KEY0004  04
571100940506PROMSG    *LIBL     KEY0005  05
571200070531PROMSG    *LIBL     KEY0323  06
571300940506PROMSG    *LIBL     KEY0007  07
571400940506PROMSG    *LIBL     KEY0008  08
571500940506PROMSG    *LIBL     KEY0009  09
571600940506PROMSG    *LIBL     KEY0010  10
571700940506PROMSG    *LIBL     KEY0011  11
571800940506PROMSG    *LIBL     KEY0012  12
571900940506PROMSG    *LIBL     KEY0013  13
572000940506PROMSG    *LIBL     KEY0014  14
572100940506PROMSG    *LIBL     KEY0015  15
572200940506PROMSG    *LIBL     KEY0016  16
572300940506PROMSG    *LIBL     KEY0017  17
572400940506PROMSG    *LIBL     KEY0018  18
572500940506PROMSG    *LIBL     KEY0019  19
572600940506PROMSG    *LIBL     KEY0020  20
572700940506PROMSG    *LIBL     KEY0021  21
572800940506PROMSG    *LIBL     KEY0022  22
572900940506PROMSG    *LIBL     KEY0023  23
573000940506PROMSG    *LIBL     KEY0024  24
573100980319**  ABILITAZIONE D1 : $FC1 / $FR1
5732009407200N  01
5733009507031N  02
5734009407201N  03
5735009407201N  04
5736009407200N  05
5737000903230N  06
5738009407200N  07
5739009412061N  08
5740009407200N  09
5741000705250N  10
5742009407200N  11
5743009407201N  12
5744009407200N  13
5745009407200N  14
5746009407200N  15
5747009407200N  16
5748009407200N  17
5749000705311N  18
5750009407200N  19
5751009407200N  20
5752009407200N  21
5753009407200N  22
5754009507260N  23   Impostare a '1' se si utilizzano opzioni
5755009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
575600980319** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
575700940916PROMSG    *LIBL     KEY0001  01
575800940916PROMSG    *LIBL     KEY0002  02
575900940916PROMSG    *LIBL     KEY0003  03
576000940916PROMSG    *LIBL     KEY0004  04
576100940916PROMSG    *LIBL     KEY0005  05
576200070531PROMSG    *LIBL     KEY0323  06
576300940916PROMSG    *LIBL     KEY0007  07
576400940916PROMSG    *LIBL     KEY0008  08
576500940916PROMSG    *LIBL     KEY0009  09
576600940916PROMSG    *LIBL     KEY0010  10
576700940916PROMSG    *LIBL     KEY0011  11
576800940916PROMSG    *LIBL     KEY0012  12
576900090323PROMSG    *LIBL     KEY0061  13
577000940916PROMSG    *LIBL     KEY0014  14
577100940916PROMSG    *LIBL     KEY0015  15
577200940916PROMSG    *LIBL     KEY0016  16
577300940916PROMSG    *LIBL     KEY0017  17
577400940916PROMSG    *LIBL     KEY0018  18
577500940916PROMSG    *LIBL     KEY0019  19
577600940916PROMSG    *LIBL     KEY0020  20
577700940916PROMSG    *LIBL     KEY0021  21
577800940916PROMSG    *LIBL     KEY0022  22
577900940916PROMSG    *LIBL     KEY0023  23
578000940916PROMSG    *LIBL     KEY0024  24
578100980319**  ABILITAZIONE S1 : $FC2 / $FR2
5782009409160N  01
5783009409160N  02
5784009409161N  03
5785009409160N  04
5786001309241N  05                                       *C2226*
5787000903230N  06
5788009409160N  07
5789009411241N  08
5790009409160N  09
5791000705250N  10
5792009409160N  11
5793009409161N  12
5794000903231N  13
5795009409160N  14
5796009409160N  15
5797009409160N  16
5798009409161N  17
5799000705311N  18
5800009409160N  19
5801009409160N  20
5802009409160N  21
5803009409160N  22
5804009507261N  23   Impostare a '1' se si utilizzano opzioni
5805009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
580600980319** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
580700940506PROMSG    *LIBL     KEY0001  01
580800940506PROMSG    *LIBL     KEY0002  02
580900940506PROMSG    *LIBL     KEY0003  03
581000940506PROMSG    *LIBL     KEY0004  04
581100940506PROMSG    *LIBL     KEY0005  05
581200940506PROMSG    *LIBL     KEY0006  06
581300940506PROMSG    *LIBL     KEY0007  07
581400940506PROMSG    *LIBL     KEY0008  08
581500940506PROMSG    *LIBL     KEY0009  09
581600940506PROMSG    *LIBL     KEY0010  10
581700940506PROMSG    *LIBL     KEY0011  11
581800940506PROMSG    *LIBL     KEY0012  12
581900940506PROMSG    *LIBL     KEY0013  13
582000940506PROMSG    *LIBL     KEY0014  14
582100940506PROMSG    *LIBL     KEY0015  15
582200940506PROMSG    *LIBL     KEY0016  16
582300940506PROMSG    *LIBL     KEY0017  17
582400940506PROMSG    *LIBL     KEY0018  18
582500940506PROMSG    *LIBL     KEY0019  19
582600940506PROMSG    *LIBL     KEY0020  20
582700940506PROMSG    *LIBL     KEY0021  21
582800940506PROMSG    *LIBL     KEY0022  22
582900940506PROMSG    *LIBL     KEY0023  23
583000940506PROMSG    *LIBL     KEY0024  24
583100980319**  ABILITAZIONE D3 : $FC3 / $FR3
5832009407200N  01
5833000903241N  02
5834009407201N  03
5835000903241N  04
5836009407200N  05
5837009407200N  06
5838009407200N  07
5839009407200N  08
5840009407200N  09
5841009409160N  10
5842009407200N  11
5843009407201N  12
5844009407200N  13
5845009407200N  14
5846009407200N  15
5847009407200N  16
5848009409160N  17
5849009409160N  18
5850009407200N  19
5851009407200N  20
5852009407200N  21
5853009407200N  22
5854009507260N  23   Impostare a '1' se si utilizzano opzioni
5855009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
585600110412** MSG
585700110412PROMSG    *LIBL     KEY0012    F12=Ritorno                D2680
585800130924PROMSG    *LIBL     KEY0006    F06=Conferma               C2226
