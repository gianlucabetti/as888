000100000125     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(PNOTA)
000200021025D1215H*PARMS BNDDIR(PJXBND PJCBND)
000300950915     H*PARMS CVTOPT(*DATETIME)
000400940223     H DECEDIT('0,') DATEDIT(*DMY.)
000500940117      *
000600951019    > *            GESTIONE RIEPILOGO PRIMANOTA
000700000810      *           ------------------------------
000800940223      *
000900000825C1408 * ATTENZIONE - è stata aggiunta la gestione della p.n. intercompany:
001000000825  "   *              deve essere sempre l'ultima cosa che esegue il pgm !!!
001100000825C1408 *              (se si aggiungono nuove funzioni farlo prima di questa)
001200060202D1938 *              Ristrutturato il pgm inserendo una sbr per ogni
001300060202D1938 *              bottone nella routine INZD1 per maggior chiarezza
001400940128     F*----------------------------------------------------*
001500960806     F*------------
001600960806     FANDFT01L  IF   E           K DISK
001700030624D1381FNDDES01L  IF A E           K DISK
001800040325C1741FNDMRA06L  IF   E           K DISK
001900070801C2049FNDREG01L  IF   E           K DISK
002000941215     F*------------
002100950816    >FNDC039D   CF   E             WORKSTN
002200950816     F                                     RENAME(D039D1:FMTD1)
002300970115     F                                     RENAME(D039D2:FMTD2)
002400940128     D*----------------------------------------------------*
002500000823C1408D*-------------
002600000823  "  D* Dati per pgm XSOC
002700000823  "  D SOC001        E DS                  EXTNAME(XSOC001DS)
002800020212C1408D    XScRegInt                   1A   Overlay(XScSkP:7)
002900020212C1530D    XscLaguna                   1    overlay(xscSkp:8)                    Scadenz. retroattivo
003000020212C1408D*-------------
003100000823  "  D* DS Interna per dati utente
003200000823  "  D XSOCDS          DS          1000
003300000823  "  D*-------------
003400000823  "  D* Reperimento autorizzazioni
003500000823  "  D XCHKAUTDS     E DS
003600000823  "  D*-------------
003700000823  "  D* Maschera dati autorizzazioni reg. intercompany
003800000824  "  D CONREIDS      E DS
003900000824  "  D*-------------
004000000901  "  D* Parametri pgm attivazione p.n. intercompany
004100041021C1408D NDC299DS      E DS                  Inz
004200041021C1761X** ex C1408 ** Err299      E                     Inz('0')
004300041021  "  X** ex C1408 **
004400041021  "  X** ex C1408 ** Prototipo procedure P.N.INTERCOMPANY
004500041021C1761X** ex C1408 **/COPY *LIBL/SRCCPY,PJC005PR
004600041021C1761 *----------------------------------------------------*
004700041021  "   * Parametri per contabilizzare pn intercompany
004800041021  "   *-----------
004900041021  "   * DS tracciato record NDBHM
005000041021  "  D NDBHMDS       E DS                  ExtName(NDBHM00F)
005100041021  "  D NDBHMParm       s                   like(NDBHMDS)
005200041021  "   *-----------
005300041021  "   * controllo esistenza condizioni pn intercompany
005400041021  "  D NDC487DS      E DS                  INZ
005500041021  "  D  RET487       E                     INZ('0')
005600041021  "  D  OPR487       E                     INZ('0')
005700041021  "  D  ERR487       E                     INZ('0')
005800041021  "  D  ERRPRO487    E                     INZ('0')
005900041021C1761D  ERRdes487    E                     INZ('0')
006000960409     D*-------------
006100020304C1530x*ANOPEDS       e DS                  ExtName(ANOPE00f)   inz
006200020304C1530D ANOPEDS       e DS                  ExtName(ANOPE00f) inz export
006300021211C1579D*-------------
006400021211  "  D* Chiamata ricorsiva
006500021211C1579D X48CHPGMDS    E DS                  INZ
006600951019     D*-------------
006700951019     D* Parametri del PGM invio posta ANA727R
006800951019     D A727DS        E DS                  EXTNAME(ANA727DS) INZ
006900951019     D  RET727       E                     INZ('0')
007000951019     D  OPR727       E                     INZ('0')
007100951019     D  ERR727       E                     INZ('0')
007200951019     D*-------------------------------
007300951019     D DataDiotto      DS             8
007400951019     D  aammgg                 1      8    inz('00/00/00')
007500951019     D  gg                     1      2
007600951020     D  mm                     4      5
007700951020     D  aa                     7      8
007800951019     D*-------------------------------
007900951019     D* Parametri del PGM invio posta ANA727R
008000951019     D TestoPosta      DS            50
008100951019     D  Kost1                  1      4    inz('Reg.')
008200951019     D  NrRegPt                5      8
008300951019     D  SerRegPt              10     18
008400951019     D  Kost2                 19     27    inz(' in data ')
008500951019     D  DtRegPt               28     35
008600000810     D*-------------
008700951011     D* Richiamo a XATB
008800951011     D XATBDS        E DS                  INZ
008900000810     D*-------------
009000951011     D* DS esterna stato
009100951011     D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
009200970113     D*-------------
009300960313     D £NDREG        E DS                  EXTNAME(NDREG00F) import
009400970612     D NDREGSAV        S                   like(£NDREG)
009500000810     D*-------------
009600940127     D* Passaggio Parametri
009700950731     D KPJBA         E DS
009800000810     D*-------------
009900951011     D* intra
010000951011     D ISW050DS      E DS                  Inz
010100951011     D  DRF050       E                     Inz(DatMin)
010200110321B1142D  DTRIF050     E                     Inz(DatMin)
010300970113     D*-------------
010400951030     D* Parametri del PGM descrizioni aggiuntive
010500951030     D NDC027DS      E DS                  INZ
010600951030     D  RET027       E                     INZ('0')
010700951030     D  OPR027       E                     INZ('0')
010800951030     D  ERR027       E                     INZ('0')
010900000810     D*-------------
011000951019     D* Parametri del PGM di manutenzione NDC061R
011100951019    >D NDC061DS      E DS                  inz
011200951019    >D  RET061       E                     INZ('0')
011300951019    >D  OPR061       E                     INZ('0')
011400951019    >D  ERR061       E                     INZ('0')
011500000810     D*-------------
011600951005     D* anagrafica ritenute d'acconto
011700951005     D NDCR05DS1     E DS                  Inz
011800951005     D  OPZR105      E                     Inz('02')
011900951005     D  CALR105      E                     Inz('P')
012000951005     D  RETR105      E                     Inz('0')
012100951005     D  OPRR105      E                     Inz('0')
012200951005     D  ERRR105      E                     Inz('0')
012300951005     D  CMTR105      E                     Inz('0')
012400951005    >D  DRGR105      E                     Inz(DatMin)
012500951005    >D  DDCR105      E                     Inz(DatMin)
012600951005    >D  DRPR105      E                     Inz(DatMin)
012700000810     D*-------------
012800961209     D* ritenute d'acconto
012900961209     D NDCR04DS      E DS                  Inz
013000040325C1741D*-------------
013100040325  "  D* ritenute d'acconto(Pagamento)
013200040325C1741D NDCR14DS      E DS                  Inz
013300000810     D*-------------
013400961209     D* guida ritenute d'acconto
013500961209     D NDCR05DS      E DS                  Inz
013600961209     D  OPZR05       E                     Inz('02')
013700961209     D  RETR05       E                     Inz('0')
013800961209     D  OPRR05       E                     Inz('0')
013900961209     D  ERRR05       E                     Inz('0')
014000961209     D  CMTR05       E                     Inz('0')
014100961209    >D  DRGR05       E                     Inz(DatMin)
014200961209    >D  DDCR05       E                     Inz(DatMin)
014300000810     D*-------------
014400950731     D* anagrafica partite
014500951005     D NDC060DS      E DS                  Inz
014600950801     D  RET060       E                     INZ('0')
014700950801     D  OPR060       E                     INZ('0')
014800950801     D  ERR060       E                     INZ('0')
014900950801     D  CMT060       E                     INZ('0')
015000950920    >D  DTPAR060     E                      inz(DatMin)
015100950920    >D  UQ4060       E                      inz(DatMin)
015200960313    >D  DTchiu060    E                      inz(DatMin)
015300000810     D*-------------
015400950731     D* mix partita
015500950817     D NDC065DS      E DS                  inz
015600950801     D  RET065       E                     INZ('0')
015700950801     D  OPR065       E                     INZ('0')
015800950801     D  ERR065       E                     INZ('0')
015900950801     D  CMT065       E                     INZ('0')
016000950920    >D  DTPAR065     E                      inz(DatMin)
016100950920    >D  UQ4065       E                      inz(DatMin)
016200000810     D*-------------
016300950801     D* tipo modello primanota
016400950817     D NDC052DS      E DS                  inz
016500950801     D  RET052       E                     INZ('0')
016600950801     D  OPR052       E                     INZ('0')
016700950801     D  ERR052       E                     INZ('0')
016800950801     D  CMT052       E                     INZ('0')
016900000810     D*-------------
017000950731     D* ds input ND011DS
017100950731     D ND004DS       E DS                  inz
017200950920     D  DTREG004     E                      inz(DatMin)
017300950920     D  DTDOC004     E                      inz(DatMin)
017400950920     D  DTLGIO004    E                      inz(DatMin)
017500950920     D  DTRFCAM004   E                      inz(DatMin)
017600950920     D  DTCOCO004    E                      inz(DatMin)
017700950920     D  DTCOAN004    E                      inz(DatMin)
017800020304C1547 * nd004ds contiene i dati da passare a ndmvc111:
017900020304  "   * le date non inizializzate, sono passate = blank
018000020304  "   * (e ndmvc111 si spacca)
018100020304C1547D   DTDCPAG004  e                      inz(DatMin)
018200960514     D*-------------
018300050209c1776x* ds input NDCS01DS
018400050209 "   x*NDCS01DS      E DS                  inz
018500050209 "   x* DtRegDaS01   E                      inz(DatMin)
018600050209 "   x* DtRegAS01    E                      inz(DatMin)
018700050209 "   c* ds input NDCSC2DS
018800050209 "   D  NDCSC2DS     E DS                  inz
018900050209 "   D  DtRegDaSC2   E                      inz(DatMin)
019000050209C1776D  DtRegASc2    E                      inz(DatMin)
019100960514     D*-------------
019200950921     D  wNrReg         S                    Like(NrReg039)
019300960514     D*-------------
019400950731     D* ds tracciato record NDREG
019500960313     D NDREGDS         s           4000
019600000810     D*-------------
019700950921     D* ds tracciato interrogazioni ANA399R
019800950921     D ANA399DS      E DS                  inz
019900960807     D*-------------
020000960807     D* DS esterna per default di causale
020100960807     D ANDFT         E DS                  EXTNAME(ANDFT00F)
020200060531D2001D*-------------
020300060531  "  D* Parametro modulo UEREGCTB
020400060531  "  D ANP053DS      E DS
020500060531D2001D*-------------
020600030113C1605D*-------------
020700030113  "  D* Parametro modulo LAGUNA
020800030113  "  D ANP062DS      E DS
020900030113  "  D*-------------
021000030113C1605D XPAOUT          DS          2000
021100051123D1901D*-------------
021200051123  "  D* Laguna
021300051123  "  D ANLGN00F      E DS
021400051123  "  D*-------------
021500051123  "  D* per sapere quante sono le chiavi primarie della classe ProJ in
021600051123  "  D* ingresso
021700051123D1901D ANG1L1DS      E DS
021800040616     D*-------------
021900040616C1767D* DS per controllo ratei
022000040616  "  D NDC326DS      E DS                  INZ
022100040616  "  D*-------------
022200040616  "  D* DS per chiamata ratei
022300040616  "  D NDC327DS      E DS                  INZ
022400040616  "  D  DtRegDa327   E                      inz(DatMin)
022500040616  "  D  DtRegA327    E                      inz(DatMin)
022600040616C1767D*-------------
022700950731     D* campi vari
022800950816     D OSys            S                   Like(Sys039)
022900950816     D ONrAsReg        S                   Like(NrAsReg039)
023000000518D0564D Sys271          S                   Like(Sys039)
023100000518D0564D NrasReg271      S                   Like(NrAsReg039)
023200020212C1530D*-------------
023300020226  "  D* schiera contenente le chiavi ProJ per il documento su Laguna
023400020212  "  D* al momento dell'APERTURA della registrazione, cioè prima di
023500020212  "  D* ogni possibile modifica
023600020212  "  D* (servono solo in modifica per sapere se sono state variate);
023700020212  "  D* uso una schiera grande come le 8 chiavi primarie di Laguna che
023800020212  "  D* sono diverse per ogni tipo registrazione. Elenco:
023900020212  "  D*  Doc.IVA Fornitore : (F)attura o (N)ota/estremi doc./sottoconto
024000020212  "  D*  Doc.IVA Cliente   : estremi doc.
024100020304  "  D KeyOld          DS
024200020304  "  D KeyOldLgn                     15A   dim(8)
024300020226  "  D* schiera contenente le chiavi ProJ per il documento su Laguna
024400020226  "  D* al momento della CONFERMA della registrazione, cioè dopo
024500020226  "  D* ogni possibile modifica
024600020304  "  D KeyNew          DS
024700020304  "  D KeyNewLgn                     15A   dim(8)
024800020221  "  D* flag per sapere se sono stati modificati dei dati della
024900021025C1530D* primanota che sono chiavi di Laguna
025000021025D1215D*KeyLgnModif     S              1    export
025100021025D1215D KeyLgnModif     S              1
025200021025D1215D*  schiera contenente le nuove chiavi secondarie ProJ
025300030210D1215D Key2NewLgn      S             15A   dim(8)
025400051123D1901D*  schiera appoggio per chiavi ProJ
025500051123D1901D KeyLgn          S             15A   dim(8)
025600030210D1270D*  schiera contenente i vecchi descrittori ProJ
025700030210  "  D WDesOld         S                   like(DesOld)
025800030210  "  D DesOld          DS
025900030210  "  D DesOldLgn                     44A   dim(10)
026000030210  "  D*  schiera contenente i nuovi descrittori ProJ
026100030210D1270D DesNew          DS
026200030210D1270x*DesNewLgn       S             44A   dim(10)
026300030210D1270D DesNewLgn                     44A   dim(10)
026400030210D1270D* flag per sapere se sono stati modificati dei dati della
026500030210  "  D* primanota che sono descrittori di Laguna
026600030210D1270D DesLgnModif     S              1
026700030210C1530D* campi per girare il Nr.Documento da Packed ad Alfanumerico
026800020221  "  D NrDocSigned     S              9S 0
026900020213C1530D NrDocAlfa       S              9A
027000021025D1215D* campi per girare il Sys/NrAsReg da Packed ad Alfanumerico
027100021025D1215D NrAsRegSigned   S              9S 0
027200021025D1215D SysSigned       S              3S 0
027300021025D1215D ClasseProJ      S              3A
027400051123D1901D*-------------
027500051123  "  D* Parametri driver Laguna
027600051123  "  D DR17Opzione     S              2A
027700051123  "  D DR17Errore      S              1A
027800051123  "  D DR17Vista       S              2S 0
027900051123  "  D DR17Dati        S           4000A
028000051123  "  D DR17Lunghezza   S              9B 0
028100051123  "  D DR17Commit      S              1A
028200051123  "  D DR17NrChiavi    S              2S 0
028300051123  "  D DR17Alloca      S              1A
028400051123D1901D DR17DataISO     S               D
028500051123D1901D*----------------
028600051123D1901D DocCatalogato   S              1A
028700021211C1579D*----------------
028800021211  "  D* Parametri utilizzati per chiamata al driver PJC00701R
028900021211  "  D* di gestione raccordo per c/c di pool
029000021211  "  D §Opzione        S              2
029100021211  "  D §Societa        S              3A
029200021211  "  D §SysReg         S              3P 0
029300021211  "  D §NrAsReg        S              9P 0
029400021211  "  D §NrRigaM        S              5P 0
029500021211  "  D §RifInt         S              9A
029600021211  "  D §SubNum         S              9P 0
029700021211  "  D §Errore         S              1
029800021211  "  D §Dati           S           4000
029900021211  "  D §Lunghezza      S              9B 0
030000021211  "  D §Commit         S              1
030100021211  "  D*----------------
030200021211  "  D* DS passaggio dati
030300021211  "  D NDCPRDS       E DS                  Inz ExtName(NDCPR00F)
030400021211  "  D*-------------
030500021211  "  D* Parametri del PGM di gestione NDC534R
030600021211  "  D NDC534DS      E DS                  Inz
030700021211  "  D  RET534       E                     INZ('0')
030800021211  "  D  OPR534       E                     INZ('0')
030900021211  "  D  ERR534       E                     INZ('0')
031000021211  "  D  CMT534       E                     INZ('0')
031100021211  "  D  DtImmPD534   E                     Inz(DatMin)
031200021211  "  D  DtImmPA534   E                     Inz(DatMin)
031300021211  "  D  DtRegPD534   E                     Inz(DatMin)
031400021211  "  D  DtRegPA534   E                     Inz(DatMin)
031500021211  "  D  DtImmDD534   E                     Inz(DatMin)
031600021211  "  D  DtImmDA534   E                     Inz(DatMin)
031700021211  "  D  DtRegDD534   E                     Inz(DatMin)
031800021211C1579D  DtRegDA534   E                     Inz(DatMin)
031900110119C2179D*-------------
032000110119  "  D* Parametri del PGM di gestione NDC740R
032100110315  "  D NDC741DS      E DS                  inz
032200110315C2179D  DTREGL741    E                     inz(DatMin)
032300940318     D*-------------
032400950126     D* Reperimento nome PGM
032500950126     D STATUS         SDS           333
032600950126     D  DSPGM            *PROC
032700950411     D  PARMS            *PARMS
032800061215D2100D  STSUSR               254    263
032900970113     D*-------------
033000970113     D* Passaggio Parametri al PGM di scrittura PN collegata
033100970113    >D NDC029DS      e DS
033200940203     D*-------------
033300940201     D* Passaggio Parametri al PGM di manutenzione
033400000823    >D NDC039DS      E DS
033500950915     D*-------------
033600950920     D  DatMin         C                   const('0001-01-01')
033700940201     D*-------------
033800940201     D DSFMT           DS
033900940506     D  $TASTO               369    369
034000940201     D  NRG                  370    370
034100940201     D  NCL                  371    371
034200000810     D*-------------
034300950725     D* Definizioni parametri standard chiamate moduli
034400950725     D Operazione      S              2
034500950725     D Esito           S              1
034600950725     D GesMsg          S              1
034700950725     D LenOut          S              9B 0
034800950731     D LenIn           S              9B 0
034900960410     D StrutturaO      S             10
035000960409     D Refresh         S              1
035100960724     D Prenot          S              1
035200960806     D CiSonoDft       S              1
035300970113     D SiPnColl        S              1
035400000810     D*-------------
035500960807     D*  campo di passaggio ANDFT
035600960807     D DFTInput        S           4000
035700940201     D*-------------
035800940201     D* posizione cursore
035900940201     D CURSOR          DS
036000940223     D  RIRI                   1      2B 0 INZ
036100940201     D  R$$                    2      2
036200940223     D  COCO                   3      4B 0 INZ
036300940201     D  C$$                    4      4
036400000810     D*-------------
036500950727     D* Costanti
036600940506     D ENTER           C                   CONST(X'F1')
036700950606     D*-------------
036800950606     D* Variabili per gestione videate
036900950606     D $Gest           S              2A
037000950606     D $Fine           S              1A
037100950606     D $InzD1          S              1A
037200950606     D $LastG          S              2A
037300950606     D $LastV          S              2A
037400950606     D*-------------
037500950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
037600950606     D CurR            S              2S 0
037700950606     D CurC            S              2S 0
037800950606     D Win             S             99A
037900950606     D*-------------
038000950606     D*  Indici
038100950606     D X               S              3S 0
038200950606     D Y               S              3S 0
038300000810     D*-------------
038400000824C1408D $ElaPnI         S              1A
038500041021C1408D $ChkPnI         S              1A
038600041021C1761X** ex C1408 **$ChkRacRI       S              1A
038700041021  "  X** ex C1408 **LenNDBHM        S              9B 0
038800041021  "  X** ex C1408 **NrRigaM         S                   Like(BHMSubRig)
038900041021  "  X** ex C1408 **Segno           S              1S 0
039000041021  "  X** ex C1408 **Partite         S              1A
039100041021  "  X** ex C1408 **SocDes          S                   Like(REGSocieta)
039200041021  "  X** ex C1408 **SysDes          S                   Like(REGSys)
039300041021  "  X** ex C1408 **NrAsDes         S                   Like(REGNrAsReg)
039400041027C1761X** ex C1408 **$ForzCol        S              1A
039500041027C1408D PgmChkPgm       S             10A
039600041027  "  D EsiChkPgm       S              1A
039700041021C1408D*-------------
039800030624D1381D Ksys            S                   like(RegSys)
039900030624D1381D KNrasReg        S                   like(RegNrasREg)
040000030916     D*-------------
040100030916B0612D* Definizioni parametri driver estremi fiscali
040200030916  "  D dvcRic          S              1
040300030916  "  D dvcSocieta      S              3
040400030916  "  D dvcUnita        S              8
040500030916  "  D dvcStrutt       S             10
040600030916  "  D dvcDtRif        S               D
040700030916  "  D dvcLenOut       S              9B 0
040800030916  "  D dvcSogg         S              8
040900030916  "  D dvcErrore       S              1
041000030916  "  D dvcOutput       S           4000
041100030916  "  D* Ds per output variazioni estremi fiscali
041200030916  "  D ANCPI         E DS                  EXTNAME(ANCPI00F)
041300030916  "  D* DS esterna per decodifica conti
041400030916  "  D ND002DS       E DS
041500030916  "  D* Definizioni parametri decodifica conto
041600030916  "  D $esito          S              1
041700030916  "  D $gesmsg         S              1
041800030916  "  D $lenout         S              9B 0
041900030916  "  D $struttura      S             10
042000030916  "  D $date           S               D
042100030916  "  D $kcc            S              6
042200030916  "  D $ksc            S              8
042300030916B0612D $soc            S              3
042400040325C1741D OkRitPagam      S              1
042500050517C1772 *----------------------------------------------------*
042600050517  "   * DS per UserExit
042700050517  "  D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
042800050517C1772 *----------------------------------------------------*
042900061215D2100 * DS per controlllo autorizzazione societa
043000061215  "  D XAutSocDs     E DS                  inz
043100061215  "   *-------------
043200061215  "   * DS Esterna gestione messaggi per COMIT
043300061215  "  D XMSGDS        E DS
043400061215  "  D  STS          E                     EXTFLD(MSGSTS)
043500061215  "  D                                     DIM(333)
043600061215  "  D  JBA          E                     EXTFLD(MSGJBA)
043700061215  "  D                                     DIM(502)
043800061221  "   *-------------
043900061221  "  D $PrimoXMsg      s              1a
044000061215D2100 *----------------------------------------------------*
044100070213C2005D*-------------
044200070213  "  D* DS esterna per dati clienti e soggetti
044300070213  "  D DVACLI        E DS                  INZ
044400070213  "  D  DVCSoggX     E                     extfld(DVCSogg)
044500070213  "  D  DVCSocitaX   E                     extfld(DVCSocieta)
044600070213  "  D  DVCUnitaX    E                     extfld(DVCUnita)
044700070213  "  D  DVCDTENTR    E                      inz(DATMIN)
044800070213  "  D  DVCDTULMO    E                      inz(DATMIN)
044900070213  "  D  DVCDTINVL    E                      inz(DATMIN)
045000070213  "  D  DVCDTFIVL    E                      inz(DATMIN)
045100070213  "  D  DVCDTNASC    E                      inz(DATMIN)
045200070213  "  D  DVCDTULMOI   E                      inz(DATMIN)
045300070213  "  D  DVCDTINVLI   E                      inz(DATMIN)
045400070213  "  D  DVCDTFIVLI   E                      inz(DATMIN)
045500070213  "  D  DVCDTINRPC   E                      inz(DATMIN)
045600070213  "  D  DVCDTULMOC   E                      inz(DATMIN)
045700070213  "  D  DVCDTFIST    E                      inz(DATMIN)
045800070213  "  D  DVCDTFIAS    E                      inz(DATMIN)
045900070213  "  D  DVCDTFIRPC   E                      inz(DATMIN)
046000070213  "  D  DVCDTINRPL   E                      inz(DATMIN)
046100070213  "  D  DVCDTFIRPL   E                      inz(DATMIN)
046200070213  "  D*-------------
046300070213  "  D* DS esterna per dati fornitori e soggetti
046400070213  "  D DVAFOR        E DS                  INZ
046500070213  "  D  DVFDTENTR    E                      inz(DATMIN)
046600070213  "  D  DVFDTULMO    E                      inz(DATMIN)
046700070213  "  D  DVFDTINVL    E                      inz(DATMIN)
046800070213  "  D  DVFDTFIVL    E                      inz(DATMIN)
046900070213  "  D  DVFDTNASC    E                      inz(DATMIN)
047000070213  "  D  DVFDTULMOI   E                      inz(DATMIN)
047100070213  "  D  DVFDTINVLI   E                      inz(DATMIN)
047200070213  "  D  DVFDTFIVLI   E                      inz(DATMIN)
047300070213  "  D  DVFDTULMOF   E                      inz(DATMIN)
047400070213  "  D  DVFDTINRPF   E                      inz(DATMIN)
047500070213  "  D  DVFDTFIRPF   E                      inz(DATMIN)
047600070213  "  D  DVFDTCONT    E                      inz(DATMIN)
047700070213  "  D*-------------
047800070213  "  D* Definizioni parametri driver soggetti
047900070213  "  DDvaRic           S              1
048000070213  "  DDvaSocieta       S              3
048100070213  "  DDvaUnita         S              8
048200070213  "  DDvaStrutt        S             10
048300070213  "  DDvaDtRif         S             10A   inz('0001-01-01')
048400070213  "  DDvaLenOut        S              9B 0
048500070213  "  DDvaSnatura       S              1
048600070213  "  DDvaCodice        S              8
048700070213  "  DDvaLineav        S              3
048800070213  "  DDvaFiliale       S              3  0
048900070213  "  DDvaSogg          S              8
049000070213  "  DDvaTpInd         S              2
049100070213  "  DDvaCdInd         S              3
049200070213  "  DDvaErrore        S              1
049300070213  "  DDvaOutput        S           4000
049400070213  "  D*-------------
049500070213  "  D WCdFisc         S                   like(DVCCdFisc)
049600070213C2005D*-------------
049700070801C2049D WDataIso        S               D
049800940127     C*----------------------------------------------------*
049900940127     C*                MAIN LINE PROGRAM
050000940127     C*----------------------------------------------------*
050100940127     C*
050200940223     C* inizializzazione variabili
050300940223     C                   EXSR      INZVAR
050400940223     C*
050500940223     C     $FINE         DOWEQ     *OFF
050600940202     C     $GEST         CASEQ     'D1'          GESD1
050700941027     C                   ENDCS
050800941027     C                   ENDDO
050900940201     C*
051000940325     C* fine programma
051100940325     C                   EXSR      ENDPGM
051200940325     C/EJECT
051300940325     C************************************************************
051400000810     C* FINE PROGRAMMA
051500940325     C************************************************************
051600940325     C     ENDPGM        BEGSR
051700941214     C*
051800941214     C*
051900941214     C* se ho confermato l'uscita proseguo
052000941214     C     $FINE         IFEQ      *ON
052100940325     C*
052200940325     C                   SETON                                            RT
052300940616     C                   RETURN
052400941214     C*
052500941214     C                   ENDIF
052600940325     C*
052700940325     C                   ENDSR
052800940325     C/EJECT
052900940201     C************************************************************
053000940201     C* RICERCHE
053100940201     C************************************************************
053200940127     C     SEARCH        BEGSR
053300940127     C*
053400940127     C* determino Riga/Colonna del cursore
053500940127     C                   MOVE      NRG           R$$
053600940127     C                   MOVE      NCL           C$$
053700940127     C                   Z-ADD     RIRI          CURR
053800940127     C                   Z-ADD     COCO          CURC
053900940127     C*
054000940224     C* I campi che al momento dell'F4 risultano protetti non devono
054100940224     C* essere abilitati alla funzione di ricerca
054200941027     C*
054300941027     C                   SELECT
054400940127     C*
054500940223     C*
054600940223     C* ricerca non abilitata in questa posizione
054700941027     C                   OTHER
054800940926     C                   SETON                                        9899
054900940127     C*
055000941027     C                   ENDSL
055100950612     C*
055200950612     C* imposto pos. del cursore
055300950612     C                   Z-ADD     CURR          H1RIGA
055400950612     C                   Z-ADD     CURC          H1COLO
055500940127     C*
055600940506     C                   MOVE      *HIVAL        $LASTV
055700950612     C*
055800940127     C                   ENDSR
055900940131     C/EJECT
056000940131     C************************************************************
056100940510     C* GESTIONE VIDEO RECORD D1
056200940131     C************************************************************
056300940127     C     GESD1         BEGSR
056400940225     C* inizializzazione videata
056500941117   B1C     $INZD1        IFEQ      *ON
056600940127     C                   EXSR      INZD1
056700940223     C                   MOVE      *OFF          $INZD1
056800941117   E1C                   ENDIF
056900940707     C*
057000940707     C* se esistono errori sulla videata
057100940707     C* emetto la write del formato a indicatori spenti per vedere
057200940707     C* le eventuali decodifiche
057300941117   B1C     *IN99         IFEQ      *ON
057400940707     C     $GEST         ANDEQ     'D1'
057500940707     C                   MOVEA     *IN           WIN
057600940707     C                   MOVE      *ALL'0'       IN5098           49
057700940707     C                   MOVEA     IN5098        *IN(50)
057800951019     C                   WRITE     FMTD1
057900940707     C                   MOVEA     WIN           *IN
058000941117   E1C                   ENDIF
058100940131     C*
058200940203     C                   EXFMT     FMTD1
058300061221D2100 * Il prossimo XmsgR deve fare la close + open della window,
058400061221  "   * per 'fissare' come sottofondo, il formato appena emesso.
058500061221D2100C                   Eval      $PrimoXMsg =  *On
058600940131     C*
058700940127     C                   EXSR      CTRD1
058800940203     C*
058900950727     C     *IN99         IFEQ      *OFF
059000940224     C* eseguo aggiornamento
059100941214     C                   EXSR      AGGANA
059200940224     C* eseguo operazioni del dopo-aggiornamento
059300941117   B4C     *IN99         IFEQ      *OFF
059400940318     C                   EXSR      GESAGG
059500941117   E4C                   ENDIF
059600941117   E3C                   ENDIF
059700941027     C*
059800940117     C                   ENDSR
059900941209     C/EJECT
060000941209     C************************************************************
060100941209     C* INIZIALIZZAZIONE VIDEATA DATI
060200941209     C************************************************************
060300940127     C     INZD1         BEGSR
060400940131     C*
060500940131     C                   CLEAR                   FMTD1
060600960806     C*
060700960806     C* reperisco i default di causale
060800960806     C                   EVAL      CiSonoDft = *off
060900960806     C* se non sto gestendo un modello
061000970113   1 C                   IF        Ctb039 <> 'MD'         and
061100960806     C* e se è stato indicato il codice
061200960806     C                             RepDft039 <> *blank
061300960807     C* se il rcd di ANDFT è arrivato dai parametri di ingresso
061400970113   2 C                   IF        Parms > 2
061500020212C1530C                             and DFTInput <> *blank
061600960807     C                   EVAL      ANDFT =
061700960807     C                             %subst(DFTInPut:1:%size(ANDFT))
061800970113   3 C                   IF        DFTCdDft <> *blank
061900960807     C                   EVAL      CiSonoDft = *on
062000970113   3eC                   ENDIF
062100960807     C* se il rcd di ANDFT NON è arrivato dai parametri di ingresso
062200960807     C                   ELSE
062300960806     C                   EVAL      DftSocieta = Societa039
062400960806     C                   EVAL      DftCdDft = RepDft039
062500960806     C                   EVAL      DftUtente = UteUlMo039
062600960806     C     K03DFT01      CHAIN     ANDFT000                           21
062700970113   3 C                   IF        *in21 = *off
062800960806     C                   EVAL      CiSonoDft = *on
062900960806     C                   ELSE
063000960806     C     K02DFT01      CHAIN     ANDFT000                           21
063100970113   4 C                   IF        *in21 = *off
063200960806     C                   EVAL      CiSonoDft = *on
063300970113   4eC                   ENDIF
063400970113   3eC                   ENDIF
063500970113   2eC                   ENDIF
063600970113   1eC                   ENDIF
063700960806     C*
063800940224     C* riempio la videata
063900940224     C                   EXSR      RIED1
064000950725     C*
064100940224     C                   MOVE      *ALL'0'       IN5098
064200940224     C                   MOVEA     IN5098        *IN(50)
064300940224     C*
064400950726     C* definizione indicatori su video
064500951011     C                   SetOff                                       020405
064600970113     C                   SetOff                                       060710
064700020221C1530x* ex C1408         SetOff                                       11
064800020221C1530C                   SetOff                                       1112
064900040616C1579C                   eval      *in13 = *off
065000040616C1767C                   eval      *in14 = *off
065100110119C2179C                   eval      *in15 = *off
065200960513     C*
065300960806     C* se ci sono i dft
065400970113   1 C                   IF        CiSonoDft = *on
065500960806     C* preseleziono le funzioni richieste
065600970113   2 C                   IF        DFTPPA = *on      and
065700960806     C                             DocIVA039 = '1'
065800960806     C                   EVAL      D1B01 = 1
065900970113   2eC                   ENDIF
066000970113   2 C                   IF        DFTMix = *on      and
066100960806     C                             DocIVA039 = '1'
066200960806     C                   EVAL      D1B02 = 1
066300970113   2eC                   ENDIF
066400970113   2 C                   IF        DFTDesAgg = *on
066500960806     C                   EVAL      D1B03 = 1
066600970113   2eC                   ENDIF
066700970113   1eC                   ENDIF
066800960806     C*
066900000810   1 C                   If        DocIva039 = *on
067000000810     C                   SETON                                        02
067100970113   1eC                   Endif
067200960513     C*
067300060202D1938C* RITENUTE: se non sto gestendo un modello
067400060202  "  C                   If        Ctb039 <> 'MD'
067500060202  "  c                   exsr      BotRitDac
067600060202D1938C                   Endif
067700960513     C*
067800060202D1938C* INTRA   : se non sto gestendo un modello
067900060206  "  c                   If        Ctb039 <> 'MD'
068000060202  "  c                   exsr      BotIntra
068100060202D1938C                   Endif
068200060202     c*
068300060202D1938C* MIX DI PARTITA
068400060202D1938c                   exsr      BotMix
068500060202     c*
068600060202     C* Messaggio per generazione autofattura
068700000810     C                   SETOFF                                       8099
068800970113   1 C                   If        Pgm039 = *blank and AftAcq039 = *on
068900000810     C                   SETON                                        8099
069000970113   1eC                   Endif
069100960513     C*
069200060202D1938C* SALVA COME MODELLO
069300060202D1938c                   exsr      BotSalva
069400960514     C*
069500060202D1938C* CESPITI : se non sto gestendo un modello
069600060202  "  C                   If        Ctb039 <> 'MD'
069700060202  "  C                   exsr      BotCespiti
069800060202D1938C                   Endif
069900970113     C*
070000060202D1938C* P.N. COLLEGATA
070100060202D1938C                   exsr      BotPNColl
070200950726     C*
070300060202C1767C* RATEI E RISCONTI : se non sto gestendo un modello
070400060202D1938C                   If        Ctb039 <> 'MD'
070500060202  "  C                   exsr      BotRatei
070600060202D1938C                   Endif
070700060202     c*
070800000825C1408C* P.N. INTERCOMPANY
070900060206D1938C                   If        Ctb039 <> 'MD'
071000060206  "  C                   exsr      BotPNInter
071100060206D1938C                   Endif
071200060202     c*
071300060202C1530C* LAGUNA : se non sto gestendo un modello
071400060202D1938C                   If        Ctb039 <> 'MD'
071500060202  "  c                   exsr      BotLaguna
071600060202D1938C                   endif
071700060202     C*
071800060202C1579C* C/CORRENTI DI POOL
071900060206D1938C                   If        Ctb039 <> 'MD'
072000060206  "  c                   exsr      BotCCPool
072100060206D1938C                   endif
072200110119C2179C*
072300110119  "  C* Legami registrazioni
072400110119C2179C                   Exsr      BotLegReg
072500110119     C*
072600940117     C                   ENDSR
072700940207     C/EJECT
072800060202D1938C************************************************************
072900060202  "   * Test per bottone ritenute d'acconto
073000060202D1938C************************************************************
073100060202     C     BotRitDac     BEGSR
073200060202     C*
073300110518D2696C* se causale ricevuta <>
073400110518  "  C                   If        OpeCausale <> Causale039
073500110518  "  C* rifaccio il reperimento
073600110518  "  C                   Exsr      RepDatCaus
073700110518  "  C                   endif
073800110518D2696C*
073900060202   1 C                   If        FlgPerc039 = *on
074000060202C1741C* oppure si tratta di un pagamento con ritenuta
074100060202C1741C                             or OkRitPagam = *on
074200110518D2696C                   If        OpeTpDocI <> '8'
074300060202     C                   SETON                                        04
074400060202     C                   Eval      D1B06 = 1
074500110518D2696C                   Endif
074600060202   1eC                   Endif
074700060202     C*
074800060202D1938C                   ENDSR
074900060202D1938C************************************************************
075000060202  "   * Test per bottone intra
075100060202D1938C************************************************************
075200060202     C     BotIntra      BEGSR
075300060202     C*
075400060202   1 C                   If        FlgIntr039 = *on
075500060202     C                   SETON                                        05
075600060202     C* reperisco stato su G12
075700060202     C                   Exsr      RepStato
075800060202     C                   Eval      D1B07 = 1
075900060202   1eC                   Endif
076000060202     C*
076100060202D1938C                   ENDSR
076200060202D1938C************************************************************
076300060202  "   * Test per bottone Salva come modello
076400060202D1938C************************************************************
076500060202     C     BotSalva      BEGSR
076600060202     C*
076700060202     C* se richiamato dalla PN insoluti
076800060202   1 C                   If        Pgm039 = 'NDC054R'
076900060202c1801c* o se richiamato dai modelli in copia
077000060202C1801C                             or %subst(flags039:2:1) = *on
077100060202     C* non mostro il bottone di salvataggio come modello
077200060202     C                   SETON                                        06
077300060202   1eC                   Endif
077400060202     C*
077500060202D1938C                   ENDSR
077600060202D1938C************************************************************
077700060202  "   * Test per bottone cespiti
077800060202D1938C************************************************************
077900060202     C     BotCespiti    BEGSR
078000060202     C*
078100060202     C* se esiste almeno un conto cespite
078200060202   1 C                   If        Cespite039 = *on
078300060202     C* mostro il bottone relativo
078400060202     C                   SETON                                        07
078500060202     C* lo preseleziono
078600060202     C                   Eval      D1B09 = 1
078700060202   1eC                   Endif
078800060202     C*
078900060202D1938C                   ENDSR
079000060202D1938C************************************************************
079100060202  "   * Test per bottone PNOTA collegata
079200060202D1938C************************************************************
079300060202     C     BotPNColl     BEGSR
079400060202     c*
079500060202     C* verifico se nella causale esiste l'indicazione di effettuare
079600060202     C* una registrazione collegata di storno
079700060202     C* Eseguo tutto ciò se dal pgm chiamante arriva l'indicazione di
079800060202     C* gestire la PN COLLEGATA (lo dice il flag PnColl039 = *on che è
079900060202     c* lo stesso flag in uscita se richiesto il bottone di Pn collegata)
080000060202     C*
080100060202     C                   EVAL      SiPnColl = *off
080200060202     C*
080300060202   1 c                   if        PnColl039 = *on
080400060202     c* controllo se esistono registrazioni collegate
080500060202     C* comunque
080600060202     C                   Exsr      RepRegColl
080700060202     c* se esiste, passo indicazione di emettere il bottone
080800060202   2 c                   if        Err029 = *off
080900060202     c                   eval      SiPnColl = *on
081000060202     C* lo preseleziono visto che esiste
081100060202     C                   Eval      D1B10 = 1
081200060202   2ec                   endif
081300060202     c*
081400060202   2 c                   If        SiPncoll = *off
081500060202     c* Se non esistono registrazioni collegate, vedo se si devono fare
081600060202     c* dalla causale
081700060202     c                   exsr      RepDatCaus
081800060202     c* Se esiste l'indicazione, controlli  i dati da tabella
081900060202   3 C                   If        OpeTab <> *Blanks
082000060202     C                   Exsr      RepPNColl
082100060202     c* se esiste, passo indicazione di emettere il bottone
082200060202   4 c                   if        Err029 = *off
082300060202     c                   eval      SiPnColl = *on
082400060202     C* lo preseleziono solo se sono in immissione
082500060202     c                   if        opz039 = '01'
082600060202     C                   Eval      D1B10 = 1
082700060202   4 c                   endif
082800060202   4 c                   endif
082900060202   3ec                   endif
083000060202   2ec                   endif
083100060202     C*
083200060202     c* Se si verificano uno di questi due casi,
083300060202     c* mostro il bottone relativo preselezionato
083400060202     c*
083500060202   2 c                   if        SiPnColl = *on
083600060202     C* se ok, mostro il bottone relativo
083700060202     C                   SETON                                        10        PR campi key
083800060202   2eC                   Endif
083900060202   1eC                   Endif
084000060202     C*
084100060202D1938C                   ENDSR
084200060202D1938C************************************************************
084300060202  "   * Test per bottone Mix di partita
084400060202D1938C************************************************************
084500060202     C     BotMix        BEGSR
084600060202     C*
084700060202     C                   If        Ctb039 =  'MD'
084800060202     C                   seton                                        09
084900060202     c                   else
085000060202     c                   setoff                                       09
085100060202     c                   endif
085200060202     C*
085300060202D1938C                   ENDSR
085400060202D1938C************************************************************
085500060202  "   * Test per bottone ratei & risconti
085600060202D1938C************************************************************
085700060202     C     BotRatei      BEGSR
085800060202     C*
085900060202C1767c* verifico se ci sono codici ratei di parcheggio nella registraz.
086000060202  "  C                   exsr      CtrlRatei
086100060202  "  c                   if        Esiste326 <>  *off
086200060202  "  c* se esistono, propongo il bottone
086300060202  "  C                   SETON                                        13        PR campi key
086400060202  "  c* preselezionato se parcheggio
086500060202  "  c                   if        Esiste326='P'
086600060202  "  C                   Eval      D1B13 = 1
086700060202  "  C                   Endif
086800060202C1767C                   Endif
086900060202     C*
087000060202D1938C                   ENDSR
087100060202D1938C************************************************************
087200060202  "   * Test per pnota Intercompany
087300060202D1938C************************************************************
087400060202     C     BotPNInter    BEGSR
087500060202     C*
087600060202C1408C*
087700060728C1408C                   If        $ElaPnI <> *Off
087800060728C1954 *
087900060728C1954X*ex C1408 ******** Select
088000060728  "  X*ex C1408 ---------------------
088100060728  "  X*ex C1408 1) "check automatico" --> eseguo operazioni iniziali
088200060728  "  X*ex C1408 ---------------------
088300060728C1954X*ex C1408 ******** When      $ElaPnI = '1'
088400060728C1954 *
088500060728C1954 * eseguo operazioni iniziali
088600060728C1408C                   ExSr      InzPnInter
088700060728C1954 *
088800060728C1408C* se esistono le condizioni per l'esecuzione della p.n. intercompany
088900060202  "  C*  visualizzo il bottone già "cliccato"
089000060202C1408C                   If        $ChkPnI = *On
089100060202C1530x***                SetOn                                        11
089200060202C1530x***                Eval      D1B11 = 1
089300060202C1579X**** ex C1530      SetOn                                        12
089400060202C1579X**** ex C1530      Eval      D1B12 = 1
089500060202C1767x**** ex C1579      eval      *in13 = *on
089600060202  "  X**** ex C1579      Eval      D1B13 = 1
089700060728C1767C                   eval      *in14 = *on
089800060728C1954 *
089900060728  "   * ---------------------
090000061215C1954 * 1) "check automatico": visualizzo il bottone già "cliccato"
090100061215D2100X**** ex C1954*visualizzo il bottone già "cliccato"
090200061215C1954 * 2) "check manuale"   : visualizzo il bottone non "cliccato"    e a
090300061215D2100X**** ex C1954*visualizzo il bottone già "cliccato"
090400060728C1954C                   if        $ElaPnI = '1'
090500060202C1767C                   Eval      D1B14 = 1
090600060728C1954C                   endif
090700060728C1954 *
090800060202C1408C                   EndIf
090900060728C1954X*ex C1408 ------------------
091000060728  "  X*ex C1408 2) "check manuale" --> visualizzo bottone e basta, sarà l'utente a
091100060728  "  X*ex C1408 ------------------     eseguire le operazioni iniziali "cliccandolo"
091200060728C1954X*ex C1408 ******** When      $ElaPnI = '2'
091300060202C1530x***                SetOn                                        11
091400060202C1579X**** ex C1530      SetOn                                        12
091500060202C1767x**   ex C1579      eval      *in13 = *on
091600060728C1954X*ex C1767 ******** eval      *in14 = *on
091700060728C1954X*ex C1408 ******** EndSl
091800060728C1408C                   EndIf
091900060202C1408C*
092000060202     C*
092100060202D1938C                   ENDSR
092200060202D1938C************************************************************
092300060202  "   * Test per Lagune
092400060202D1938C************************************************************
092500060202     C     BotLaguna     BEGSR
092600060202     C*
092700060202C1530C* se Laguna è attivo
092800060202  "  C                   IF        xscLaguna = *on
092900060202  "  C* se la registrazione è di tipo IVA
093000060202  "  C                             and DocIVA039 = '1'
093100060202  "  C* se l'opzione d'ingresso è quella di modifica
093200060202  "  C                             and ( Opz039 = '02' or
093300060202  "  C* oppure quella diretta da lista registrazioni
093400060202  "  C                                   Opz039 = '  ')
093500060202  "  C* visualizzo bottone Laguna
093600060202  "  C                   SetOn                                        11
093700060202  "  C* controllo se sono cambiati i dati di catalogazione
093800060202  "  C                   EXSR      CtrlDatiLgn
093900060202C1530C* se sono cambiate le chiavi documentali
094000060202D1901C* oppure sono cambiate i descrittori
094100060202  "  C                   IF        (KeyLgnModif = *on
094200060202  "  C                             or DesLgnModif = *on)
094300060202  "  C* e c'è un documento catalogato o per la chiave precedente o
094400060202  "  C* quella attuale
094500060202D1901C                             and DocCatalogato = *on
094600060202D1901x* ex C1530         IF        KeyLgnModif = *on
094700060202D1901x* oppurte se sono cambiate i descrittori
094800060202D1901x* ex D1270                   or DesLgnModif = *on
094900060202C1530C* preseleziono il bottone
095000060202  "  C                   Eval      D1B11 = 1
095100060202  "  C                   EndIf
095200060202  "  C                   EndIf
095300060202C1530C*
095400060202     C*
095500060202D1938C                   ENDSR
095600110119C2179C************************************************************
095700110119  "   * Test per Legami registrazioni
095800110119  "  C************************************************************
095900110119C2179C     BotLegReg     BEGSR
096000110308     C* se causale ricevuta <>
096100110308     C                   If        OpeCausale <> Causale039
096200110308     C* rifaccio il reperimento
096300110308     C                   Exsr      RepDatCaus
096400110308     c                   endif
096500110119     C                   If        %SubSt(OpeFlg:1:1) = *On
096600110119     C                   Eval      *in15 = *on
096700110119     C                   Eval      D1B15 = 1
096800110119     C                   EndIf
096900110119     C*
097000110119C2179C                   ENDSR
097100060202D1938C************************************************************
097200060202  "   * Test per bottone CCPool
097300060202D1938C************************************************************
097400060202     C     BotCCPool     BEGSR
097500060202C1579C*
097600060202  "  C* verifico esistenza del raccordo tra movimento e
097700060202  "  C* registrazione di giroconto
097800060202  "  C                   ExSr      CtrNDCPR
097900060202  "  C                   if        §Errore     = '0'
098000060202  "  C* reperisco i dati passati
098100060202  "  C                   eval      %Subst(NDCPRDS:1:§Lunghezza)  =
098200060202  "  C                             %Subst(§Dati:1:§Lunghezza)
098300060202  "  C* visualizzo bottone
098400060202  "  C                   eval      *in12 = *on
098500060202  "  C* preseleziono il bottone
098600060202  "  C                   Eval      D1B12 = 1
098700060202  "  C                   endif
098800060202C1579C*
098900060202D1938C                   ENDSR
099000021211C1579C************************************************************
099100021211  "   * Verifico esistenza raccordo tra
099200021211  "   * registrazioni giroconto per c/c pool
099300021211  "  C************************************************************
099400021211  "  C     CtrNDCPR      BEGSR
099500021211  "  C*
099600021211  "  C                   Eval      §Lunghezza   = %Size(NDCPRDS)
099700021211  "  C                   CALLB     'PJC00701R'
099800021211  "  C                   parm      '01'          §Opzione                       1
099900021211  "  C                   parm      Societa039    §Societa                       7
100000021211  "  C                   parm      Sys039        §SysReg                        7
100100021211  "  C                   parm      NrAsReg039    §NrAsReg                       7
100200021211  "  C                   parm      0             §NrRigaM                       7
100300021211  "  C                   parm      *Blank        §RifInt                        7
100400021211  "  C                   parm      0             §SubNum                        7
100500021211  "  C                   parm      *Off          §Errore                        2
100600021211  "  C                   parm                    §Dati                          3
100700021211  "  C                   parm                    §Lunghezza                     4
100800021211  "  C                   parm      '0'           §Commit                        6
100900021211  "  C*
101000021211C1579C     XCtrNDCPR     ENDSR
101100000825C1408C************************************************************
101200000825  "  C* Operazioni iniziali p.n. intercompany
101300000825  "  C************************************************************
101400000825C1408C     InzPnInter    BEGSR
101500000825     C*
101600000825     C                   Eval      $ChkPnI = *Off
101700041021C1761 * Chiamo pgm che esegue le operazioni:
101800041021  "   * - Verifica esistenza condizioni per esecuzione p.n. interc.
101900041021  "   * - Verifica se esiste il raccordo reg. intercompany
102000041021  "   * - Se non esistono (più) le condizioni ma esiste il raccordo
102100041021  "   *   cancello il raccordo
102200041021  "  C                   reset                   NDC487DS
102300041021  "   *
102400041021  "  C                   eval      Societa487 =  XScSoc
102500041021  "  C                   eval      SocProv487 =  Societa039
102600041021  "  C                   eval      Sys487 =  REGSys
102700041021  "  C                   eval      NrAsReg487 =  REGNrAsReg
102800041021  "   *     a registrazione di tipo IVA/NonIva
102900041021  "  C                   Eval      DocIva487 = DocIva039
103000041021  "  C                   eval      Unita487   =  REGUnita
103100041021  "  C                   movel     NDC487DS      Kpjbu
103200041021  "  C                   call      'NDC487R'
103300041021  "  C                   parm                    Kpjba
103400041021  "  C                   parm                    Ndc299DS
103500041021  "  C                   parm                    NDBHMParm
103600041021  "  C                   movel     Kpjbu         NDC487DS
103700041021  "   *
103800041026  "  C                   if        ErrPro487 <> '9'
103900041021  "  C                   eval      $ChkPnI = *On
104000041021  "  C                   endif
104100041021C1761 *
104200041021C1761X** ex C1408 **     Eval      $ChkRacRI = *Off
104300041021  "  X** ex C1408 **     Eval      $ForzCol = *Off
104400041021  "  X** ex C1408 **
104500041021  "  X** ex C1408 **Verifica esistenza condizioni per esecuzione p.n. intercompany
104600041021  "  X** ex C1408 **     ExSr      ChkPnInter
104700041021  "  X** ex C1408 **
104800041021  "  X** ex C1408 **Verifica se esiste il raccordo reg. intercompany
104900041021  "  X** ex C1408 **     ExSr      ChkRacRI
105000041021  "  X** ex C1408 **
105100041021  "  X** ex C1408 **Se non esistono (più) le condizioni ma esiste il raccordo ...
105200041021  "  X** ex C1408 **     If        $ChkPnI = *Off and
105300041021  "  X** ex C1408 **               $ChkRacRI = *On and $ForzCol <> *On
105400041021  "  X** ex C1408 ** ... cancello il raccordo !!!
105500041021  "  X** ex C1408 **     ExSr      DelRacRI
105600041021  "  X** ex C1408 **     EndIf
105700041021C1761X** ex C1408 **
105800000825C1408C                   ENDSR
105900041021C1761X** ex C1408 ************************************************
106000041021  "  X** ex C1408 **Controlla esistenza "condizioni" intercompany
106100041021  "  X** ex C1408 ************************************************
106200041021  "  X** ex C1408 **ChkPnInter    BEGSR
106300041021  "  X** ex C1408 **
106400041021  "  X** ex C1408 **     Eval      LenNDBHM = %Size(NDBHMDS)
106500041021  "  X** ex C1408 **
106600041021  "  X** ex C1408 **     If        C0501ChkCI(REGSys: REGNrAsReg: '0':
106700041021  "  X** ex C1408 **                          LenNDBHM: NDBHMDS: NrRigaM:
106800041021  "  X** ex C1408 **                          Segno: Partite) = 1
106900041021  "  X** ex C1408 **     Eval      $ChkPnI = *On
107000041021  "  X** ex C1408 **     Else
107100041021  "  X** ex C1408 **     Eval      $ChkPnI = *Off
107200041021  "  X** ex C1408 **     EndIf
107300041021  "  X** ex C1408 **
107400041021C1761X** ex C1408 **     ENDSR
107500041021C1761X** ex C1408 ************************************************
107600041021  "  X** ex C1408 **Controlla esistenza raccordo registrazioni interc.
107700041021  "  X** ex C1408 ************************************************
107800041021  "  X** ex C1408 **ChkRacRI      BEGSR
107900041021  "  X** ex C1408 **
108000041021  "  X** ex C1408 **     If        C0502RacRI('99': REGSocieta:
108100041021  "  X** ex C1408 **                          REGSys: REGNrAsReg: SocDes:
108200041021  "  X** ex C1408 **                          SysDes: NrAsDes: $ForzCol) = 1
108300041021  "  X** ex C1408 **     Eval      $ChkRacRI = *On
108400041021  "  C* se forzato (per registrazione mancante) è come se non esistessero
108500041021  "  C*  le condizioni intercompany ...
108600041021  "  X** ex C1408 **     If        $ForzCol = *On
108700041021  "  X** ex C1408 **               and SocDes = *Blank
108800041021  "  X** ex C1408 **     Eval      $ChkPnI = *Off
108900041021  "  X** ex C1408 **     EndIf
109000041021  "  X** ex C1408 **     Else
109100041021  "  X** ex C1408 **     Eval      $ChkRacRI = *Off
109200041021  "  X** ex C1408 **     EndIf
109300041021  "  X** ex C1408 **
109400041021C1761X** ex C1408 **     ENDSR
109500041021C1761X** ex C1408 ************************************************
109600041021  "  X** ex C1408 **Cancellazione raccordo registrazioni intercompany
109700041021  "  X** ex C1408 ************************************************
109800041021  "  X** ex C1408 **DelRacRI      BEGSR
109900041021  "  X** ex C1408 **
110000041021  "  X** ex C1408 **     CallP     C0502RacRI('04': REGSocieta:
110100041021  "  X** ex C1408 **                          REGSys: REGNrAsReg)
110200041021  "  X** ex C1408 **
110300041021C1761X** ex C1408 **     ENDSR
110400000824C1408C************************************************************
110500000901  "  C* Esecuzione p.n. intercompany
110600000824  "  C************************************************************
110700000824C1408C     RunPnInter    BEGSR
110800041021C1761 *
110900041021  "   * Chiamo pgm che esegue le operazioni:
111000041021  "   * - Verifica esistenza condizioni per esecuzione p.n. interc.
111100041021  "   * - Verifica se esiste il raccordo reg. intercompany
111200041021  "   * - Se non esistono (più) le condizioni ma esiste il raccordo
111300041021  "   *   cancello il raccordo
111400041021  "   * - Se esistono le condizioni, esegue p.n. interc.
111500041021  "   *
111600041021  "  C                   reset                   NDC487DS
111700041021  "  C                   eval      Societa487 =  XScSoc
111800041021  "  C                   eval      SocProv487 =  Societa039
111900041021  "  C                   eval      Sys487 =  REGSys
112000041021  "  C                   eval      NrAsReg487 =  REGNrAsReg
112100041021  "  C                   eval      Unita487   =  REGUnita
112200041021  "   *     a registrazione di tipo IVA/NonIva
112300041021  "  C                   eval      DocIva487 = DocIva039
112400041021  "  C                   movel     NDC487DS      Kpjbu
112500041021  "   *
112600041021  "  C                   call      'NDC487R1'
112700041021  "  C                   parm                    Kpjba
112800041021  "  C                   movel     Kpjbu         Ndc487DS
112900041021  "   *
113000041021  "  C                   Eval      Err039 = Err487
113100041021C1761 *
113200041021C1761X** ex C1408 **
113300041021  "  X** ex C1408 **     Reset                   NDC299DS
113400041021  "  X** ex C1408 **     Eval      SocPro299 = Societa039
113500041021  "  X** ex C1408 **     Eval      SysPro299 = REGSys
113600041021  "  X** ex C1408 **     Eval      NrAsPro299 = REGNrAsReg
113700041021  "  X** ex C1408 **     Eval      NrRMPro299 = NrRigaM
113800041021  "  X** ex C1408 **     Eval      SegPro299 = Segno
113900041021  "  X** ex C1408 **     Eval      PartPro299 = Partite
114000041021  "  X** ex C1408 **     Eval      UniPro299 = REGUnita
114100041021  "  X** ex C1408 **     Eval      SocDes299 = BHMSocieta
114200041021  "  X** ex C1408 **     Select
114300041021  "  X** ex C1408 **modifica reg. intercompany
114400041021  "  X** ex C1408 **     When      $ChkRacRI = *On
114500041021  "  X** ex C1408 **     Eval      Opz299 = '02'
114600041021  "  X** ex C1408 **     Eval      SysDes299 = SysDes
114700041021  "  X** ex C1408 **     Eval      NrAsDes299 = NrAsDes
114800041021  "  X** ex C1408 **immissione reg. intercompany
114900041021  "  X** ex C1408 **     Other
115000041021  "  X** ex C1408 **     Eval      Opz299 = '01'
115100041021  "  X** ex C1408 **     EndSl
115200041021  "  X** ex C1408 **
115300041021  "  X** ex C1408 **     MoveL     NDC299DS      KPJBU
115400041021  "  X** ex C1408 **
115500041021  "  X** ex C1408 **     Call      'NDC299R1'
115600041021  "  X** ex C1408 **     Parm                    KPJBA
115700041021  "  X** ex C1408 **     Parm                    NDBHMDS
115800041021  "  X** ex C1408 **
115900041021  "  X** ex C1408 **     MoveL     KPJBU         NDC299DS
116000041021  "  X** ex C1408 **
116100041021  "  X** ex C1408 **     Eval      Err039 = Err299
116200041021C1761X** ex C1408 **
116300000824C1408C                   ENDSR
116400040616C1767C************************************************************
116500040616  "  C* Controlla esistenza ratei nella registrazione
116600040616  "  C************************************************************
116700040616C1767C     CtrlRatei     BEGSR
116800040616     C*
116900040616     c                   reset                   ndc326ds
117000040616     c                   eval      Societa326=societa039
117100040616     c                   eval      Ctb326 = Ctb039
117200040616     c                   move      DtReg039      Dtreg326
117300040616     c                   eval      Sys326=Sys039
117400040616     c                   eval      Nrasreg326=Nrasreg039
117500040616     c                   eval      Opz326='01'
117600040616     c                   eval      kpjbu=ndc326ds
117700040616     c                   call      'NDC326R'
117800040616     C                   PARM                    kpjba
117900040616     c                   eval      ndc326ds=kpjbu
118000040616     C*
118100040616C1767C                   ENDSR
118200040616C1767C************************************************************
118300040616  "  C* Chiamata al pgm di gestione ratei e risconti
118400040616C1767c************************************************************
118500040616     C     Call327R      BEGSR
118600040616     c*
118700040616    >C                   RESET                   NDC327DS
118800040616    >C                   MOVEL     '02'          Opz327
118900040616    >C                   MOVEL     Sys039        Sys327
119000040616    >C                   MOVEL     NrAsReg039    NrAsReg327
119100040616    >C                   MOVEL     NDC327DS      KPJBU
119200040616    >C                   CALL      'NDC327R'
119300040616     C                   PARM                    KPJBA
119400040616     C*
119500040616C1767C                   ENDSR
119600940207     C************************************************************
119700940510     C* RIEMPIMENTO VIDEATA  D1
119800940207     C************************************************************
119900940207     C     RIED1         BEGSR
120000940207     C*
120100950725     C* impostare i campi della videata dai campi
120200950301     C*
120300950920     C     DtReg039      IfEQ      DatMin
120400950725     C                   Movel     *zero         D1DtReg
120500950725     C                   Else
120600950918     C                   CALLb     'XDT4000'
120700950915     C                   PARM      DtReg039      XDTAMG           10
120800950915     C     D1DtReg       PARM                    XDTGMA            6 0
120900951019     C                   PARM      2             XDTSTA            1 0
121000950725     C                   EndIf
121100950920     C     DtPar039      IfEQ      DatMin
121200950725     C                   Movel     *zero         D1DtPar
121300950725     C                   Else
121400950918     C                   CALLb     'XDT4000'
121500950915     C                   PARM      DtPar039      XDTAMG           10
121600950915     C     D1DtPar       PARM                    XDTGMA            6 0
121700950915     C                   PARM      2             XDTSTA            1 0
121800950725     C                   EndIf
121900950920     C     DtDoc039      IfEQ      DatMin
122000950725     C                   Movel     *zero         D1DtDoc
122100950725     C                   Else
122200950918     C                   CALLb     'XDT4000'
122300950915     C                   PARM      DtDoc039      XDTAMG           10
122400950915     C     D1DtDoc       PARM                    XDTGMA            6 0
122500950915     C                   PARM      2             XDTSTA            1 0
122600950725     C                   EndIf
122700950816     C                   Eval      D1Snatura  =  SNATURA039
122800950816     C                   Eval      D1Soggetto =  SOGGETT039
122900950816     C                   Eval      D1Causale  =  Causale039
123000950816     C                   Eval      D1SoggD    =  SOGGD039
123100950816     C                   Eval      D1CausD    =  CAUSD039
123200950816     C                   Eval      D1NrReg    =  NRREG039
123300950816     C                   Eval      D1SerReg   =  SERREG039
123400950816     C                   Eval      D1NrDoc    =  NRDOC039
123500950816     C                   Eval      D1SerDoc   =  SERDOC039
123600970226     C                   Eval      D1TpRegz   =  REGTpRegz
123700950816     C                   Eval      D1NrPar    =  NRPAR039
123800950816     C                   Eval      D1SerPar   =  SERPAR039
123900950816     C                   Eval      D1Divisa   =  DIVISA039
124000950816     C                   EVal      D1Importo  =  IMPORTO039
124100950816     C                   Eval      D1ImportD  =  IMPortD039
124200070801C2049C* se presente registrazione collegata, emetto estremi
124300070801  "  c                   if        RegNrAsCol <> 0
124400070801  "  c                   seton                                        03
124500070801  "  C* Salvo £ndreg
124600070801  "  C                   Movel     £NDREG        NdREgsav
124700070801  "  C     K02Reg01      chain     NdReg01L
124800070801  "  c                   if        %found(ndreg01L)
124900070801  "  c                   move      RegDtReg      Wdataiso
125000070801  "  c     *jobrun       move      Wdataiso      D1DtRegCol
125100070801  "  C                   Eval      D1NrRegCol =  RegNrReg
125200070801  "  C                   Eval      D1SerRegCo =  RegSerReg
125300070801  "  C                   Eval      D1TpRegZCo =  RegTpRegz
125400070801  "  c                   endif
125500070801  "  C* riimposto £ndreg
125600070801  "  C                   Movel     NdRegSav      £NDREG
125700070801C2049c                   endif
125800940207     C*
125900940207     C                   ENDSR
126000940207     C/EJECT
126100940131     C************************************************************
126200940207     C* CONTROLLO VIDEATA
126300950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
126400950613     C* 95 96 97 98
126500940131     C************************************************************
126600940127     C     CTRD1         BEGSR
126700940131     C*
126800940127     C                   SETOFF                                       99
126900940127     C* non imposto pos. del cursore
127000940127     C                   Z-ADD     0             H1RIGA
127100940127     C                   Z-ADD     0             H1COLO
127200000810     C*
127300950731     C* controllo stato bottoni :
127400000810     C*
127500950731     C* 1 = Anagrafica partite
127600950731     C                   if        d1b01 = 1
127700960515    >C                   CLEAR                   KPJBU
127800951019     C                   if        DtPar039 <> datmin
127900951019    >C                   MOVEL     ctb039        ctb061
128000040401     C                   MOVEL     '02'          Opz061
128100951019    >C                   MOVEL     Kcc039        KCC061
128200951019    >C                   MOVEL     Soggett039    KSC061
128300951019     C                   move      DtPar039      dtpar061
128400951019     C                   move      nrpar039      nrpar061
128500951019     C                   move      serpar039     serpar061
128600951019    >C                   MOVEL     Snatura039    snatura61
128700040401D1573c                   MOVEL     TabPnot039    TabPnot061
128800040402D1573c                   MOVEL     DocIva039     DocIva061
128900951019    >C                   MOVEL     NDC061DS      KPJBU
129000951019    >C                   CALL      'NDC061R'
129100951019     C                   PARM                    KPJBA
129200951019     C                   else
129300951019    >C                   Reset                   NDC060DS
129400951019     C                   Eval      Opz060  = '02'
129500951019     C                   Eval      Ctb060    = Ctb039
129600951019     C                   Eval      Unita060  = Unita039
129700951019     C                   Eval      Kcc060    = Kcc039
129800951019     C                   Eval      Ksc060    = Soggett039
129900951019     C                   Eval      NrPar060  = NrPar039
130000951019     C                   Eval      DtPar060  = DtPar039
130100951019     C                   Eval      SerPar060 = SerPar039
130200951019     C                   Movel     NDC060DS      KPJBU
130300951019     C                   Call      'NDC060R'
130400951019     C                   Parm                    KPJBA
130500951019     C                   endif
130600951019     C                   endif
130700000810     C*
130800950731     C* 2 = Mix partita
130900950731     C                   if        d1b02 = 1
131000960515    >C                   CLEAR                   KPJBU
131100950731    >C                   Reset                   NDC065DS
131200950731     C                   Eval      Opz065  = '02'
131300950921     C                   Eval      Ctb065    = Ctb039
131400950921     C                   Eval      Unita065  = Unita039
131500950816     C                   Eval      Kcc065  = Kcc039
131600950816     C                   Eval      Ksc065  = Soggett039
131700950921     C                   Eval      NrPar065  = NrPar039
131800950921     C                   Eval      DtPar065  = DtPar039
131900950921     C                   Eval      SerPar065 = SerPar039
132000950921     C                   Movel     NDC065DS      KPJBU
132100950921     C                   Call      'NDC065R'
132200950731     C                   Parm                    KPJBA
132300950731     c                   endif
132400000810     C*
132500950731     C* 3 = Descrizione aggiuntiva
132600950731     C                   if        d1b03 = 1
132700951030     C                   RESET                   NDC027DS
132800951030     C                   MOVEL     '02'          OPZ027
132900951030     C                   MOVEL     Sys039        Sys027
133000951030     C                   MOVEL     NrAsReg039    NrAsReg027
133100951030     C                   MOVEL     'T'           TipDes027
133200951030     C* Il commit lo deve eseguire il pgm stesso
133300951030     C                   MOVE      '0'           Cmt027
133400951030     C* avverto il pgm di inizializzarsi
133500951030     C                   MOVEL     *on           Inz027
133600951030     C* passo tutti quei campi che possono servire al pgm
133700951030     C                   MOVEL     NDC027DS      KPJBU
133800951030     C                   CALL      'NDC027R'
133900951030     C                   PARM                    KPJBA
134000951030     C                   MOVEL     KPJBU         NDC027DS
134100950731     c                   endif
134200000810     C*
134300950731     C* 4 = Salva come modello
134400950731     C                   if        d1b04 = 1
134500970612     C* Salvo £ndreg (perchè si sporca con i modelli)
134600970612     C                   Movel     £NDREG        NdREgsav
134700950731     C                   Exsr      SalvaModPN
134800970612     C* riimposto £ndreg
134900970612     C                   Movel     NdRegSav      £NDREG
135000950731     c                   endif
135100000810     C*
135200950921     C* 5 = Interrogazioni
135300950921     C                   if        d1b05 = 1
135400960515    >C                   CLEAR                   KPJBU
135500950921    >C                   Reset                   ANA399DS
135600950921     C                   Eval      A39Ctb   = Ctb039
135700950921     C                   Eval      A39Unita = Unita039
135800951013     C                   Eval      A39Societa= Societa039
135900950921     C                   Eval      A39Kcc   = Kcc039
136000950921     C                   Eval      A39Ksc   = Soggett039
136100950921     C                   Eval      A39Snatura= SNatura039
136200950921     C                   Movel     ANA399DS      KPJBU
136300950921     C                   Call      'ANA399R'
136400950921     C                   Parm                    KPJBA
136500950921     C                   endif
136600961209     c*
136700951011     C* 6 = Ritenute d'aaconto
136800951005     C                   if        d1b06 = 1
136900040325C1741C*se non si tratta di pagamento, ma di fattura
137000040325C1741C                   if        OkRitPagam = *off
137100961209     c* vedo se esiste un solo movimento su ndmra o + di uno, per chiamare
137200961209     C* o il pgm di manutenzione o la guida
137300961209     c                   reset                   NDCR04DS
137400961209     c                   eval      NasRegFr04 = NrAsReg039
137500961209     C                   Eval      SYSFR04    =  Sys039
137600961209     C                   Eval      SocietaR04 =  Societa039
137700961209     c                   eval      KPJBU = Ndcr04ds
137800961209     c                   call      'NDCR04R'
137900961209     c                   parm                    KPJBA
138000961209     C                   movel     kpjbu         NDCR04DS
138100961209     c* Ritorno
138200961209     C                   Select
138300961210     C* se presente un solo movimento o nessun movimento
138400961210     c                   when      NrrR04 = 1 or nrrR04 = 0
138500960515    >C                   CLEAR                   KPJBU
138600951005    >C                   Reset                   NDCR05DS1
138700951005     C                   Eval      NASRGFR105 =  NrAsReg039
138800951005     C                   Eval      SYSFR105   =  Sys039
138900961210     c                   if        NrrR04 = 1
139000961209     C                   Eval      STAR105    =  staR04
139100961210     c                   else
139200961210     C                   Eval      STAR105    =  '0'
139300961210     c                   endif
139400951005     C                   Eval      KCCR105    =  Kcc039
139500951005     C                   Eval      KSCR105    =  Soggett039
139600951005     C                   Eval      SERREGR105 =  SerReg039
139700951005     C                   Eval      NRGR105    =  NrReg039
139800951005     C                   Eval      DRGR105    =  DtReg039
139900951005     C                   Eval      NDCR105    =  NrDoc039
140000951005     C                   Eval      DDCR105    =  DtDoc039
140100951005     C                   Eval      SERDOCR105 =  SerDoc039
140200951005     C                   Eval      NRPR105    =  NrPar039
140300951005     C                   Eval      DRPR105    =  DtPar039
140400951005     C                   Eval      SERPARR105 =  SerPar039
140500951005     C                   Eval      CODR105    =  Causale039
140600960410     C                   Eval      TOTDOCR105 =  Importo039
140700981015     C                   Eval      TOTDODR105 =  ImportD039
140800981016     C                   Eval      cambioR105 =  Cambio039
140900960410     C* i valori negativi (provenienti da note accredito/addebito) vanno
141000960410     C* passati positivi
141100960410     C                   Eval      IMPASSR105 =  Imponib039
141200990114     c                   if        imponib039 = 0
141300990114     C                   Eval      IMPASSR105 =  Importo039
141400990114     c                   endif
141500960410     C                   IF        ImpAssR105 < 0
141600960410     C                   Eval      ImpAssR105 =  ImpAssR105  * -1
141700960410     C                   ENDIF
141800960410     C                   Eval      SNSR105    =  Sns039
141900960410     C                   IF        SNSR105 < 0
142000960410     C                   Eval      SNSR105 =  SNSR105  * -1
142100960410     C                   ENDIF
142200960410     C                   Eval      IVAR105    =  Iva039
142300960410     C                   IF        IVAR105 < 0
142400960410     C                   Eval      IVAR105 =  IVAR105  * -1
142500960410     C                   ENDIF
142600981015     c* in divisa
142700981015     C                   Eval      IMPASDR105 =  ImponD039
142800981015     C                   IF        ImpAsDR105 < 0
142900981015     C                   Eval      ImpAsDR105 =  ImpAsDR105  * -1
143000981015     C                   ENDIF
143100981015     C                   Eval      SNSdR105    =  Snsd039
143200981015     C                   IF        SNSdR105 < 0
143300981015     C                   Eval      SNSdr105 =  SNSdR105  * -1
143400981015     C                   ENDIF
143500981015     C                   Eval      IVAdr105    =  Ivad039
143600981015     C                   IF        IVAdR105 < 0
143700981015     C                   Eval      IVAdR105 =  IVAdR105  * -1
143800981015     C                   ENDIF
143900981015     c                   eval      divisar105 = divisa039
144000981015     c*
144100951005     C                   Eval      DAR105     =  DA039
144200951005     C                   Eval      NRRMRAR105 =  0
144300951005     C                   Movel     NDCR05DS1     KPJBU
144400951005     C                   Call      'NDCR05R1'
144500951005     C                   Parm                    KPJBA
144600961209     C* se presente + di un movimento
144700961209     c                   when      NrrR04 > 1
144800961209    >C                   CLEAR                   KPJBU
144900961209    >C                   Reset                   NDCR05DS
145000961209     C                   Eval      NASReGFR05  =  NrAsReg039
145100961209     C                   Eval      SYSFR05    =  Sys039
145200961209     C                   Eval      KCCR05     =  Kcc039
145300961209     C                   Eval      KSCR05     =  Soggett039
145400961209     C                   Movel     NDCR05DS      KPJBU
145500961209     C                   Call      'NDCR05R'
145600961209     C                   Parm                    KPJBA
145700961209     c                   endsl
145800040325c1741c* ritenuta di pagamento
145900040325  "  c                   else
146000040325  "  C                   exsr      CallRit14
146100040325C1741C                   endif
146200951005     C                   endif
146300000810     C*
146400951011     C* 7 = Intra
146500951011     C                   if        d1b07 = 1
146600030916B0612C* cerco partita iva alla data documento
146700030916B0612c                   exsr      CallDvaCpi
146800960515    >C                   CLEAR                   KPJBU
146900951011    >C                   Reset                   ISW050DS
147000951011     C                   Eval      SOC050  =  Societa039
147100951011     C                   Eval      DUN050  =  Unita039
147200951011     C                   Eval      STA050  =  NazG12
147300030916B0612c* se trovate variazioni uso quelle
147400030916  "  C     dvcerrore     ifeq      *off
147500030916 "   C                   movel     DvcOutput     ANCPI
147600101210B0612C                   Eval      CIV050  =  %subst(CpiPartIva:3:12)
147700101210B1142C* Se presente devo passare lo stato della p.iva
147800101210B1142C                   If        %subst(CpiPartIva:1:2) <> Sta050
147900101210B1142C                             and %subst(CpiPartIva:1:2) <> '  '
148000101210B1142C                             and %subst(CpiPartIva:1:2) <  '00'
148100101210B1142C                   eval      Sta050 = %subst(CpiPartIva:1:2)
148200101210B1142C                   Endif
148300101210B0612c                   else
148400030916B0612c* altrimenti uso quella in  entrata
148500030916     C                   Eval      CIV050  =  %subst(PartIva039:3:12)
148600101210B1142C* Se presente devo passare lo stato della p.iva
148700101210B1142C                   If        %subst(PartIva039:1:2) <> Sta050
148800101210B1142C                             and %subst(PartIva039:1:2) <> '  '
148900101210B1142C                             and %subst(PartIva039:1:2) <  '00'
149000101210B1142C                   eval      Sta050 = %subst(PartIva039:1:2)
149100101210B1142C                   Endif
149200030916B0612C                   endif
149300100715B1133X****               If        TipoReg039 = '1'
149400100715B1133X****               Eval      TRG050  =  'I'
149500100715B1133X****               Else
149600100715B1133X****               Eval      TRG050  =  'E'
149700100715B1133X****               Endif
149800100715B1133C*
149900100715B1133C                   Select
150000100715B1133C                   when      Snatura039 = 'F'
150100100715B1133C                   Eval      TRG050  =  'I'
150200100715B1133C                   when      Snatura039 = 'C'
150300100715B1133C                   Eval      TRG050  =  'E'
150400100715B1133C                   other
150500100715B1133C                   Eval      TRG050  =  ' '
150600100715B1133C                   Endsl
150700100715B1133C*
150800951011     C                   Eval      SRF050  =  SerDoc039
150900951011     C                   Eval      NFT050  =  NrDoc039
151000951113     C                   Eval      DFT050  =  DtDoc039
151100951113     C                   Eval      DRF050  =  DtReg039
151200951011     C                   Eval      VAL050  =  Divisa039
151300951011     C                   Eval      VML050  =  Importo039
151400951011     C                   Eval      VMV050  =  ImportD039
151500100426B1126C                   Eval      Sys050  =  Sys039
151600100426B1126C                   Eval      NrAsReg050 = NRASREG039
151700951011     C                   Movel     ISW050DS      KPJBU
151800951011     C                   Call      'ISW050R'
151900951011     C                   Parm                    KPJBA
152000951011     C                   endif
152100000810     C*
152200951019     C* 8 = Invio
152300951019     C                   if        d1b08 = 1
152400951019     C                   RESET                   A727DS
152500951019     C                   MOVE      '01'          OPZ727
152600951019     C*
152700951019     C                   MOVE      SerReg039     SerRegPt
152800951019     C                   MOVE      NrReg039      NrRegPt
152900951019     C                   CALLb     'XDT4000'
153000951019     C                   PARM      DtReg039      XDTAMG           10
153100951019     C                   PARM                    XDTGMA            6 0
153200951019     C                   PARM      0             XDTSTA            1 0
153300951019     C                   PARM                    XDTANN            4 0
153400951019     C                   PARM                    XDTMES            2 0
153500951019     C                   PARM                    XDTGIO            2 0
153600951020     C                   Reset                   DataDiotto
153700000125C1320C                   Select
153800000125  "  C* formato mm/gg/aaaa
153900000125  "  C                   When      $DatFmt = '*MDY'
154000000125  "  C                   Move      XdtAnn        aa
154100000125  "  C                   Move      Xdtmes        gg
154200000125  "  C                   Move      Xdtgio        mm
154300000125  "  C* formato aa/mm/gg
154400000125  "  C                   When      $DatFmt = '*YMD'
154500000125  "  C                   Move      XdtAnn        gg
154600000125  "  C                   Move      Xdtmes        mm
154700000125  "  C                   Move      Xdtgio        aa
154800000125  "  C* formato gg/mm/aaaa
154900000125C1320C                   When      $DatFmt = '*DMY'
155000951020     C                   Move      XdtAnn        aa
155100951019     C                   Move      Xdtmes        mm
155200951019     C                   Move      Xdtgio        gg
155300000125C1320C                   EndSl
155400951019     C                   move      DataDiOtto    DtRegpt
155500951019     C                   MOVEL     TestoPosta    TXT727
155600951019     C                   MOVEL     'PNO'         TPA727
155700951019     C                   MOVEL     Sys039        KY1727
155800951019     C                   MOVEL     NrAsReg039    KY2727
155900951019     C                   MOVEL     *blank        KY3727
156000951019     C                   MOVEL     *blank        KY4727
156100951019     C                   MOVE      'E'           PGM727
156200951019     C                   MOVE      Societa039    SOC727
156300951019     C                   MOVE      *on           INZ727
156400951019     C                   CLEAR                   KPJBU
156500951019     C                   MOVEL     A727DS        KPJBU
156600951019     C                   CALL      'ANA727R'
156700951019     C                   PARM                    KPJBA
156800951019     C                   MOVEL     KPJBU         A727DS
156900951019     C                   endif
157000960513     C*
157100960513     C* 9 = Movimenti cespiti
157200960515     C                   IF        D1B09 = 1
157300090220D2487C* controllo chiamata ricorsiva
157400090220  "  C                   Reset                   X48ChPgmDS
157500090220  "  C                   Eval      X48Pgm      = 'NDCSC2R'
157600090220  "  C                   Eval      KPJBU       = X48ChPgmDS
157700090220  "  C                   CALL      'X48CHPGM'
157800090220  "  C                   Parm                    KPJBA
157900090220  "  C                   Movel     KPJBU         X48ChPgmDS
158000090220  "  C*
158100090220D2487C                   if        X48Errore   = *Off
158200050408C1776X*                  RESET                   NDCS01DS
158300050408  "  X*                  MOVEL     '02'          OpzS01
158400050408  "  X*                  MOVEL     Sys039        SysS01
158500050408  "  X*                  MOVEL     NrAsReg039    NrAsRegS01
158600050408  "  X*                  MOVEL     NDCS01DS      KPJBU
158700050408  "  X*                  CALL      'NDCS01R'
158800050209  "  X*                  PARM                    KPJBA
158900050408  "  C                   RESET                   NDCSC2DS
159000050408  "  C                   MOVEL     '02'          OpzSC2
159100050408  "  C                   MOVE      'CG'          CtbSC2
159200050408  "  C                   MOVEL     Sys039        SysSC2
159300050408  "  C                   MOVEL     NrAsReg039    NrAsRegSC2
159400050408  "  C                   MOVEL     NDCSC2DS      KPJBU
159500050209C1776C                   CALL      'NDCSC2R'
159600050209     C                   PARM                    KPJBA
159700960513     C                   ENDIF
159800090220D2487C                   ENDIF
159900970113     C*
160000970113     C* 10= P.N.Collegata
160100970113   1 C                   IF        D1B10 = 1
160200970114     c*
160300970114     c* verifico se la registrazione esiste
160400970113     C                   Exsr      RepRegColl
160500970113     c* se esiste
160600970116     c                   if        Err029   = *off
160700970113     c* emetto finestra per scegliere se cancellare o modificare la
160800970113     c* registrazione collegata
160900970116     c* preseleziono il bottone di modifica
161000970116     c                   clear                   FMTD2
161100970116     c                   eval      d2b01 = 1
161200970116     c                   eval      d2b02 = 0
161300970115     c                   EXFMT     FMTD2
161400970115     C* se scelto, ripasso informazione e se scelta modifica o cancellazione
161500970115     C                   SELECT
161600970128     c* prevale l 'annullamento
161700970128     C                   when      D2B02 = 1
161800970128     c                   eval      PNColl039 = *on
161900970128     c                   eval      OPZ039    = '04'
162000110304D2670C                   if        %addr(OpzPnCol)<> *null
162100110304  "  c                   eval      OPZPnCol  = '04'
162200110304D2670c                   endif
162300970128     c                   eval      D2b01 = 0
162400970115     C                   when      D2B01 = 1
162500970114     c                   eval      PNColl039 = *on
162600970115     c                   eval      OPZ039    = '02'
162700110304D2670C                   if        %addr(OpzPnCol)<> *null
162800110304  "  c                   eval      OPZPnCol  = '02'
162900110304D2670c                   endif
163000970115     c                   other
163100970115     c                   eval      PNColl039 = *off
163200970115     c                   endsl
163300970114     c                   ELSE
163400970115     c* se non esiste, ripasso informazione per l'immissione
163500970127     c                   eval      OPZ039    = '01'
163600110304D2670C                   if        %addr(OpzPncol)<> *null
163700110304  "  c                   eval      OPZPnCol  = '01'
163800110304D2670c                   endif
163900970114     c                   eval      PNColl039 = *on
164000970115     c                   eval      TabColl039= OpeTab
164100970114     c                   endif
164200970115     C                   ELSE
164300970115     c* non scelto il bottone
164400970115     c                   eval      PNColl039 = *off
164500970113   1eC                   ENDIF
164600020221C1530C*
164700020304  "  C* 11=Laguna
164800020221  "  C*
164900030113C1530C                   IF        D1B11 = 1
165000030113C1605C*
165100030113  "  C* reperisco dal parametro modulo LAGUNA se permettere la
165200030113  "  C* Sostituzione o il Versionamento o entrambi
165300030113  "  C*
165400030113  "  C                   MOVEL     *BLANK        XPARIC
165500060531C1605C                   MOVEL     XSCSOC        XPASOC
165600060531D2001x***                MOVEL     'LAGUNA'      XPAPAR
165700060531D2001C                   MOVEL(P)  'LAGUNA'      XPAPAR
165800060531C1605C                   MOVEL     *BLANK        XPAOUT
165900030113  "  C                   EXSR      REPPAR
166000030113  "  C* la finestra intelligente la chiamo solo se c'è il parametro
166100030113  "  C* modulo
166200030113  "  C     XPAERR        IFEQ      '0'
166300030113C1605C                   MOVEL     XPAOUT        ANP062DS
166400030113C1605C*
166500030113C1530C* chiamo il modulo che decide che tipo di tendina (comandi)
166600021025C1530C* mostrare
166700021025D1215C***                CALLB     'NDC517R'
166800021025  "  C***                PARM                    KPJBA
166900021025  "  C***                PARM                    NDC039DS
167000021025  "  C***                PARM                    KeyOld
167100021025  "  C***                PARM                    KeyNew
167200021025  "  C                   CALL      'ANA672R'
167300021025  "  C                   PARM                    KPJBA
167400021025  "  C                   PARM                    ClasseProJ
167500021025  "  C                   PARM                    KeyOld
167600021025  "  C                   PARM                    KeyNew
167700021025  "  C                   PARM                    Key2NewLgn
167800021025D1215C                   PARM                    DesNewLgn
167900030113C1605C                   PARM                    SOVFatC062
168000030210D1270C                   PARM                    DesOldLgn
168100030113C1605C                   ENDIF
168200021025C1530C                   ENDIF
168300020221C1530C*
168400021211C1579C*
168500021211  "  C* 12=C/correnti di pool
168600021211  "  C                   IF        D1B12 = 1
168700021211  "  C* chiamata al prgm di gestione
168800021211  "  C                   EXSR      Call534R
168900021211  "  C                   ENDIF
169000021211C1579C*
169100040616C1767C*
169200040616  "  C* 13=Ratei e risconti
169300040616  "  C                   IF        D1B13 = 1
169400040616  "  C* chiamata al prgm di gestione
169500040616  "  C                   EXSR      Call327R
169600040616  "  C                   ENDIF
169700040616C1767C*
169800110119C2179C* Legami registrazioni
169900110119  "  C                   IF        D1B15 = 1
170000110119  "  C* chiamata al prgm di gestione
170100110119  "  C                   EXSR      Call741R
170200110119C2179C                   ENDIF
170300950731     C*
170400021211C1579X***** ex C1408 12=P.N.INTERCOMPANY
170500040616C1767x****  ex C1579 13=P.N.INTERCOMPANY
170600040616C1767C* 14=P.N.INTERCOMPANY
170700020221C1408C*
170800020221C1530x***                If        D1B11 = 1
170900021211C1579X**** ex C1530      If        D1B12 = 1
171000040616C1767x**** ex C1579      If        D1B13 = 1
171100040708C1767C                   If        D1B14 = 1
171200061215D2100 * devo essere autorizzato alla società di destinazione
171300061215D2100C                   exsr      CtrlAutSoc
171400061215D2100X**per decidere se visualizzare la dicitura "P.N.Intercompany"
171500061215  "  X**(se Autorizzato in modo AUTOMATICO(1) o in modo MANUALE(2))
171600061215  "  X**ho già usato la routine "InzPnInter"
171700061215  "  X** ex C1408 * "check manuale" --> in questo momento eseguo le operazioni iniziali
171800061215  "  X** ex C1408 ****** If        $ElaPnI = '2'
171900061215  "  X** ex C1408 ****** ExSr      InzPnInter
172000061215D2100X** ex C1408 ****** EndIf
172100061215C1408C* se esistono le condizioni per la p.n. intercompany ...
172200000824  "  C                   If        $ChkPnI = *On
172300000825  "  C* ... eseguo la p.n. intercompany
172400000824  "  C                   ExSr      RunPnInter
172500000810  "  C                   EndIf
172600000824  "  C                   EndIf
172700000906  "  C*
172800000906  "  C**********************************************************************
172900000906  "  C* ATTENZIONE: la P.N.INTERCOMPANY deve essere sempre l'ultima operaz.
173000000906  "  C* eseguita (se si aggiungono nuove funzioni farlo prima di questa !!!)
173100000906  "  C**********************************************************************
173200000906C1408C*
173300940117     C                   ENDSR
173400061215D2100 ************************************************************
173500061215  "   * Controllo autorizzazione al società di destinazione
173600061215  "   ************************************************************
173700061215  "  C     CtrlAutSoc    Begsr
173800061215  "   *
173900061215  "  C                   clear                   XAutSocDS
174000061215  "  C                   move      StsUsr        XASPrf
174100061215  "  C                   move      Knsif         XASSI
174200061215  "  C                   move      SocDes299     XASSocieta
174300061215  "  C                   call      'XAUTSOC'
174400061215  "  C                   parm                    XAutSocDS
174500061215  "  C                   if        XASRtnCde <> '0'
174600061221  "   *
174700061221  "   *   Variabile segnala l'impossibilità di procedere per l'Interc.
174800061221  "  C                   eval      $ChkPnI = *off
174900061221  "   *
175000061215  "   *   Wdw di avvertimento
175100061215  "  C                   clear                   XMsgDS
175200061215  "  C                   clear                   XMsgTxt
175300061215  "   *      nr messaggio precodificato
175400061215  "  C                   movel     0             MsgMsg
175500061215  "   *      esistenza risposta
175600061215  "  C                   movel     'N'           MsgRsp
175700061215  "   *      emissione a video
175800061215  "  C                   movel     'S'           MsgEmv
175900061215  "   *      riga da cui parte la finestra
176000061221  "  C                   z-add     7             MsgRgp
176100061215  "   *      gestione analisi del problema (F2)
176200061215  "  C                   movel     'N'           MsgLck
176300061215  "   *      non utilizzato
176400061215  "  C                   movel     'N'           MsgCnl
176500061215  "   *      mandare o meno alla coda del display/utente
176600061215  "  C                   movel     'N'           MsgVid
176700061215  "   *      mandare o meno all'operatore
176800061215  "  C                   movel     'N'           MsgOpe
176900061215  "   *      emissione in stampa
177000061215  "  C                   movel     'N'           MsgStp
177100061221  "   *
177200061215  "   *      PRO0039-Utente non abilitato alla società
177300061221  "  C                   clear                   MsgMs1
177400061221  "  C                   clear                   MsgMs2
177500061215  "  C                   eval      IdMsg = 'PROMSG    *LIBL     PRO0039'
177600061215  "  C                   callb     'XRTVM'
177700061215  "  C                   parm                    IdMsg            27
177800061215  "  C                   parm                    TxtMsg          100
177900061215  "  C                   parm                    ErrMsg            1
178000061221  "  C                   parm      *on           CtrMsg            1            centratura
178100061215  "  C                   parm      70            LenMsg            3 0
178200061215  "  C                   if        ErrMsg <> *on
178300061221  "  C                   movel     TxtMsg        MsgMs3
178400061215  "  C                   else
178500061221  "  C                   movel     *all'?'       MsgMs3
178600061215  "  C                   endif
178700061221  "  C     MsgMs3        cat       SocDes299:1   MsgMs3
178800061221  "   *
178900061221  "   *      per mantenere il video principale come sottofondo:
179000061221  "   *      - close + open ('1') della wdw, la 1° emissione
179100061221  "   *      - wdw rimane aperta('0'), le emissioni successive consecutive
179200061221  "  C                   eval      XMsgInz = $PrimoXMsg
179300061221  "  C                   if        $PrimoXMsg  = *on
179400061221  "  C                   eval      $PrimoXMsg  = *off
179500061221  "  C                   endif
179600061221  "   *
179700061215  "  C                   call      'XMSGR'
179800061215  "  C                   parm                    XMsgDS
179900061215  "  C                   parm                    XMsgTxt         135
180000061215  "  C                   parm                    XMsgInz           1
180100061215  "   *
180200061215  "  C                   endif
180300061215  "   *
180400061215D2100C     XCtrlAutSoc   Endsr
180500040325C1741C************************************************************
180600040325  "  C* Gestione ritenute relative al pagamento
180700040325C1741C************************************************************
180800040325     C     CallRit14     BEGSR
180900040325     c*
181000040325     c* chiamo pgm di gestione ritenute in stato da versare            are
181100040325     c                   reset                   NDCR14DS
181200040507     c                   eval      opzr14 = opz039
181300040325     c                   eval      NasRegPr14 = NrAsReg039
181400040325     C                   Eval      SYSPR14    =  Sys039
181500040325     C                   Eval      SocietaR14 =  Societa039
181600040325     C                   EVAL      InzR14 = *on
181700040325     c                   eval      KPJBU = Ndcr14ds
181800040325     c                   call      'NDCR14R'
181900040325     c                   parm                    KPJBA
182000040325     C                   movel     kpjbu         NDCR14DS
182100040325     c*
182200040325C1741C                   ENDSR
182300030916B0612C************************************************************
182400030916  "  C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
182500030916B0612C************************************************************
182600030916     C     CALLDVACPI    BEGSR
182700030916     C* reperisco il capoconto del conto
182800030916     c                   movel     Snatura039    $kcc
182900030916     C                   CALLB     'XCAPCLIFOR'
183000030916     C                   PARM                    Societa039
183100030916     C                   PARM                    $kcc
183200030916     C                   PARM                    soggett039
183300030916     C*
183400030916     C* reperisco il soggetto del conto
183500030916     C                   clear                   ND002DS
183600030916     C                   eval      $LenOut = %Size(ND002DS)
183700030916     C                   CALLB     'NDMVC002'
183800030916     C                   PARM      Societa039    $soc
183900030916     C                   PARM                    $kcc
184000030916     C                   PARM      soggett039    $ksc
184100030916     C                   PARM                    $date
184200030916     C                   PARM      *off          $GesMsg
184300030916     C                   PARM      *off          $Esito
184400030916     C                   PARM      'ND002DS'     $Struttura
184500030916     C                   PARM                    ND002DS
184600030916     C                   PARM                    $LenOut
184700030916     C* reperisco partita iva del soggetto alla data documento
184800030916     C                   move      '2'           DVCRIC
184900030916     C                   move      Societa039    DVCSOCIETA
185000030916     C                   move      *blank        DVCUNITA
185100030916     C                   movel     'ANCPI'       DVCSTRUTT
185200030916     C                   move      DtDoc039      DVCDTRIF
185300030916     C                   eval      DVCLENOUT = %Size(ANCPI)
185400030916     C                   move      sogg002       DVCSOGG
185500030916     C*
185600030916     C                   CALLB     'NDDVACPI'    PDVACPI
185700030916     C*
185800030916B0612C                   ENDSR
185900951011     C/EJECT
186000021211C1579C************************************************************
186100021211  "  C* Chiamata al pgm di gestione raccordi di c/c di pool
186200021211  "  C************************************************************
186300021211  "  C     Call534R      BEGSR
186400021211  "  C*
186500021211  "  C* controllo chiamata ricorsiva
186600021211  "  C                   Reset                   X48ChPgmDS
186700021211  "  C                   Eval      X48Pgm      = 'NDC534R'
186800021211  "  C                   Eval      KPJBU       = X48ChPgmDS
186900021211  "  C                   CALL      'X48CHPGM'
187000021211  "  C                   Parm                    KPJBA
187100021211  "  C                   Movel     KPJBU         X48ChPgmDS
187200021211  "  C*
187300021211  "  C                   if        X48Errore   = *Off
187400021211  "  C*
187500021211  "  C                   RESET                   NDC534DS
187600021211  "  C                   Eval      OPZ534      = '02'
187700021211  "  C*
187800021211  "  C                   EXSR      Rie534DS
187900021211  "  C*
188000021211  "  C                   MOVEL     NDC534DS      KPJBU
188100021211  "  C                   CALL      'NDC534R'
188200021211  "  C                   PARM                    KPJBA
188300021211  "  c                   endif
188400021211  "  C*
188500021211C1579C     XCall534R     ENDSR
188600021211C1579C************************************************************
188700021211  "  C* Utilizzo i dati passati in NDCPRDS
188800021211  "  C************************************************************
188900021211  "  C     Rie534DS      BEGSR
189000021211  "  C*
189100021211  "  C                   Eval      Societa534  = CPRSocieta
189200021211  "  C                   Eval      ErrCtb534   = *Blank
189300021211  "  C                   Eval      KccP534     = *Blank
189400021211  "  C                   Eval      KscP534     = *Blank
189500021211  "  C                   Eval      CauRigP534  = CPRCauRigP
189600021211  "  C                   Eval      Segno534    = *Blank
189700021211  "  C                   Eval      SysPro534   = CPRSysPro
189800021211  "  C                   Eval      NrAsPro534  = CPRNrAsPro
189900021211  "  C                   Eval      NrRiPro534  = 0
190000021211  "  C                   Eval      DtImmPD534  = DatMin
190100021211  "  C                   Eval      DtImmPA534  = DatMin
190200021211  "  C                   Eval      DtRegPD534  = DatMin
190300021211  "  C                   Eval      DtRegPA534  = DatMin
190400021211  "  C                   Eval      KccD534     = *Blank
190500021211  "  C                   Eval      KscD534     = *Blank
190600021211  "  C                   Eval      CauTesD534  = CPRCauTesD
190700021211  "  C                   Eval      SysDes534   = CPRSysDes
190800021211  "  C                   Eval      NrAsDes534  = CPRNrAsDes
190900021211  "  C                   Eval      DtImmDD534  = DatMin
191000021211  "  C                   Eval      DtImmDA534  = DatMin
191100021211  "  C                   Eval      DtRegDD534  = DatMin
191200021211  "  C                   Eval      DtRegDA534  = DatMin
191300021211  "  C                   Eval      RifInt534   = *Blank
191400021211  "  C                   Eval      SubNum534   = 0
191500021211  "  C*
191600021211C1579C     XRie534DS     ENDSR
191700110119C2179C/EJECT
191800110202  "  C***************************************************************
191900110119  "  C* Chiamata al pgm di gestione raccordi di legami registrazioni
192000110119  "  C***************************************************************
192100110119C2179C     Call741R      BEGSR
192200110119     C*
192300110119     C* controllo chiamata ricorsiva
192400110119     C                   Reset                   X48ChPgmDS
192500110119     C                   Eval      X48Pgm      = 'NDC740R'
192600110119     C                   Eval      KPJBU       = X48ChPgmDS
192700110119     C                   CALL      'X48CHPGM'
192800110119     C                   Parm                    KPJBA
192900110119     C                   Movel     KPJBU         X48ChPgmDS
193000110119     C*
193100110119     C                   if        X48Errore   = *Off
193200110119     C*
193300110119     C                   RESET                   NDC741DS
193400110119     C                   Eval      OPZ741      = '01'
193500110119     C*
193600110119     C                   EXSR      Rie741DS
193700110119     C*
193800110119     C                   MOVEL     NDC741DS      KPJBU
193900110119     C                   CALL      'NDC740R'
194000110119     C                   PARM                    KPJBA
194100110119     C                   EndIf
194200110119     C*
194300110119C2179C                   ENDSR
194400110119  "  C************************************************************
194500110119  "  C* Utilizzo i dati passati in NDC039DS
194600110119  "  C************************************************************
194700110119C2179C     Rie741DS      BEGSR
194800110119     C*
194900110119     C                   Eval      Societa741  = Societa039
195000110119     C                   Eval      NrReg741    = NrReg039
195100110119     C                   Eval      DtReg741    = DtReg039
195200110119     C                   Eval      SerReg741   = SerReg039
195300110610D2688C                   Eval      Sys741      = Sys039
195400110610D2688C                   Eval      NrAsReg741  = NrAsReg039
195500110119     C*
195600110119C2179C                   ENDSR
195700951011     C************************************************************
195800951011     C* Reperimento stato
195900951011     C************************************************************
196000951011     C     RepStato      BEGSR
196100951011     C*
196200951011     C                   MOVE      '1'           XTBRIC
196300951011     C                   MOVE      Societa039    XTBAZI
196400951011     C                   MOVE      *ZERO         XTBKEY
196500951011     C                   MOVE      'G12'         XTBCOD
196600951011     C                   MOVE      Stato039      XTBKEY
196700961011     C                   CALLb     'XATB'
196800951011     C                   PARM                    XATBDS
196900951011   B3C     XTBERR        IFEQ      '0'
197000951011     C                   MOVEL     XTBUNI        TABG12
197100951011   E2C                   ENDIF
197200951011     C                   EndSr
197300970113     C/EJECT
197400970113     C************************************************************
197500970113     C* Reperimento regola generazione p.nota collegata
197600970113     C************************************************************
197700970113     C     RepPNColl     BEGSR
197800970116     c*
197900970116     c* Reperisco esistenza regola pn collegata
198000970116     c                   reset                   ndc029ds
198100970116     c                   eval      opz029 = '03'
198200970116     c                   eval      sys029 = sys039
198300970116     c                   eval      NrasReg029 = NrasReg039
198400970116     c                   eval      Tabcoll029 = opetab
198500970116     C                   Call      'NDC029R'
198600970116     c                   parm                    KPJBA
198700970116     c                   parm                    ndc029ds
198800970113     C*
198900970113     C                   EndSr
199000970113     C/EJECT
199100970113     C************************************************************
199200970113     C* Reperimento esistenza  registrazione collegata
199300970113     C************************************************************
199400970113     C     RepRegColl    BEGSR
199500970113     c*
199600970114     c                   reset                   ndc029ds
199700970113     c                   eval      opz029 = '02'
199800970116     c                   eval      sys029 = sys039
199900970116     c                   eval      NrasReg029 = NrasReg039
200000970113     C                   Call      'NDC029R'
200100970113     c                   parm                    KPJBA
200200970113     c                   parm                    ndc029ds
200300970113     C*
200400970113     c                   Endsr
200500970113     C/EJECT
200600960409     C************************************************************
200700970113     C* Reperimento dati causale
200800960409     C************************************************************
200900970113     C     RepDatCaus    BEGSR
201000960409     C*
201100960409     C                   Eval      LenOut= %Size(ANOPEDS)
201200960409     C                   Callb     'NDMVC003'
201300960409     C                   PARM                    Societa039
201400960409     C                   PARM                    Causale039
201500960409     C                   PARM      Datmin        Data             10
201600960409     C                   PARM      '1'           Uso               1
201700960409     C                   PARM      *off          GesMsg
201800960409     C                   PARM                    Esito
201900960409     C                   PARM      'ANOPE'       StrutturaO
202000960409     C                   PARM                    ANOPEDS
202100960409     C                   PARM                    LenOut
202200960409     C                   PARM      *off          Refresh
202300960409     C*
202400960409     C                   ENDSR
202500950731     C/EJECT
202600950731     C************************************************************
202700950731     C* Salva come modello primanota
202800950731     C************************************************************
202900950731     C     SalvaModPN    BEGSR
203000950731     C*
203100960724     C* reperisco numero per nuova registrazione
203200960724     C                   CallB     'NDMVC101'
203300960724     C                   PARM                    Esito
203400960724     C                   PARM                    OSys
203500960724     C                   PARM                    ONrasreg
203600960724     C                   PARM      '0'           Prenot
203700960724     C*
203800960724     C* richiedo l'immissione testata modello
203900960724     C                   CLEAR                   KPJBU
204000960724     C                   Reset                   NDC052DS
204100960724    >C                   Eval      OPZ052     = '01'
204200960724    >C                   Eval      TPPNOT052  = DOCIVA039
204300960724    >C                   Eval      Sys052     = Osys
204400960724    >C                   Eval      Kcc1052     = Kcc039
204500960724    >C                   Eval      Ksc1052     = Soggett039
204600041118C1801c                   eval      Causale052 = Causale039
204700041118  "  C                   Eval      SysPn052   = sys039
204800041118c1801C                   Eval      NrAsRPn052 = NrAsReg039
204900960724    >C                   Eval      NrAsReg052 = ONrAsReg
205000960724     c                   Movel     NDC052DS      KPJBU
205100960724     C                   Call      'NDC052R'
205200960724     C                   Parm                    KPJBA
205300960724     C                   Movel     KPJBU         NDC052DS
205400030624     C*
205500960724     C* se eseguita un operazione di aggiornamento proseguo
205600960724     C                   if        Err052 = *off and OPR052= *on
205700960724     C*
205800950731     c                   Eval      LenOut = %Size(NDREGDS)
205900950731     C                   CAllB     'NDMVC117'
206000950816     C                   PARM                    sys039
206100950816     C                   PARM                    nrasreg039
206200950731     C                   PARM                    Osys
206300950731     C                   PARM                    Onrasreg
206400960410     C                   PARM      'NDREG'       StrutturaO
206500950731     C                   PARM                    LenOut
206600950731     C                   PARM                    Esito
206700950731     C                   PARM                    NDREGDS
206800960724     C                   PARM                    ModoMod052
206900960724     C*
207000960724     C* se la copia è andata a buon fine
207100950731     C                   If        Esito = *off
207200960724     C* scrivo la testata registrazione
207300950731     c                   Eval      LenIn  = %Size(ND004DS)
207400950731     C                   Exsr      Rie004DS
207500950731     C                   CallB     'NDMVC111'
207600950801    >C                   PARM      '03'          Operazione
207700950731     C                   PARM                    LenIn
207800950731     C                   PARM                    ND004DS
207900950731     C                   PARM      *off          GesMsg
208000950731     C                   PARM                    Esito
208100950731     C                   PARM                    *omit
208200950731     C                   PARM                    *omit
208300950731     C                   PARM                    *omit
208400950731     C                   PARM                    *omit
208500030624     c*
208600030624D1381c* copio anche le descrizioni aggiuntive
208700030624D1381c                   exsr      CopiaDescr
208800960724     C*
208900960724     C* confermo la registrazione
209000950921     C                   CALLB     'NDMVC102'
209100960724     C                   PARM      '02'          Operazione
209200950921     C                   PARM      *off          GesMsg
209300950921     C                   PARM                    Esito
209400950921     C                   PARM      *on           Commit            1
209500950921     C                   PARM                    WNrReg
209600960724     C*
209700950731     C                   Endif
209800960724     C*
209900950731     C                   Endif
210000950731     C*
210100950731     C                   ENDSR
210200950731     C/EJECT
210300950731     C************************************************************
210400950731     C* Riempimento rcd da scrivere
210500950731     C************************************************************
210600950731     C     Rie004DS      BEGSR
210700950731     C*
210800950731     C                   RESET                   ND004DS
210900950731     C*
211000950731     C                   EVAL      KNrAz004   = RegKNrAz
211100950731     C                   EVAL      FunGen004  = RegFunGen
211200950731     C                   EVAL      Societa004 = RegSocieta
211300950731     C                   EVAL      CausTes004 = RegCausTes
211400950731     C                   EVAL      Classe004  = RegClasse
211500950731     C                   EVAL      Unita004   = RegUnita
211600950731     C                   EVAL      Ctb004     = 'MD'
211700950731     C                   EVAL      TpRegz004  = RegTpRegz
211800950731     C                   EVAL      NrReg004   = D1NrReg
211900950731     C                   EVAL      SerReg004  = D1SerReg
212000950731     C                   EVAL      NrDoc004   = RegNrDoc
212100950731     C                   EVAL      SerDoc004  = RegSerDoc
212200950731     C                   EVAL      Divisa004  = RegDivisa
212300950731     C                   EVAL      Cambio004  = RegCambio
212400950731     C                   EVAL      TpCam004  = *blank
212500950731     C                   MOVE      RegDtLGio     DtLGio004
212600950920     C                   MOVE      DatMin        DtRfCam004
212700950731     C                   MOVE      RegDtReg      DtReg004
212800950731     C                   MOVE      RegDtDoc      DtDoc004
212900950731     C                   MOVE      RegDtCoCo     DtCoCo004
213000950731     C                   MOVE      RegDtCoAn     DtCoAn004
213100950731     C*
213200950731     C                   ENDSR
213300030624D1381C************************************************************
213400030624     C* Copia descrizioni aggiuntive
213500030624D1381C************************************************************
213600030624     C     CopiaDescr    BEGSR
213700030624     C
213800030624     C* Aggancio record per copia
213900030624     C                   Eval      KSys    =  Sys039
214000030624     C                   Eval      KNrAsReg=  NrAsReg039
214100030624     C     K02Des01      Setll     NDDes01L
214200030624     c                   do        *hival
214300030624     C     K02Des01      ReadE     NDDes01L                               21
214400030624     c   21              leave
214500030624     C*
214600030624     C                   Eval      DesSys = OSys
214700030624     C                   Eval      DesNrAsReg= ONrAsReg
214800030624     c*
214900030624     c                   write     NDDES000
215000030624     c                   enddo
215100030624     C*
215200030624D1381C                   ENDSR
215300970113     C/EJECT
215400940224     C************************************************************
215500950301     C* AGGIORNAMENTO ANAGRAFICA
215600940224     C************************************************************
215700941214     C     AGGANA        BEGSR
215800940207     C*
215900050517C1772 * richiama pgm User Exit
216000060531C1772C                   if        UsrErr = *blank
216100060531D2001c                             and Finale053=*on
216200060531C1772 *
216300050517  "   * passa opzione ricevuta a user exit
216400050517  "  C                   move      *blank        XUKOpz
216500050517  "   * imposta chiave univoca  per user exit
216600050517  "  C                   move      XScSoc        XUKSoc
216700050517  "  C                   move      XScDUn        XUKDUn
216800050517  "  C                   movel     DSPgm         XUKPgm
216900050523  "   *    ho fino a 6 campi chiave passabili
217000050517  "  C                   move      Sys039        XUKFl1
217100050523  "  C                   move      NrAsReg039    XUKFl2
217200050523  "  C                   move      DtReg039      XUKFl3
217300050523  "  C                   move      NrReg039      XUKFl4
217400050523  "  C                   move      SerReg039     XUKFl5
217500050523  "  C                   move      Causale039    XUKFl6
217600050517  "  C                   movel     XUKDS         Kpjbu
217700050517  "  C     DSPgm         cat       'U':0         UsrExt           10
217800050517  "  C                   call      UsrExt                               21
217900050517  "  C                   parm                    Kpjba
218000050517  "  C                   if        *in21 = *on
218100050517  "  C                   move      *on           UsrErr            1
218200050517  "  C                   endif
218300050517C1772C                   endif
218400950725     C*
218500940203     C                   ENDSR
218600940224     C/EJECT
218700940224     C************************************************************
218800940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
218900940224     C************************************************************
219000940224     C     GESAGG        BEGSR
219100941214     C*
219200950816    >C                   MOVE      *ON           OPR039
219300940224     C* segnalo al pgm chiamante l'aver premuto l'F6
219400950816    >C                   MOVE      '0'           RET039
219500950301     C* e ritorno al pgm chiamante
219600950301     C                   MOVE      *ON           $FINE
219700940224     C*
219800940224     C                   ENDSR
219900940207     C/EJECT
220000940131     C************************************************************
220100940131     C* OPERAZIONI INIZIALI
220200940131     C************************************************************
220300940131     C     *INZSR        BEGSR
220400940131     C*
220500940127     C* Reperimento parametri
220600940127     C*
220700940117     C     *ENTRY        PLIST
220800940117     C                   PARM                    KPJBA
220900951011     C                   PARM                    NDC039DS
221000960807     C                   PARM                    DFTInPut
221100030210C1530C                   PARM                    KeyOld
221200030210D1270C                   PARM                    WDesOld
221300110304D2670C                   PARM                    OpzPnCol          2
221400000823C1408C*
221500000823  "  C* Reperimento dati società
221600000823  "  C*
221700000823  "  C                   MOVEL     'SOC001'      TIPXSC
221800000823  "  C                   MOVEL     *BLANK        SOCXSC
221900000823  "  C                   EXSR      REPSOC
222000000823  "  C     RTNXSC        IFNE      '1'
222100000823  "  C                   MOVEL     XSOCDS        SOC001
222200000823  "  C                   ELSE
222300000823  "  C                   MOVE      *ON           ERR039
222400000823  "  C                   MOVE      *ON           $FINE
222500000823  "  C                   EXSR      ENDPGM
222600000823C1408C                   ENDIF
222700000824     C*
222800000125C1320C* reperisco DATFMT del job
222900000125  "  C                   CallB     'XDATFMT'
223000000125C1320C                   Parm                    $DatFmt           4
223100060531     C*
223200060531D2001c* Reperisco parametro UEREGCTB  per vedere se chiamare USREXT
223300060531  "  C                   MOVEL     *BLANK        XPARIC
223400060531  "  C                   MOVEL     XSCSOC        XPASOC
223500060531  "  C                   MOVEL     'UEREGCTB'    XPAPAR
223600060531  "  C                   MOVEL     *BLANK        XPAOUT
223700060531  "  C                   EXSR      REPPAR
223800060531  "  C     XPAERR        IFEQ      '0'
223900060531  "  C                   MOVEL     XPAOUT        ANP053DS
224000060531D2001c                   ENDIF
224100000824     C*
224200950725     C                   ENDSR
224300940509     C/EJECT
224400940223     C************************************************************
224500940223     C* INIZIALIZZAZIONE VARIABILI
224600940223     C************************************************************
224700940223     C     INZVAR        BEGSR
224800030210D1270C*
224900030210  "  C* controllo parametri passati
225000030210  "  C                   IF        %parms < 5
225100030210  "  C                   EVAL      DesOld = *blank
225200030210  "  C                   ELSE
225300030210  "  C                   EVAL      DesOld = WDesOld
225400030210D1270C                   ENDIF
225500940715     C*
225600940223     C* Variabili per gestione videate
225700940128     C*
225800940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
225900940223     C                   MOVE      *OFF          $FINE                          fine pgm
226000940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
226100940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
226200940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
226300940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
226400000518     c*
226500000810D0564C* Chiamo programma per sistemare il flag di segnalazione
226600000518  "  c* esistenza cespiti su ndreg, in modo da far vedere o meno il bottone
226700000518  "  c* solo se esistono davvero o movimenti cespiti o conti di ndmov di
226800000518  "  c* sottonatura '4'
226900000518  " >C                   CALL      'NDC271R'
227000000518  "  C                   PARM      Sys039        SyS271
227100000518  "  C                   PARM      NrasReg039    NrasReg271
227200000518  "  C                   PARM      Societa039    Societa271        3
227300000518  "  C                   PARM      Cespite039    Cespite271        1
227400000518  "  C                   PARM                    err271            1
227500000518  "  C                   PARM                    opr271            1
227600000518  "  c* Riimposto il flag cespite039 con cespite271
227700000518D0564c                   move      Cespite271    Cespite039
227800940127     C*
227900940223     C* Variabili appoggio
228000940127     C*
228100940224     C                   Z-ADD     0             CURR
228200940224     C                   Z-ADD     0             CURC
228300000823C1408C*
228400000901  "  C* P.N. INTERCOMPANY
228500000901  "  C*
228600000901  "  C                   Eval      $ElaPnI = *Off
228700000906  "  C* se la società gestisce le REG. INTERCOMPANY ...
228800041008C1408C                   If        XScRegInt = '1'
228900041008C1761X* ex C1408 ** ... e la reg. è NON-IVA ...
229000041008C1761X* ex C1408 **                and REGDocIva = '0'
229100060206C1903X* ex C1408 ** ... e la reg. è DEFINITIVA ...
229200060206C1903X* ex C1408 **                and REGTpRegz = 'D'
229300060206C1408C* ... e se nello stack delle chiamate non c'è NDC299R
229400000906  "  C*      (se c'è significa che sono arrivato qui perchè attivato già
229500000901  "  C*      da una p.n. intercompany, quindi non la devo più fare
229600041008C1408C*      altrimenti entro in un loop di primenote ...)
229700041008D1674X***                Eval      PgmChkPgm = 'NDC299R'
229800041008  "  X***                Call      'XCHKPGM'
229900041008  "  X***                Parm                    PgmChkPgm
230000041008  "  X***                Parm      '0'           EsiChkPgm
230100041117D1674X***                If        EsiChkPgm <> *On
230200041027C1761X* X48chpgm avverte l'utente con window, se chiamata ricorsiva
230300041027  "  X* ed in tal caso mi interessa solo sapere se chiamata ricorsiva
230400041027  "  X* senza avvisare l'utente con la window
230500041027  "  X* ex D1674 *** C* controllo chiamata ricorsiva
230600041027  "  X* ex D1674 ***     Reset                   X48ChPgmDS
230700041027  "  X* ex D1674 ***     Eval      X48Pgm      = 'NDC299R'
230800041027  "  X* ex D1674 ***     Eval      KPJBU       = X48ChPgmDS
230900041027  "  X* ex D1674 ***     CALL      'X48CHPGM'
231000041027  "  X* ex D1674 ***     Parm                    KPJBA
231100041027  "  X* ex D1674 ***     Movel     KPJBU         X48ChPgmDS
231200041027  "  X* ex D1674 ***
231300041027C1761X* ex D1674 ***     if        X48Errore   = *Off
231400041027C1761 *
231500041027  "  C                   eval      PgmChkPgm = 'NDC299R'
231600041027  "  C                   call      'XCHKPGM'                            2121
231700041027  "  C                   parm                    PgmChkPgm
231800041027  "  C                   parm      '0'           EsiChkPgm
231900041027C1761C                   if        EsiChkPgm <> *on and *in21 = *off
232000041008C1408C* verifico se l'utente è autorizzato alla procedura e eventualmente
232100000824  "  C*  qual è la sua impostazione (automatica/manuale)
232200000823  "  C                   ExSr      ChkAutReI
232300000901  "  C                   EndIf
232400000823C1408C                   EndIf
232500040325     c*
232600040325C1741C* Se documento non iva vedo se esiste almeno una riten.del pagam.
232700040325  "  c                   eval      OkRitPagam = *off
232800040325  "  c                   if        Dociva039 ='0'
232900040325  "  C                   Eval      MRANAsRegP = NrAsReg039
233000040325  "  C                   Eval      MRASysP    = Sys039
233100040325  "  C                   Eval      MRASta = '1'
233200040325  "  c     k03Mra06      chain     Ndmra06l
233300040325  "  c                   if        %found(ndmra06l)
233400040325  "  c                   eval      OkRitPagam = *on
233500040325  "  c                   endif
233600040325C1741c                   endif
233700940224     C*
233800940224     C* Inizializzazione/Decodifica prima videata
233900940224     C*
234000940224     C                   EXSR      INZD1
234100940224     C                   MOVE      *OFF          $INZD1
234200940127     C*
234300940117     C                   ENDSR
234400960514     C/EJECT
234500960514     C************************************************************
234600960514     C* Definizione KLIST
234700960514     C************************************************************
234800960514     C     DEFKLIST      BEGSR
234900960514     C*
235000960806     C* ANDFT01L
235100960806     C     K03DFT01      KLIST
235200960806     C                   KFLD                    DFTSocieta
235300960806     C                   KFLD                    DFTCdDft
235400960806     C                   KFLD                    DFTUtente
235500960806     C     K02DFT01      KLIST
235600960806     C                   KFLD                    DFTSocieta
235700960806     C                   KFLD                    DFTCdDft
235800960514     C*
235900030624D1381C     K02Des01      KLIST
236000030624  "  C                   KFLD                    KSys
236100030624D1381C                   KFLD                    KNrasReg
236200030916B0612C* PLIST per NDDVACPI
236300030916  "  C     PDVACPI       PLIST
236400030916  "  C                   PARM                    DVCRIC
236500030916  "  C                   PARM                    DVCSOCIETA
236600030916  "  C                   PARM                    DVCUNITA
236700030916  "  C                   PARM                    DVCSTRUTT
236800030916  "  C                   PARM                    DVCDTRIF
236900030916  "  C                   PARM                    DVCLENOUT
237000030916  "  C                   PARM                    DVCSOGG
237100030916  "  C                   PARM                    DVCERRORE
237200030916B0612C                   PARM                    DVCOUTPUT
237300040325     c*
237400040325C1741C* NDMRA06L
237500040325  "  C     K03MRA06      KLIST
237600040325  "  C                   KFLD                    MraSysP
237700040325  "  C                   KFLD                    MraNAsRegP
237800040325C1741C                   KFLD                    MraSta
237900070801C2049C     K02Reg01      KLIST
238000070801  "  C                   KFLD                    RegSys
238100070801C2049C                   KFLD                    RegNrAsCol
238200030624     c*
238300960514     C                   ENDSR
238400000823C1408C************************************************************
238500000823  "  C* Controllo autorizzazione registrazioni intercompany
238600000823  "  C************************************************************
238700000823C1408C     ChkAutReI     BEGSR
238800000823     C*
238900000823     C                   CLEAR                   XCHKAUTDS
239000000823     C*
239100000823     C                   MOVE      XSCSOC        XCASOC
239200000823     C                   MOVE      XSCDUN        XCAUNI
239300000823     C                   Z-ADD     0             XCAGRP
239400000823     C                   MOVE      KNMUS         XCAPRF
239500000823     C                   MOVE      'CONREI'      XCAFNC
239600000823     C                   MOVE      *ZERO         XCAVFU
239700000823     C                   MOVE      'CK'          XCATCK
239800000823     C                   MOVE      '0'           XCANAU
239900000823     C*
240000000823     C                   CALLB     'XCHKAUT'
240100000823     C                   PARM                    XCHKAUTDS
240200000823     C*
240300000823     C                   If        XCARtn <> 0
240400000823     C* non esiste l'autorizzazione --> è come se non fosse autorizzato ...
240500000824     C                   Eval      $ElaPnI = *Off
240600000823     C                   Else
240700000823     C* esiste l'autorizzazione ...
240800000823    >C                   MoveL     XCADDS        CONREIDS
240900000824     C                   Eval      $ElaPnI = ElaPnI
241000000823     C                   EndIf
241100000823     C*
241200000823C1408C                   ENDSR
241300000823C1408C************************************************************
241400000823  "  C* REPERIMENTO DATI SOCIETA'
241500000823  "  C************************************************************
241600000823C1408C     REPSOC        BEGSR
241700000823     C*
241800000823     C                   CALLB     'XSOC'
241900000823     C                   PARM                    TIPXSC            6
242000000823     C                   PARM                    SOCXSC            3
242100000823     C                   PARM                    CDSXSC            9 0
242200000823     C                   PARM                    MODXSC            3
242300000823     C                   PARM      *BLANKS       RTNXSC            1
242400000823     C                   PARM                    XSOCDS
242500000823     C                   PARM                    KPJBA
242600000823     C*
242700000823C1408C                   ENDSR
242800020212C1530C************************************************************
242900020212  "  C* controllo se sono cambiati i dati di catalogazione
243000020212  "  C************************************************************
243100020212C1530C     CtrlDatiLgn   BEGSR
243200020212     C*
243300020213     C                   EVAL      KeyLgnModif = *off
243400030210D1270C                   EVAL      DesLgnModif = *off
243500020213     C* trasformi da packed a alfanumerici
243600020213     C                   Z-ADD     NrDoc039      NrDocSigned
243700020213     C                   MOVEL     NrDocSigned   NrDocAlfa
243800020226     C*
243900020226     C* creo la schiera con la nuova chiave (cioè quella dopo la
244000020226     C*  conferma della registrazione)
244100020226     C                   EXSR      RieKeyNewLgn
244200030210D1270C*
244300030210  "  C* se i descrittori precedenti non sono stati caricati
244400030210  "  C                   IF        DesOld = *blank
244500030210  "  C* allora li impongo = a quelli attuali
244600030210  "  C                   EVAL      DesOld = DesNew
244700030210D1270C                   ENDIF
244800020213     C*
244900020226     C* testo se la chiavi sono differenti
245000020304     C                   IF        KeyOld <> KeyNew
245100020212     C                   EVAL      KeyLgnModif = *on
245200020212     C                   ENDIF
245300030210D1270C*
245400030210  "  C* testo se i descrittori sono differenti
245500030210  "  C                   IF        DesOld <> DesNew
245600030210  "  C* parte personalizzabile:
245700030210  "  C*  non tutte le aziende avranno gli stessi descrittori e non
245800030210  "  C*  tutte saranno interessate ad avere una ricatalogazione per
245900030210  "  C*  il cambiamento degli stessi, per cui è qui che bisogna
246000030210  "  C*  personalizzare per attivare il comando suddetto a seconda
246100070215D1270C*  dei descrittori voluti
246200070215C2005C*  A std il test è su qualunque descrittore.
246300070215D1270C*  ATTENZIONE: stesso test su ANA672R
246400030210  "  C*                  IF        ClasseProJ = '020'
246500030210  "  C*                            and (DesNewLgn(3) <> DesOldLgn(3)
246600030210  "  C*                              or DesNewLgn(4) <> DesOldLgn(4)
246700030210  "  C*                              or DesNewLgn(5) <> DesOldLgn(5))
246800070214  "  C                   EVAL      DesLgnModif = *on
246900030210  "  C*                  ENDIF
247000030210D1270C                   ENDIF
247100051123D1901C*
247200051123  "  C* controllo esistenza rcd su ANLGN con la chiave precedente
247300051123  "  C*
247400051123  "  C                   EVAL      DocCatalogato = *off
247500051123  "  C                   MOVEA     KeyOldLgn     KeyLgn
247600051123  "  C                   EXSR      DriverLGN
247700051123  "  C*
247800051123  "  C* se non c'è il rcd
247900051123  "  C                   IF        DR17Errore = *on
248000051123  "  C*
248100051123  "  C* controllo esistenza rcd su ANLGN con la chiave attuale
248200051123  "  C* solo se la chiave è diversa dalla precedente
248300051123  "  C*
248400051123  "  C                   IF        KeyLgnModif = *on
248500051123  "  C*
248600051123  "  C                   MOVEA     KeyNewLgn     KeyLgn
248700051123  "  C                   EXSR      DriverLGN
248800051123  "  C*
248900051123  "  C* se c'è il rcd
249000051123  "  C                   IF        DR17Errore = *off
249100051123  "  C* memo che c'è qualcosa catalogato per la nuova chiave
249200051123  "  C                   EVAL      DocCatalogato = *on
249300051123  "  C                   ENDIF
249400051123  "  C*
249500051123  "  C                   ENDIF
249600051123  "  C*
249700051123  "  C* se c'è il rcd
249800051123  "  C                   ELSE
249900051123  "  C* memo che c'è qualcosa catalogato per la vecchia chiave
250000051123  "  C                   EVAL      DocCatalogato = *on
250100051123D1901C                   ENDIF
250200020212     C*
250300020212C1530C                   ENDSR
250400020226C1530C************************************************************
250500020226  "  C* memo chiavi dell'associazione tra registrazione e Laguna
250600020226  "  C* dopo la conferma della registrazione
250700020226  "  C************************************************************
250800020226C1530C     RieKeyNewLgn  BEGSR
250900030113     C*
251000030113     C                   MOVEA     *blank        KeyNewLgn
251100020226     C*
251200020226     C                   SELECT
251300020226     C*  Doc.IVA Fornitore : (F)attura o (N)ota/estremi doc./sottoconto
251400020304     C                   WHEN      D1SNatura = 'F'
251500021025D1215x* spostato tutto nelle rispettive routine
251600021025D1215C                   EXSR      IVAFor
251700020226     C*
251800020226     C*  Doc.IVA Cliente   : estremi doc.
251900020304     C                   WHEN      D1SNatura = 'C'
252000021025D1215x* spostato tutto nelle rispettive routine
252100021025D1215C                   EXSR      IVACli
252200020226     C*
252300020226     C                   ENDSL
252400020226     C*
252500020226C1530C                   ENDSR
252600021025D1215C*---------------------------------------------------------*
252700021025  "  C* IVACLI - Catalogazione documenti Iva Clienti
252800021025  "  C*---------------------------------------------------------*
252900021025D1215C     IVACLI        BEGSR
253000021025     C*
253100021025     C                   EVAL      ClasseProJ = '030'
253200021025     C*
253300021025     C* nuova chiave primaria di catalogazione ProJ
253400021025     C*
253500021025     C*  (la società non può cambiare)
253600021025     C                   MOVEL(P)  XSCSOC        KeyNewLgn(1)
253700021025     C                   MOVEL(P)  DtDoc039      KeyNewLgn(2)
253800021025     C                   MOVEL(P)  NrDocAlfa     KeyNewLgn(3)
253900021025     C                   MOVEL(P)  SerDoc039     KeyNewLgn(4)
254000021025     C*
254100021025     C* nuova chiave secondaria
254200021025     C*
254300021025     C*   Sistema
254400021025     C                   Z-ADD     Sys039        SysSigned
254500021025     C                   MOVEL     SysSigned     Key2NewLgn(1)
254600021025     C*   Nr.Assoluto Registrazione
254700021025     C                   Z-ADD     NrAsReg039    NrAsRegSigned
254800021025     C                   MOVEL     NrAsRegSigned Key2NewLgn(2)
254900021025     C*
255000021025     C* nuovi descrittori
255100021025     C*
255200021025     C*   Codice Cliente
255300021025     C                   eval      DesNewLgn(1) = Soggett039
255400021025     C*   Ragione Sociale Cliente
255500021025     C                   eval      DesNewLgn(2) = Soggd039
255600021025     C*   Partita Iva Cliente
255700021025     C                   eval      DesNewLgn(3) = Partiva039
255800030113     C*   Serie registrazione
255900030113     C                   eval      DesNewLgn(4) = SerReg039
256000021025     C*   Numero registrazione
256100021025     C                   MOVEL     NrReg039      DesNewLgn(5)
256200030113     C*   Data registrazione
256300030113     C                   MOVEL     DtReg039      DesNewLgn(6)
256400070213C2005C*   Codice fiscale
256500070213  "  C                   MOVE      'C'           DVASNatura
256600070907C2005C                   EXSR      RepDatiSog
256700070907D2224C*   Flag natura ordine, Agente 1 e descrizione agente 1
256800070907  "  C*   non ce li ho in primanota per cui lascio 3 buchi
256900070907  "  C*   Cod. fiscale
257000070907D2224C                   eval      DesNewLgn(10) = WCdFisc
257100070907D2224x***                EVAL      DesNewLgn(7) = WCdFisc
257200070213C2005C*
257300021025     C*
257400021025D1215C                   ENDSR
257500021025D1215C*---------------------------------------------------------*
257600021025  "  C* IVAFOR - Catalogazione documenti Iva Fornitori
257700021025  "  C*---------------------------------------------------------*
257800021025D1215C     IVAFOR        BEGSR
257900021025     C*
258000021025     C                   EVAL      ClasseProJ = '020'
258100060303D1937C*
258200060303  "  C* Bisogna sempre reperire la causale di testata per una corretta
258300060303  "  C* impostazione del flag tipo documento in laguna. Può succedere
258400060303  "  C* in alcuni casi che la causale di testa non venga decodificata,
258500060303  "  C* per cui il flag di tipo documento (F=fattura N=Nota) viene
258600060303  "  C* impostato a blanks, così viene richiesta una ricatalogazione in
258700060303  "  C* laguna e se confermata viene perso il legame con la contabilità
258800060303  "  C* Quindi, se non reperita la causale di testata o se diversa
258900060303  "  C* da quella effettiva
259000060303  "  C                   If        OpeCausale <> Causale039
259100060303  "  C* rifaccio il reperimento
259200060303  "  C                   Exsr      RepDatCaus
259300060303D1937C                   Endif
259400021025     C*
259500021025     C* nuova chiave primaria di catalogazione ProJ
259600021025     C*
259700021025     C*  (la società non può cambiare)
259800021025     C                   MOVEL(P)  XSCSOC        KeyNewLgn(1)
259900021025     C*    decido se fattura o nota
260000021025     C                   SELECT
260100021025     C*     fattura
260200021025     C                   WHEN      OpeTpDocI = '0' or
260300060303D1937C                             OpeTpDocI = '2' or
260400060303     C                             OpeTpDocI = '3' or
260500021025     C                             OpeTpDocI = '5' or
260600021025     C                             OpeTpDocI = '6'
260700021025     C                   MOVEL(P)  'F'           KeyNewLgn(2)
260800021025     C*     nota
260900021025     C                   WHEN      OpeTpDocI = '1' or
261000021025     C                             OpeTpDocI = '4' or
261100021025     C                             OpeTpDocI = '7'
261200021025     C                   MOVEL(P)  'N'           KeyNewLgn(2)
261300021025     C                   ENDSL
261400021025     C                   MOVEL(P)  DtDoc039      KeyNewLgn(3)
261500021025     C                   MOVEL(P)  NrDocAlfa     KeyNewLgn(4)
261600021025     C                   MOVEL(P)  SerDoc039     KeyNewLgn(5)
261700021025     C                   MOVEL(P)  Soggett039    KeyNewLgn(6)
261800021025     C*
261900021025     C* nuova chiave secondaria
262000021025     C*
262100021025     C*   Sistema
262200021025     C                   Z-ADD     Sys039        SysSigned
262300021025     C                   MOVEL     SysSigned     Key2NewLgn(1)
262400021025     C*   Nr.Assoluto Registrazione
262500021025     C                   Z-ADD     NrAsReg039    NrAsRegSigned
262600021025     C                   MOVEL     NrAsRegSigned Key2NewLgn(2)
262700021025     C*
262800021025     C* nuovi descrittori
262900021025     C*
263000021025     C*   Ragione Sociale Fornitore
263100021025     C                   eval      DesNewLgn(1) = Soggd039
263200021025     C*   Partita Iva Fornitore
263300021025     C                   eval      DesNewLgn(2) = Partiva039
263400030113     C*   Serie registrazione
263500030113     C                   eval      DesNewLgn(3) = SerReg039
263600021025     C*   Numero registrazione
263700021025     C                   MOVEL     NrReg039      DesNewLgn(4)
263800030113     C*   Data registrazione
263900030113     C                   MOVEL     DtReg039      DesNewLgn(5)
264000070213C2005C*   Codice fiscale
264100070214  "  C                   MOVE      'F'           DVASNatura
264200070213  "  C                   EXSR      RepDatiSog
264300070213  "  C                   EVAL      DesNewLgn(6) = WCdFisc
264400070213C2005C*
264500021025     C*
264600021025D1215C                   ENDSR
264700070213C2005C************************************************************
264800070213  "  C* Reperimento dati del soggetto e della sua natura (C/F)
264900070213  "  C************************************************************
265000070213C2005C     RepDatiSog    BEGSR
265100070213     C*
265200070213     C* richiamo il driver dei soggetti per recuperare i dati
265300070213     C* dell'indirizzo amministrativo (tipo/codice indirizzo contabile)
265400070213     C* e i dati della natura del conto
265500070213     C                   MOVE      '1'           DVARic
265600070213     C                   MOVE      XSCSOC        DVASocieta
265700070213     C                   MOVE      Soggett039    DVACodice
265800070213     C                   MOVEL     *blank        DVAStrutt
265900070213     C                   IF        DVASNatura = 'C'
266000070213     C                   MOVEL     'DVACLI'      DVAStrutt
266100070213     C                   EVAL      DVALenOut = %Size(DVACLI)
266200070213     C                   ELSE
266300070213     C                   MOVEL     'DVAFOR'      DVAStrutt
266400070213     C                   EVAL      DVALenOut = %Size(DVAFOR)
266500070213     C                   ENDIF
266600070213     C*
266700070213     C                   CALLB     'NDDVASOG'
266800070213     C                   PARM                    DVARIC
266900070213     C                   PARM                    DVASOCIETA
267000070213     C                   PARM                    DVAUNITA
267100070213     C                   PARM                    DVASTRUTT
267200070213     C                   PARM                    DVADTRIF
267300070213     C                   PARM                    DVALENOUT
267400070213     C                   PARM                    DVASNATURA
267500070213     C                   PARM                    DVACODICE
267600070213     C                   PARM      *blank        DVALINEAV
267700070213     C                   PARM      *zero         DVAFILIALE
267800070213     C                   PARM                    *omit
267900070213     C                   PARM                    *omit
268000070213     C                   PARM                    *omit
268100070213     C                   PARM                    DVAERRORE
268200070213     C                   PARM                    DVAOUTPUT
268300070213     C*
268400070213     C                   EVAL      WCdFisc = *blank
268500070213     C                   IF        DVAErrore<>*ON
268600070213     C                   IF        DVASNatura = 'C'
268700070213     C                   EVAL      %subst(DVACLI:1:DVALENOUT)
268800070213     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
268900070213     C                   EVAL      WCdFisc=DVCCdFisc
269000070213     C*
269100070213     C                   ELSE
269200070213     C                   EVAL      %subst(DVAFOR:1:DVALENOUT)
269300070213     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
269400070213     C                   EVAL      WCdFisc=DVFCdFisc
269500070213     C                   ENDIF
269600070213     C                   ENDIF
269700070213     C*
269800070213C2005C                   ENDSR
269900051123D1901C************************************************************
270000051123  "  C* reperimento dati sui documenti catalogati in Laguna
270100051123  "  C************************************************************
270200051123D1901C     DriverLGN     BEGSR
270300051123     C*
270400051123     C                   Clear                   DR17Dati
270500051123     C                   Clear                   ANLGN00F
270600051123     C*
270700051123     C* imposto le chiavi per fatture attive
270800051123     C                   EVAL      LGNClassPJ = ClasseProJ
270900051123     C* recupero il numero di chiavi
271000051123     C                   EXSR      RepNrChiavi
271100051123     C                   EVAL      LGNClassLg = 'DOC'
271200051123     C                   EVAL      LGNKey101 = KeyLgn(1)
271300051123     C                   EVAL      LGNKey102 = KeyLgn(2)
271400051123     C                   EVAL      LGNKey103 = KeyLgn(3)
271500051123     C                   EVAL      LGNKey104 = KeyLgn(4)
271600051123     C                   EVAL      LGNKey105 = KeyLgn(5)
271700051123     C                   EVAL      LGNKey106 = KeyLgn(6)
271800051123     C                   EVAL      LGNKey107 = KeyLgn(7)
271900051123     C                   EVAL      LGNKey108 = KeyLgn(8)
272000051123     C*
272100051123     C                   MOVEL     ANLGN00F      DR17Dati
272200051123     C                   eval      DR17Lunghezza = %size(ANLGN00F)
272300051123     C                   eval      DR17Errore    = *off
272400051123     C*
272500051123     C                   CALLB     'PJDR1701R'
272600051123     C                   parm      '01'          DR17Opzione                    1
272700051123     C                   parm                    DR17Errore                     2
272800051123     C                   parm      2             DR17Vista                      5
272900051123     C                   parm                    DR17Dati                       3
273000051123     C                   parm                    DR17Lunghezza                  4
273100051123     C                   parm      *off          DR17Commit                     6
273200051123     C                   parm                    DR17NrChiavi                   7
273300051123     C                   parm      *off          DR17Alloca                     8
273400051123     C                   parm                    *omit                          9
273500051123     C*
273600051123D1901C                   ENDSR
273700051123D1901C************************************************************
273800051123  "  C* Reperisco il numero di chiavi primarie della classe ProJ
273900051123  "  C************************************************************
274000051123D1901C     RepNrChiavi   BEGSR
274100051123     C*
274200051123     C* reperisco la tabella della classe ProJ
274300051123     C                   Eval      XTbRic = '1'
274400051123     C                   Eval      XTbAzi = XScSoc
274500051123     C                   Eval      XTbKey = *zero
274600051123     C                   Eval      XTbCod = '1L1'
274700051123     C                   Move      ClasseProJ    XTbKey
274800051123     C                   CALLB     'XATB'
274900051123     C                   PARM                    XATbDS
275000051123     C                   If        XTbErr = '0'
275100051123     C                   Eval      ANG1L1DS = XTbUni
275200051123     C* le chiavi possibili sono 8, per ogniuna deve essere indicato
275300051123     C* tracciato e campo di riferimento; partendo dall'ultima, la prima non
275400051123     C* vuota mi determina il numero delle chiavi
275500051123     C                   SELECT
275600051123     C                   WHEN      Trc1081L1 <> *blank
275700051123     C                   EVAL      DR17NrChiavi = 8
275800051123     C                   WHEN      Trc1071L1 <> *blank
275900051123     C                   EVAL      DR17NrChiavi = 7
276000051123     C                   WHEN      Trc1061L1 <> *blank
276100051123     C                   EVAL      DR17NrChiavi = 6
276200051123     C                   WHEN      Trc1051L1 <> *blank
276300051123     C                   EVAL      DR17NrChiavi = 5
276400051123     C                   WHEN      Trc1041L1 <> *blank
276500051123     C                   EVAL      DR17NrChiavi = 4
276600051123     C                   WHEN      Trc1031L1 <> *blank
276700051123     C                   EVAL      DR17NrChiavi = 3
276800051123     C                   WHEN      Trc1021L1 <> *blank
276900051123     C                   EVAL      DR17NrChiavi = 2
277000051123     C                   WHEN      Trc1011L1 <> *blank
277100051123     C                   EVAL      DR17NrChiavi = 1
277200051123     C* se chi ha modellato la tabella si è dimenticato di mettere i valori
277300051123     C* di tracciato e campo, non funzionerà nulla perchè le chiavi
277400051123     C* considerate saranno 0
277500051123     C                   OTHER
277600051123     C                   EVAL      DR17NrChiavi = 0
277700051123     C                   ENDSL
277800051123     C* se ci sono degli errori dichiaro che il numero delle chiavi è 0
277900051123     C                   ELSE
278000051123     C                   EVAL      DR17NrChiavi = 0
278100051123     C                   Endif
278200051123     C*
278300051123     C* alle chiavi reperite vanno sempre aggiunte le prime 2 della
278400051123     C* VL 02l e cioè LGNClassPJ / LGNClassLG
278500051123     C                   EVAL      DR17NrChiavi = DR17NrChiavi + 2
278600051123     C*
278700051123D1901C                   ENDSR
278800030113C1605C************************************************************
278900030113  "  C* REPERIMENTO PARAMETRI GESTIONE
279000030113  "  C************************************************************
279100030113C1605C     RepPar        BEGSR
279200030113     C*
279300030113     C                   CALLB     'XPAR'
279400030113     C                   PARM                    XPASOC            3
279500030113     C                   PARM                    XPAPAR            8
279600030113     C                   PARM                    XPAOUT
279700030113     C                   PARM                    XPAERR            1
279800030113     C                   PARM                    XPARIC            1
279900030113     C*
280000030113C1605C                   ENDSR
