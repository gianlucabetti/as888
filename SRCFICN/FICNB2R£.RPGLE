000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020122     H* ficnB2R *----------------------------------------------------*
000300020214     H*         - TASSAZIONE PADRONCINI
000400000000     H*--------------------------------------------------------------*
000500931015     FTABEL00F  IF   E           K DISK
000600011106     FTNTBE01L  IF   E           K DISK
000700020408     Ffifce01L  UF A E           K DISK    commit
000800040406     F*****fifceW1L  UF A E           K DISK    usropn commit
000900040406     FfifceW1L  UF A E           K DISK    usropn
001000090507     FfiTGT01L  IF   E           K DISK
001100011116     Ffifgt01L  IF   E           K DISK
001200020214     Ffifpt01L  IF   E           K DISK    PREFIX(F01:3)
001300020213     Ffifpt02L  IF   E           K DISK    rename(FIFPT000:FIFPT002)
001400011116     Ffifpd01L  IF   E           K DISK
001500020328     Ffiftdw1L  IF   E           K DISK
001600030320     Ffnarb01L  IF   E           K DISK
001700030320     Ffnblp01L  IF   E           K DISK
001800100423     D TSR             S              1    DIM(99)                              TIPI SERVIZIO VIZ
001900100423     D CTS             S              3    DIM(99)                              CDTAR TIPI SERVIZ
002000100423     D FTP             S              3    DIM(999)                              CONS PART     VIZ
002100100423     D USR             S              1    DIM(999)                              UTILIZZO "R"  VIZ
002200100423     D USC             S              1    DIM(999)                              UTILIZZO "C"  VIZ
002300100423     D TBA             S              1    DIM(999)                              base/accessori
002400100423     D SPM             S              1    DIM(999)                              più/meno
002500100423     D fc1             S              1    DIM(999)                              C/E consegna base
002600100423     D CE1             S              3    DIM(999)                              C/E consegna base
002700100423     D fc2             S              1    DIM(999)                              C/E consegna base
002800100423     D CE2             S              3    DIM(999)                              C/E ritiri   base
002900100423     D fc3             S              1    DIM(999)                              C/E consegna base
003000100423     D CE3             S              3    DIM(999)                              C/E consegna acc.
003100100423     D fc4             S              1    DIM(999)                              C/E consegna base
003200100423     D CE4             S              3    DIM(999)                              C/E ritiri   acc.
003300100423      *
003400100423     D UTI             S              1    DIM(999)                              UTILIZZO      VIZ
003500100423      *
003600100423     D TSP             S              1    DIM(99)                              TIPI SERVIZIO VIZ
003700100423     D TPT             S              3    DIM(99)                              TAR PART.X TSPVIZ
003800020426      *-  schiere per storno voci C/E in presenza di accessori in
003900020426      *-  sostituzione alla tariffa base.
004000020426     D iitt            S             10  3 DIM(99)                              Importo §TAITT
004100020426     D vitt            S              3    DIM(99)                              Importo §TAITT
004200020426     D itst            S             10  3 DIM(99)                              Importo §TATST
004300020426     D vtst            S              3    DIM(99)                              Importo §TATST
004400020426      *-
004500020125     D Se_Base         S              1                                         se Tar.Base S/N
004600020313     D Se_Access       S              1                                         se Tar.Base S/N
004700021212     D DiskC_ETC       S              1                                         su Ritiri
004800090914      *
004900090914     D   DCTP        E DS
005000090914     D KCliPar         S              7s 0 DIM(9999)                            Tar.Part.sul cliente
005100090914     D KCliParTSR      S              8a   DIM(9999)                            Tar.Part. + TSR
005200090914     D KTarStdNew      S              7A   DIM(9999)                            Tar.Part.+ Tar.Std.
005300090914     D KTarStd         S              4A   DIM(9999)                            Tar.da Sostituire
005400090914     D KTarNew         S              4A   DIM(9999)                            Tar.Sostituente
005500090914     D KTarObb         S              1A   DIM(9999)                            se Obbligatoria
005600090914     D KTarUgu         S              1A   DIM(9999)                            se Uguale come defin
005700090914     D KCliTrovato     S              7s 0 DIM(9999)                            Cliente Presente CTP
005800090914     D KStdNewCli      S             14A   DIM(9999) DESCEND                    Tar.STD+NEW+cliente
005900090915     D KTSRNewCli      S             11A   DIM(9999)                            Tar.TSR+NEW+cliente
006000090914     D KSTDDaExcl      S              4A   DIM(9999)                            Tar.già Sostituite
006100090914      *
006200090914     D tariffa_da_saltare...
006300090914     D                 S              1A
006400090914      *
006500090914     D Tar_da_sostituire...
006600090914     D                 S              1A
006700090914     d x_il_cliente    s              7s 0
006800090914      *
006900090914     D KtutteBolle     S              1A
007000090914      *
007100910423     D WLBDAT          DS
007200910423     D  G02DAT                 1      6  0
007300910423     D  G02INV                 7     12  0
007400910423     D  G02ERR                13     13
007500910423     D  G02TGI                14     18  0
007600910423     D WGIDAT          DS
007700910423     D  GIODAT                 1      6  0
007800910423     D  GIOINV                 7     12  0
007900910423     D  GIOTGI                13     17  0
008000950127     D                 DS
008100950127     D  W01CAP                 1      9
008200971001     D  WDSCAP                 3      5
008300020321      *  per campi relativi al bonus
008400020321     d dsbonus       e ds                  EXTNAME(DFPT01)
008500020125      *
008600020213     d   tar_valida    S              1    INZ('N')
008700020213      *
008800011106     D YEURDS        E DS                  EXTNAME(YEURCODS)
008900011106     D DGED          E DS
009000931022     D DSTR          E DS
009100931025     D DS1Z          E DS
009200931025     D DS8A          E DS
009300980224     D DS5E          E DS
009400020122     D ficnB2        E DS                  EXTNAME(ficnB2DS)
009500030108     D FIcnB3        E DS                  EXTNAME(FICNB3DS)
009600900521     C****************************************************************
009700900521     C*  RIEPILOGO INDICATORI
009800900521     C***************************************************************
009900931025     C* 01    - NON TASSABILI ACCESSORI PER MANCA TARIFFA
010000931025     C* 03    - SOSTITUZIONE DETTAGLIO TARIFFA CON TAR PARTICOLARI
010100931025     C* 04    - TARIFFA BASE GIA' ESISTENTE
010200000621     C* 51    - SOSTITUZIONE DETTAGLIO TARIFFA CON TAR PARTICOLARI (LASCIATO AVVISO/GIACENZA)
010300931022     C* 90    - ON INDICA CHE C'E' UN ERRORE IN TASSAZIONE
010400900521     C*****************************************************************
010500000000     C     *ENTRY        PLIST
010600020122     C                   PARM                    ficnB2
010700030527     C*
010800030527     C* Reset del Flag di lettura della tariffa Etichettatura x Disk C
010900030527     c                   clear                   DiskC_ETC
011000021212     C*
011100020215     C*  se chiamato solo per dare il manca tariffa
011200020215     C*   il programma va subito a fine.
011300020215     c                   if        §taFLG ='M'
011400020215     C                   movel     §taPDR        W01PDR
011500020215     C                   exsr      Manca_TAR
011600020215     C                   goto      fine
011700020215     C                   ENDIF
011800020215     C* o per chiudere
011900020215     c                   if        §taFLG ='C'
012000020215     C                   seton                                        LR
012100020215     C                   goto      fine
012200020215     C                   ENDIF
012300020215     C*
012400020215     C*  Normalmente esegue tutto da qui in avanti.
012500020213     C                   CLEAR                   W01SIA
012600020426     C                   Clear                   iitt
012700020426     C                   Clear                   vitt
012800020426     C                   Clear                   itst
012900020426     C                   Clear                   vtst
013000020426     C                   move      'N'           Sost_ITT          1
013100020426     C                   move      'N'           Sost_TST          1
013200020506      *
013300020506      * Per tariffa a giornata pulisce i campi di pck/eti
013400020506      *  e comunque pulisce i campi del calcolo del picking/etichettatura
013500020506     C                   clear                   w01tge
013600020506     C                   clear                   w01tgp
013700020506     C                   clear                   w01tpe
013800020214      *
013900020214     C* SE TAFGC E' UGUALE A J-GIACENZA O L-LASCIATO AVVISO NON CALCOLO TUTTE LE ALTRE
014000020214     C* TARIFFE PARTICOLARI   A PARTE INCASSI CONTRASSEGNO /ASSEGNATI/COMPETENZE
014100020215     C                   move      'N'           Giac_LAvv         1
014200020214     C     §TAFGC        IFEQ      'J'
014300020214     C     §TAFGC        OREQ      'L'
014400020215     C                   move      'S'           Giac_LAvv
014500020214     C                   ENDIF
014600020325      *
014700020325      * se Isola:
014800020325     c                   move      'N'           se_isola          1
014900020325     c     §tafin        Ifeq      'I'
015000020325     c     §tatsm        andeq     'S'
015100020325     c     §taiso        oreq      'S'
015200020325     c     §tatsm        andeq     'T'
015300020325     c                   move      'S'           se_isola          1
015400020325     c                   end
015500020130      *
015600030403      * In Conferma se a Prestazione Ritiri c'è solo una distinta x Ritiri Annullati
015700030403      *  imposta il flag per NON reperire tariffe a prestazione IAD o ETI
015800030403     c                   move      'N'           Solo_Rit_Ann      1
015900030403     c     §tactr        Ifeq      999
016000030403     c     §tatsr        andeq     'R'
016100030403     c     §tatbs        andgt     0
016200030403     c     §takgr        andeq     0
016300030403     c     §tapgm        andeq     'CONFERM'
016400030403     c                   move      'S'           Solo_Rit_Ann
016500030403     c                   End
016600030403      *
016700020130     C                   clear                   UTI
016800020215     C     §taTSR        IFEQ      'R'
016900020130     C                   MOVEA     USR           UTI                             RITIRI
017000020130     C                   ELSE
017100020130     C                   MOVEA     USC           UTI                             CONSEGNE
017200020130     C                   ENDIF
017300020215     C*
017400020215      *  Prende la Tariffa Principale
017500020215      *    dalla Testata FIFGT
017600020215      *               *-------------------------*
017700020225     C                   exsr      Tariffa_Princ
017800020215      *               *-------------------------*
017900020213      *
018000030529      * Voglio sapere in anticipo se c'è qualche tariffa in Particolare
018100030529      *               *-------------------------*
018200030529     C                   exsr      Tariffe_Part
018300030529      *               *-------------------------*
018400030529      *
018500020213      *    ----------------------------------------------
018600020213      * Una volta presa la tariffa di testata principale entro in
018700020213      *   dettaglio dei singoli codici tariffe.
018800020213      *    ----------------------------------------------
018900020215      * Chiave di lettura
019000020215     C                   eval        k02PDR  =   fgtPDR
019100020215     C                   eval        k02SML  =   fgtSML
019200020215     C                   eval        k02TSR  =   fgtTSR
019300020215     C                   eval        k02CTR  =   fgtCTR
019400020215     C                   eval        k02PRG  =   fgtPRG
019500020215     C                   eval        k02TSM  =   §taTSM
019600020213      *
019700020214      * Leggendo per livello di Tipo Somma
019800020213     C     kfpt02        setll     fifpt002
019900020213     C     kfpt02        reade     fifpt002
020000020213      *
020100020214     c                   dow       not %EOF(fifpt02l)
020200090914     C*
020300090914     C*   Attenzione se ci sono delle tariffe particolari sul cliente
020400090914     C*    che devono essere sostituite con altre.....
020500090914     c                   if        fptatb =' '
020600090914     c                   eval      tariffa_da_saltare = *blank
020700090914     C*
020800090914     C*  controlla se deve ignorare la tariffa
020900090914     C                   exsr      Ignora_Tariffa
021000090914     C*
021100090914     C                   end
021200020215      *
021300020213      *  solo quelle valide
021400020213     c     fptatb        ifeq      *blank
021500090914     c                   IF        tariffa_da_saltare = *blank
021600020215     C*
021700090914     C*
021800020215     C*  Imposta il Peso Tassabile
021900090928     C                   EXSR      moltiplicatore
022000090928      *
022100020215     C*  segno in tab.8A x somma algebrica
022200020322     C                   Exsr      Tab_8A
022300020213      *
022400020213     c                   select
022500020215      * Tariffa Base
022600020213     c                   When      fptCTD = 'BAC' or
022700020213     c                             fptCTD = 'BAR'
022800090915      ***
022900020213      * solo se Sped.da Pagare
023000020325      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
023100020419      *  e non è un solo incasso
023200030527      *  e non è una bolla rec.C/A (FW)
023300030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
023400020213     c     §TAPAG        ifeq      'S'
023500020605     c**** se_isola      andeq     'N'
023600020422     c     §tasic        andne     'S'
023700020514     C     Giac_LAvv     andeq     'N'
023800030821     c     Bolla_FW      andeq     *blank
023900020213     c                   exsr      Base
024000020213     c                   endif
024100020213      *
024200020215      * Tariffa Stop
024300030902     C                   when      fptCTD = 'STC' or
024400090915     c                             fptCTD = 'STT' or
024500030902     c                             fptCTD = 'STR' and Bolla_2R = *blank or
024600030901     c                             fptCTD = 'RCC' and Bolla_2R = 'S'
024700090914      *
024800020215      * solo se Sped.da Pagare e Stop validi
024900020325      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
025000020419      *  e non è un solo incasso
025100030527      *  e non è una bolla rec.C/A (FW)
025200030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
025300020213     c     §TAPAG        ifeq      'S'
025400020215     c     §taSPP        andgt     0
025500020605     c**** se_isola      andeq     'N'
025600020422     c     §tasic        andne     'S'
025700020514     C     Giac_LAvv     andeq     'N'
025800030821     c     Bolla_FW      andeq     *blank
025900020213     c                   exsr      Stop
026000020213     c                   endif
026100020213      *
026200020215      * Tariffa Pick/Etic
026300020213     C                   when      fptCTD = 'PCK' or
026400021212     c                             fptCTD = 'ETI' or
026500021212     c                             fptCTD = 'ETC'
026600030403      *
026700030403      * Quando si è a prestazione di Ritiro R999 e ci sono solo Ritiri Annullati
026800030403      *  non deve prendere la prestazione di Etichettatura
026900030527      *  o non è una bolla rec.C/A (FW)
027000030821      *  o non è una bolla di Ritiro Contestuale alla Consegna (2R)
027100030403     c                   if        Solo_Rit_Ann = 'S' and fptCTD = 'ETI'
027200030527     c                             or Bolla_FW  = 'S'
027300030403     c                   goto      No_Tar_ETI
027400030403     c                   end
027500030403      *
027600021212      * Attenzione:
027700021212      *  sui Ritiri per il cliente abilitato al Disk C (tramite il codice
027800021212      *  trattamento Merce in Bolla) deve valorizzare con la Tariffa "ETC"
027900021212      *  al posto della Tariffa "ETI" (in alternativa) e se non fosse presente
028000021212      *  la tariffa per il Disk C allora deve valorizzare con la normale "ETI".
028100021212      * Per tutti gli altri casi sempre con la normale "ETI".
028200021216      *   E'importantissimo che le 2 tariffe ETC e ETI siano ordinate altrimenti
028300021216      *   il test non può funzionare. Prima deve leggere ETC e in seguito ETI.
028400030527      * Attenzione:
028500030527      *  Variazione Importante alla regola: quando è un disk C, e quindi non etichettato
028600030527      *  dal cliente, deve comunque essere pagato l'etichettatura al padroncino anche se
028700030527      *  questi in testata aveva "N" di etichettatura. (Quindi gli verranno pagati sempre
028800030527      *  i diskC) -> entrerà in vigore da giugno 2003
028900021212     c                   if        §taTSR = 'C' or
029000030217     c                             §taTSR = 'G' or
029100021219     c                             §taTSR = 'R' and fptCTD = 'ETC' and
029200021219     c                             §taDKC = 'S'                    or
029300021212     c                             §taTSR = 'R' and fptCTD = 'ETI' and
029400021212     c                                DiskC_ETC <> 'S'
029500020215      * solo se da Pagare
029600020419      *  e non è un solo incasso
029700020215     c     §taPPE        ifeq      'S'
029800020513     c     §tatsr        andne     'G'
029900021212      *
030000030527      * oppure da giugno 2003 basta che sia un Disk C x etichettatura da dare
030100030527      * al padroncino
030200030527     c     §taDKC        oreq      'S'
030300030527     c     §tatsr        andeq     'R'
030400030527      *
030500020506     c     §tapgp        oreq      'S'
030600020507     c     fptCTD        andeq     'PCK'
030700020513     c     §tatsr        andeq     'G'
030800021212      *
030900020506     c     §tapge        oreq      'S'
031000020507     c     fptCTD        andeq     'ETI'
031100020513     c     §tatsr        andeq     'G'
031200020506      *
031300020506     c     §tasic        ifne      'S'
031400021212      *
031500021216     c                   if        fptCTD = 'ETC'
031600021212      * Per memorizzare che era presente la tariffa x etic. Disk C
031700021212      *  e non permettere il calcolo quindi con la tariffa "ETI"
031800021212      *   se presenti entrambe.
031900021212     c                   move      'S'           DiskC_ETC
032000021212     c                   end
032100021212      *
032200020213     c                   exsr      Pck_Etic
032300020215     c                   endif
032400020506      *
032500020506     c                   endif
032600021212      *
032700021212     c                   endiF
032800030403      *
032900030403     c     No_Tar_ETI    tag
033000020325      *----------------
033100020325      * Tariffa  Espresso Carriaggio di consegna
033200030527      *  e non è una bolla rec.C/A (FW)
033300030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
033400030527     C                   when      fptCTD = 'ECA' and
033500030901     c                             Bolla_FW = *blank
033600020325     C     §taXPR        ifeq      'S'
033700020325     C     §tatsm        andeq     'T'
033800020325     C     §taTSP        oreq      'E'
033900020325     C     §tatsm        andeq     'S'
034000020325     C*
034100091008     C     §taTSP        oreq      'H'
034200091008     C     §tatsm        andeq     'S'
034300091008     C*
034400020419      * solo se Sped.da Pagare e Stop validi
034500020419      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
034600020419      *  e non è un solo incasso
034700020325     c     §TAPAG        ifeq      'S'
034800020605     c**** se_isola      andeq     'N'
034900020422     c     §tasic        andne     'S'
035000020325     c                   exsr      Base_eca
035100020325     c                   endif
035200020325     C*
035300020325     C                   end
035400020325      *
035500020325      * Tariffa  Espresso Stop di consegna
035600030527      *  e non è una bolla rec.C/A (FW)
035700030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
035800030527     C                   when      fptCTD = 'EST' and
035900030901     c                             Bolla_FW = *blank
036000020325     C     §taXPR        ifeq      'S'
036100020325     C     §tatsm        andeq     'T'
036200020325     C     §taTSP        oreq      'E'
036300020325     C     §tatsm        andeq     'S'
036400020325     C*
036500091008     C     §taTSP        oreq      'H'
036600091008     C     §tatsm        andeq     'S'
036700091008     C*
036800020419      * solo se Sped.da Pagare e Stop validi
036900020419      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
037000020419      *  e non è un solo incasso
037100020325     c     §TAPAG        ifeq      'S'
037200020325     c     §taSPP        andgt     0
037300020605     c**** se_isola      andeq     'N'
037400020422     c     §tasic        andne     'S'
037500020325     c                   exsr      Stop_est
037600020325     c                   endif
037700020325     C*
037800020325     C                   end
037900020321      * ------------
038000030109     C* Tariffa Destinatari disagiati (Maggiorazione Carriaggio)
038100030527      *  e non è una bolla rec.C/A (FW)
038200030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
038300030527     C                   when      fptCTD = 'XC ' and
038400030901     c                             Bolla_FW = *blank
038500030109      *
038600030109      *  Se è impostato a Stop come per la maggiorazione a Stop
038700030109      *   deve avere le spedizioni all'interno dello Stop
038800030109      *  tutte omogenee alrimenti non da la maggiorazione.
038900030109     C                   If        §tatsm = 'S' or
039000030109     C                             §tatsm = 'T' and §TADED = 'S'
039100030109      *
039200030109      * se Bolla con segnalazione Dest.Disagiato
039300030109     C* comunque se impostata "X" in uno dei 2 campi la data Consegna
039400030109     C*  su ARBDCM è > 0 come controlla il FICNB1R prima di passare i parametri
039500030109     C                   If        §TATC1 = 'X' or §TATC2 = 'X'
039600030109     C*
039700030109      * solo se Sped.da Pagare
039800030109      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
039900030109      *  e non è un solo incasso
040000030109     c     §TAPAG        ifeq      'S'
040100030109     c**** se_isola      andeq     'N'
040200030109     c     §tasic        andne     'S'
040300030109     C     Giac_LAvv     andeq     'N'
040400030109      * Maggiorazione Carriaggio
040500030109     c                   exsr      Base_Disag
040600030109     c                   end
040700030109     C                   endIF
040800030109      *
040900030109     C                   endIF
041000030109      *
041100030109      * ------------
041200030109      * Tariffa  Stop Destinatari Disagiati (Maggiorazione Stop)
041300030527      *  e non è una bolla rec.C/A (FW)
041400030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
041500030527     C                   when      fptCTD = 'XS ' and
041600030901     c                             Bolla_FW = *blank
041700030109      *  Essendo a Stop
041800030109      *   deve avere le spedizioni all'interno dello Stop
041900030109      *  tutte omogenee alrimenti non da la maggiorazione.
042000030109     C                   If        §tatsm = 'T' and §TADED = 'S'
042100030109      *
042200021223      * se Bolla con segnalazione Dest.Disagiato
042300030109     C* comunque se impostata "X" in uno dei 2 campi la data Consegna
042400030109     C*  su ARBDCM è > 0 come controlla il FICNB1R prima di passare i parametri
042500021223     C                   If        §TATC1 = 'X' or §TATC2 = 'X'
042600030109      *
042700021223      * solo se Sped.da Pagare e Stop validi
042800021223      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
042900021223      *  e non è un solo incasso
043000021223     c     §TAPAG        ifeq      'S'
043100021223     c     §taSPP        andgt     0
043200021223     c**** se_isola      andeq     'N'
043300021223     c     §tasic        andne     'S'
043400021223     C     Giac_LAvv     andeq     'N'
043500030109      * Maggiorazione Stop
043600030109     c                   exsr      Stop_disag
043700021223     c                   endif
043800021223      *
043900021223     c                   endIF
044000030109      *
044100030109     c                   endIF
044200021223      * ------------
044300030527      * Tariffa Bonus
044400030402      *  può dare il Bonus solo in presenza di consegne.
044500030527     C                   when      fptCTD = 'BON'
044600030410     c********                     and §takgc > 0
044700020215      *  solo se c'è un Bonus da attribuire
044800020215     c     FptATA        ifgt      0
044900020213     c                   exsr      Bonus
045000020215     c                   end
045100020213      *
045200020215      * Tariffa Minimo Garantito
045300030527     C                   when      fptCTD = 'MGA'
045400020312    4C     §TAtsr        IFEQ      'G'
045500020215     c     Piumen        ifeq      '-'
045600091202     C                   z-sub(h)  fptata        §TAMNT
045700020215     c                   else
045800091202     C                   z-add(h)  fptata        §TAMNT
045900020215     c                   end
046000020312    4C                   ENDIF
046100020213      *
046200020215      * Tariffa Giornata Intera
046300020213     C                   when      fptCTD = 'TIG'
046400020312    4C     §TAFGR        IFEQ      'I'
046500020312    4C     §TAtsr        andeq     'G'
046600020215     c     Piumen        ifeq      '-'
046700091202     C                   z-sub(h)  fptata        §TATIG
046800020215     c                   else
046900091202     C                   z-add(h)  fptata        §TATIG
047000020215     c                   end
047100020312    4C                   ENDIF
047200030527      *
047300020215      * Tariffa Mezza Giornata
047400020213     C                   when      fptCTD = 'TMG'
047500020312    4C     §TAFGR        IFEQ      'M'
047600020312    4C     §TAtsr        andeq     'G'
047700020215     c     Piumen        ifeq      '-'
047800091202     C                   z-sub(h)  fptata        §TATIG
047900020215     c                   else
048000091202     C                   z-add(h)  fptata        §TATIG
048100020215     c                   end
048200020312    4C                   ENDIF
048300020213      *
048400020215      * Tariffa Importo Addizionale
048500020213     C                   when      fptCTD = 'IAD'
048600030403      * A Prestazione se sono solo ritiri annullati deve non prendere
048700030403      *  la tariffa Addizionale.
048800030403     c     Solo_Rit_Ann  ifeq      'S'
048900030403     c                   goto      No_tar_IAD
049000030403     c                   endIF
049100020312    4C     §TAtsr        IFNE      'G'
049200020215     c     Piumen        ifeq      '-'
049300091202     C                   z-sub(h)  fptata        §TAIAD
049400091202     c                   sub(h)    fptata        vocice
049500020215     c                   else
049600091202     C                   z-add(h)  fptata        §TAIAD
049700091202     c                   add(h)    fptata        vocice
049800020215     c                   end
049900020312     c                   END
050000030403     c     No_tar_IAD    tag
050100020213      *
050200020213     c                   other
050300020215      * Tariffa Accessori
050400020311     c     §taita        ifne      *all'9'
050500020606     C* oppure accessori particolari come la maggiorazione dello stop oltre i 5 kg.
050600020604     c     fptctd        oreq      'X  '
050700090915     c     fptctd        oreq      'XT '
050800020320     C*
050900020320     C* inizializza il flag di calcolo accessori
051000020320     C                   move      'N'           esegui            1
051100020320      *
051200030527      *  Solo Recupero C/A
051300030527     c     Bolla_FW      ifeq      'S'
051400030527     c                   exsr      Recup_Cas
051500030527     c                   else
051600030527      *  Solo Incasso
051700020320     c     §tasic        ifeq      'S'
051800020320     c                   exsr      Solo_Incasso
051900020320     c                   else
052000030527      *  Accessori
052100020320     c                   exsr      Accessori
052200030821     c                   end
052300030527     c                   end
052400020322      *
052500020320      * se deve eseguire il calcolo dell'accessorio
052600020320     C     Esegui        ifeq      'S'
052700020322     C     UtiOK         andeq     'S'
052800020322     C                   EXSR      Calc_Access
052900020320     C                   End
053000020320      *
053100020311     c                   end
053200020213      *
053300020213     c                   endsl
053400020326      *
053500020326      *  Non deve aggiornare se in simulazione
053600030108      *   Prepara i dati per il FIFCE per comparazione al Conto Economico
053700020326     c                   if        §tasml=' '
053800030108     c                   Exsr      xContoEcon
053900020403     c                   end
054000020326      *
054100090914     c                   endIF
054200020327     c                   end
054300020213     C     kfpt02        reade     fifpt002
054400020213     c                   enddo
054500030110     C*    ----------------------------------------------
054600020426      *
054700020426      *  A fine ciclo occorre vedere se sono avvenute delle sostituzioni
054800020426      *   della tariffa base da parte di alcune tariffe accessorie e
054900020426      *    di conseguenza occorre riadattare i valori di conto economico
055000020426     C     Sost_ITT      ifeq      'S'
055100020426      *              *-------------------------*
055200020426     c                   exsr      Sost_FIFCE
055300020426      *              *-------------------------*
055400020426     c                   end
055500030110      *
055600030110     C*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
055700030108     C* Prima di uscire, controlla le Eccezioni:
055800030108      *
055900030108     C*   Se ci sono da valorizzare dei Ritiri Annullati.
056000030108      *    a tempo di totali a prestazione ovviamente in CONFERMA
056100030108     C                   If        §taTBS > 0 and §taTSR = 'R' and
056200030108     C                             §taCTR = 999 and §tapgm = 'CONFERM'
056300030110     C*
056400030110     C*            >>-->>-*-*-*-*-*-*-*-*-*-*-<<--<<
056500030108     c                   Exsr      Ritiri_Ann
056600030110     C*            >>-->>-*-*-*-*-*-*-*-*-*-*-<<--<<
056700030108     C*
056800030108     c                   EndIf
056900030110     C*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
057000030108     C*
057100020226     c                   if        §taitt = *all'9'
057200020226     c                   if        §tatpe > 0 or §tatst > 0
057300020226     c                   eval      §taitt = 0
057400020226     c                   end
057500020226     c                   end
057600020225      *
057700020213     C     fine          tag
057800020213     C                   RETURN
057900020215     C*================================================================
058000020215     C* VEDO SE MANCA TARIFFA                                         *
058100020215     C*================================================================
058200020215     C     Manca_TAR     BEGSR
058300020215     C*
058400020215     C* RICERCO TARIFFA A GIORNATA
058500020215     C                   Z-ADD     1             C
058600020215     C     'G'           LOOKUP    TSR(C)                                 30
058700020215     C  N30              MOVEL     999           W01CTR
058800020215     C   30              MOVEL     CTS(C)        W01CTR
058900020215     C                   MOVEL     'G'           W01TSR
059000020215     C*               *-------------------------*
059100020215     C                   EXSR      Ric_Tariffa
059200020215     C*               *-------------------------*
059300020215     C*
059400020215     C* VERRA' SEGNALATO "MANCA TARIFFA" SE:
059500020215     C*                   -------------
059600020215     C* 1) NON C'E TARIFFA A GIORNATA
059700020215     C* 2) C'E' TARIFFA A GIORNATA CON IL MINIMO ESPRESSO
059800020215     C* 3) C'E' TARIFFA A GIORNATA MA GLI IMPORTI TARIFFA INT/MEZZA
059900020215     C*    GIORNATA E MINIMO SONO VUOTI
060000020215     C*
060100020215     C     tar_valida    ifeq      'N'
060200020215     C                   MOVEL     'E'           §TAERR
060300020215     C                   else
060400020215     C*
060500020215     C* ricerca valori Tariffa a Giornata
060600020215     C                   EXSR      Tar_Giornata
060700020215     C*
060800020215     C     w01mnt        IFgt      0
060900020215     C     w01tmg        OReq      0
061000020215     C     w01tig        ANDeq     0
061100020215     C                   eval      tar_valida = 'N'
061200020215     C                   MOVEL     'E'           §TAERR
061300020215     C                   endif
061400020215     C*
061500020215     C                   end
061600020215     C*
061700020215     C                   ENDSR
061800020213     C*================================================================
061900020215     C* RICERCO TARIFFA                                               *
062000020213     C*================================================================
062100020215     C     Tariffa_Princ BEGSR
062200020215      *
062300020215      * Prende la Tariffa valida impostando il progressivo
062400020215      * e se la tariffa non è valida SEGNALA MANCA TARIFFA e va a fine pgm
062500020215     C                   movel     §taPDR        W01PDR
062600020215     C                   movel     §taCTR        W01CTR
062700020215     C                   movel     §taTSR        W01TSR
062800020215      *               *-------------------------*
062900020215     C                   exsr      Ric_Tariffa
063000020215      *               *-------------------------*
063100020319     C     Tar_valida    ifeq      'N'
063200020319     C     §tactr        andeq     999
063300030331      *
063400030331      * In eccezione x i ritiri annullati:
063500030331      *  Se non esiste la tariffa a prestazione e stiamo prendendo in esame
063600030331      *   dei ritiri in cui ci sono solo dei ritiri annullati, eseguo solo.
063700030331      *    la Routine per i ritiri annullati (per prenderne il valore) poi
063800030331      *     comunque esco come faceva prima il programma.
063900030331     C                   If        §taTBS > 0 and §taTSR = 'R' and
064000030331     C                             §taCTR = 999 and §tapgm = 'CONFERM'
064100030331     C*            >>-->>-*-*-*-*-*-*-*-*-*-*-<<--<<
064200030331     c                   Exsr      Ritiri_Ann
064300030331     C*            >>-->>-*-*-*-*-*-*-*-*-*-*-<<--<<
064400040506      *
064500040506      * Prima controlla che tutto sia OK dopo i ritiri annullati
064600040506     C     Tar_valida    ifeq      'N'
064700040510     C****               MOVEL     'E'           §TAERR
064800040506      * imposta A = Manca tariffa ritiri Annullati
064900040510     C*****              MOVEL     'A'           §TAter
065000040506     C                   ENDIF
065100040506      *
065200030331     c                   EndIf
065300030331      *  dopo esce
065400020319     c                   goto      fine
065500020319     C                   ENDIF
065600020319      *
065700020215     C     Tar_valida    ifeq      'N'
065800020215     C                   exsr      Manca_TAR
065900020215     C     §taerr        cabEQ     'E'           fine
066000020215     C                   ENDIF
066100020215      *
066200020215     C* SE C'E' RIFERIMENTO ALTRA TARIFFA USO QUESTO PER IL DETTAGLIO
066300020215    1C     FGTRAP        IFGT      0
066400020215     C                   Z-ADD     FGTRAP        W01PDR
066500020215     C                   Z-ADD     FGTRCT        W01CTR
066600020215      *               *-------------------------*
066700020215     C                   EXSR      Ric_Tariffa
066800020215      *               *-------------------------*
066900020215    2C     Tar_valida    ifeq      'N'
067000020215     C                   exsr      Manca_TAR
067100020215     C     §taerr        cabEQ     'E'           fine
067200020215    2c                   end
067300020215    1C                   ENDIF
067400020215     C*
067500020215     C                   ENDSR
067600020215     C*================================================================
067700020215     C* RICERCO PROGRESSIVO TARIFFA VALIDO                            *
067800020215     C*================================================================
067900020215     C     Ric_Tariffa   BEGSR
068000020215     C*
068100020213     C                   CLEAR                   WALF3
068200020213     C*
068300031024     C     riprova       tag
068400020215     C     KFGT          chain     fifgt000                           30
068500091112     C*
068600041213      ******* Era errato averlo:qui occorre spostarlo all'interno del ciclo
068700041213      *******  in questo punto veniva testata solo la prima tariffa a prescindere
068800041213      *******  dalle date di scadenza tariffa poi sporcava irreparabilmente
068900041213      *******  la chiave di ricerca tariffa
069000041213      * se ho tariffa a giornata riferita ad altro padroncino
069100041213    1C**N30FGTRAP        IFGT      0
069200041213     c*****§taTSR        andeq     *blank
069300041213     c*****§taCTR        andeq     *zeros
069400041213     c*****§taAAS        andeq     *zeros
069500041213     c*****§taLNP        andeq     *zeros
069600041213     c*****§taNRS        andeq     *zeros
069700041213     c*****§taNSP        andeq     *zeros
069800041213     C*******            Z-ADD     FGTRAP        W01PDR
069900041213     C*******            Z-ADD     FGTRCT        W01CTR
070000041213     c*******            goto      riprova
070100041213     c*******            end
070200041213     C*******
070300020213    1C     *in30         DOWeq     *off
070400020213     C*
070500020213     C* TARIFFA VALIDA:
070600020213     C*  Non deve essere annullata e deve avere la divisa richiesta
070700020213      **   Data Documento deve essere compresa fra le date di attivazione e
070800020213      ***   scadenza e deve essere stata stampata (quindi convalidata)
070900020213      ****   >altrimenti è un Manca Tariffa<
071000020213    2C     FGTATB        ifEQ      ' '
071100020213     c     §tadiv        andeq     fgtdiv
071200091112      *
071300091112     C* Se tariffa del periodo
071400091112     C     Ktgt          CHAIN     FItgt01l
071500091112 2/3 c                   if        %Found(FItgt01l) and
071600091112     c                             §TADDC >= tgtDDT and §TADDC <= tgtDST
071700091112     *****
071800091112     C**** §TADDC        IFge      FGTDDT
071900091112     C**** §TADDC        andLE     FGTDST
072000091112     *****
072100091112   3 c     §tasml        IFeq      *blank
072200091112     C     tgtDTS        aNDgt     0
072300091112     C**** fgtDTS        aNDgt     0
072400091112     c     §tasml        OReq      'S'
072500020417      *
072600041213      * Il controllo viene portato all'interno del ciclo per gestire
072700041213      * correttamente la tariffa di riferimento
072800041213      * se ho tariffa a giornata riferita ad altro padroncino
072900091112    4C  N30FGTRAP        IFGT      0
073000041213     c     §taTSR        andeq     *blank
073100041213     c     §taCTR        andeq     *zeros
073200041213     c     §taAAS        andeq     *zeros
073300041213     c     §taLNP        andeq     *zeros
073400041213     c     §taNRS        andeq     *zeros
073500041213     c     §taNSP        andeq     *zeros
073600041213     C                   Z-ADD     FGTRAP        W01PDR
073700041213     C                   Z-ADD     FGTRCT        W01CTR
073800041213     c                   goto      riprova
073900091112    4c                   end
074000041213      *
074100020213     C                   MOVEL     FGTPRG        WALF3             3
074200020213     C                   leave
074300091112      *
074400091112   3 C                   end
074500091112     C                   end
074600091112   2 C                   end
074700091112     C*
074800020213     C  N30KFGT          READE     fifgt000                               30
074900091112   1 C                   ENDDO
075000020213     C*
075100020215     C*  TARIFFA INESISTENTE O ANCORA DA DECORRERE - 90 ON -
075200020213     C     WALF3         ifeq      *BLANKS
075300020213     C                   SETON                                        90
075400020213     C                   else
075500020213     C                   MOVEL     WALF3         W01PRG
075600020213     C     KFGT2         CHAIN     fifgt000                           90
075700020213     C                   end
075800020213     C*
075900020213     C* se ha trovato la tariffa valida --> 90 Off
076000020213     C                   move      'N'           tar_valida
076100020213     C  n90              move      'S'           tar_valida
076200020213     C  n90              movel     fgtprg        W01PRG
076300020213     C*
076400090507     C*  se ha trovato la tariffa aggancia la Supertestata
076500090507     C*   per reperire nuove informazioni come la società e il Fornitore
076600091112     C                   clear                   §taSOC
076700091112     C                   clear                   §taCDF
076800090507     C                   if         tar_valida = 'S'
076900090507     C     Ktgt          CHAIN     fitgt01l
077000090507     c                   if        %Found(fitgt01l)
077100090507     C                   move      tgtSOC        §taSOC
077200090507     C                   move      tgtCDF        §taCDF
077300090507     C                   end
077400090507     C*
077500090507     C                   end
077600090507     C*
077700020213     C                   ENDSR
077800020215     C*================================================================
077900020215     C* ricerca valori Tariffa a Giornata                             *
078000020215     C*================================================================
078100020215     C     Tar_Giornata  BEGSR
078200020215     C*
078300020215     C*  per la tariffa a Giornata la tariffa è una "G/999"
078400020215     C                   Z-ADD     1             C
078500020215     C     'G'           LOOKUP    TSR(C)                                 30
078600020215     C  N30              MOVEL     999           W01CTR
078700020215     C   30              MOVEL     CTS(C)        W01CTR
078800020215     C                   MOVEL     'G'           W01TSR
078900020215     C*
079000020215     C* >>>>>>>>>>>> se non c'è in testata FPT il minimo garantito  "MGA"
079100020215     C*  <<<<<<<<<    se non c'è  "  "    "    la mezza giornata    "TMG"
079200020215     C*  <<<<<<<<<    se non c'è  "  "    "    la giornata intera   "TIG"
079300020215     C* >>>>>>>>>>>> tutti i valori si trovano sul campo FPTATA di testata.
079400020215     C*
079500020215     C* Minimo Garantito
079600020215     C                   z-add     0             w01mnt
079700020215     C                   movel     'MGA'         w01ctd
079800020215     C     KFPT00        chain     fifpt01L
079900020215     C                   if        %found(fifpt01L) and F01atb=*blank
080000020215     C                   z-add     F01ata        w01mnt
080100020215     c                   end
080200020215     C*----------
080300020215     C* Tariffa Mezza  Giornata
080400020215     C                   z-add     0             w01tmg
080500020215     C                   movel     'TMG'         w01ctd
080600020215     C     KFPT00        chain     fifpt01L
080700020215     C                   if        %found(fifpt01L) and F01atb=*blank
080800020215     C                   z-add     F01ata        w01tmg
080900020215     c                   end
081000020215     C*----------
081100020215     C* Tariffa Giornata Intera
081200020215     C                   z-add     0             w01tig
081300020215     C                   movel     'TIG'         w01ctd
081400020215     C     KFPT00        chain     fifpt01L
081500020215     C                   if        %found(fifpt01L) and F01atb=*blank
081600020215     C                   z-add     F01ata        w01tig
081700020215     c                   end
081800020215     C*----------
081900020215     C                   ENDSR
082000020213     C*================================================================
082100020213     C* Imposta la Tariffa Base                                       *
082200020213     C*================================================================
082300020213     C     Base          BEGSR
082400020213     C*
082500020213     C* SALVO APPLICAZIONE TARIFFA FINITA ora sulla testata particolare
082600020213     C*  della BAC/BAR in altri casi il campo contiene altri valori.
082700091202     C                   z-add(h)  fptata        w01ata
082800020213      *
082900020213      *  esce da questa routine impostando il valore, se trovato, "w01imp".
083000020213      *   prima di essere chiamata deve essere impostato il codice dettaglio
083100020213      *    su cui ricercare la tariffa e quindi l'importo w01imp.
083200020213      *
083300020213      *  Cerca quindi la tariffa di dettaglio con il giusto CAP e scaglione
083400020213     c                   eval      Se_Base ='S'
083500020213      *             *---------------------------*
083600020215     C                   exsr      Tar_Base
083700020213      *             *---------------------------*
083800020514     C*
083900020514     c                   if        §taitt = *all'9' and w01imp<>0
084000020514     c                   eval      §taitt = 0
084100020514     c                   end
084200020213     C*
084300020213      *  ----> quindi imposta il risultato ottenuto
084400020213     c     Piumen        ifeq      '-'
084500091202     c****               z-sub(h)  w01imp        §taitt
084600091202     c                   sub(h)    w01imp        §taitt
084700091202     c                   sub(h)    w01imp        vocice
084800020213    1C                   else
084900091202     c****               z-add(h)  w01imp        §taitt
085000091202     c                   add(h)    w01imp        §taitt
085100091202     c                   add(h)    w01imp        vocice
085200020213    1C                   End
085300020213     C*
085400020426     C* memorizza in schiera per eventuale storno a fronte di un accessorio
085500020426     C* in sostituzione alla tariffa base
085600020426     C                   exsr      Mem_xBase
085700020426     C*
085800020213     C                   ENDSR
085900020213     C*================================================================
086000020325     C* Imposta la Tariffa Base x espresso (la maggiorazione)         *
086100020213     C*================================================================
086200020325     C     Base_eca      BEGSR
086300020213      *
086400020325      * richiama la routine del calcolo come per gli accessori
086500020325     C*  Calcolo Valore
086600020325     c                   eval      Se_Access ='N'
086700020325     C*               *----------------------*
086800020325     c                   exsr      Calc_VAL
086900020325     C*               *----------------------*
087000020514     C*
087100020514     c                   if        §taitt = *all'9' and w01itr<>0
087200020514     c                   eval      §taitt = 0
087300020514     c                   end
087400020325     C*
087500020325      * poi eseguo come la base ma in aggiunta
087600020325     c     Piumen        ifeq      '-'
087700091202     c                   sub(h)    W01ITR        §taitt
087800091202     c                   sub(h)    w01itr        vocice
087900020325    1C                   else
088000091202     c                   add(h)    W01ITR        §taitt
088100091202     c                   add(h)    w01itr        vocice
088200020325    1C                   End
088300020426     C*
088400020426     C* memorizza in schiera per eventuale storno a fronte di un accessorio
088500020426     C* in sostituzione alla tariffa base
088600020426     C                   exsr      Mem_xBase
088700020325     C*
088800020325     C                   ENDSR
088900020325     C*================================================================
089000020325     C* Imposta la Tariffa Stop                                       *
089100020325     C*================================================================
089200020325     C     Stop          BEGSR
089300020325      *
089400020213      *  Cerca quindi la tariffa di dettaglio con il giusto CAP e scaglione
089500020213     C*        CALCOLO solo il Valore della riga
089600020213     c                   eval      Se_Base ='N'
089700020213      *             *---------------------------*
089800020215     C                   exsr      Tar_Base
089900020213      *             *---------------------------*
090000020213      *
090100020213     c                   clear                   w01tst
090200020213      *
090300020213     C     w01imp        IFGT      0
090400091202     C     §TASPP        MULT(h)   w01imp        w01TST
090500020213     C                   ENDIF
090600020213      *
090700020213      * Imposta il campo di ritorno
090800020213     c     Piumen        ifeq      '-'
090900020514     c****               z-sub     w01tst        §taTST
091000020514     c                   sub       w01tst        §taTST
091100020326     c                   sub       w01tst        vocice
091200020213    1C                   else
091300020514     c****               z-add     w01tst        §taTST
091400020514     c                   add       w01tst        §taTST
091500020326     c                   add       w01tst        vocice
091600020213    1C                   End
091700020426     C*
091800020426     C* memorizza in schiera per eventuale storno a fronte di un accessorio
091900020426     C* in sostituzione alla tariffa base che azzera anche lo STOP
092000020426     C                   exsr      Mem_xStop
092100020213      *
092200020213     C                   ENDSR
092300020213     C*================================================================
092400020325     C* Imposta la Tariffa Stop x espresso (la maggiorazione)         *
092500020325     C*================================================================
092600020325     C     Stop_est      BEGSR
092700020325      *
092800020325      * richiama la routine del calcolo come per gli accessori
092900020325     C*  Calcolo Valore
093000020325     c                   eval      Se_Access ='N'
093100020325     C*               *----------------------*
093200020325     c                   exsr      Calc_VAL
093300020325     C*               *----------------------*
093400020325      *
093500020325      * poi eseguo come lo stop ma in aggiunta
093600020325     c                   clear                   w01tst
093700020325      *
093800020325     C     w01itr        IFGT      0
093900091202     C     §TASPP        MULT(h)   w01itr        w01TST
094000020325     C                   ENDIF
094100020325      *
094200020325      * Imposta il campo di ritorno
094300020325     c     Piumen        ifeq      '-'
094400020325     c                   sub       w01tst        §taTST
094500020326     c                   sub       w01tst        vocice
094600020325    1C                   else
094700020325     c                   add       w01tst        §taTST
094800020326     c                   add       w01tst        vocice
094900020325    1C                   End
095000020426     C*
095100020426     C* memorizza in schiera per eventuale storno a fronte di un accessorio
095200020426     C* in sostituzione alla tariffa base che azzera anche lo STOP
095300020426     C                   exsr      Mem_xStop
095400020325      *
095500020325     C                   ENDSR
095600030109     C*================================================================
095700030109     C* Imposta la Tariffa Base x dest.disagiato (la maggiorazione)   *
095800030109     C*================================================================
095900030109     C     Base_Disag    BEGSR
096000030109      *
096100030109      * richiama la routine del calcolo come per gli accessori
096200030109     C*  Calcolo Valore
096300030109     c                   eval      Se_Access ='N'
096400030109     C*               *----------------------*
096500030109     c                   exsr      Calc_VAL
096600030109     C*               *----------------------*
096700030109     C*
096800030109     c                   if        §taitt = *all'9' and w01itr<>0
096900030109     c                   eval      §taitt = 0
097000030109     c                   end
097100030109     C*
097200030109      * poi eseguo come la base ma in aggiunta
097300030109     c     Piumen        ifeq      '-'
097400091202     c                   sub(h)    W01ITR        §taitt
097500091202     c                   sub(h)    w01itr        vocice
097600030109    1C                   else
097700091202     c                   add(h)    W01ITR        §taitt
097800091202     c                   add(h)    w01itr        vocice
097900030109    1C                   End
098000030109     C*
098100030109     C* memorizza in schiera per eventuale storno a fronte di un accessorio
098200030109     C* in sostituzione alla tariffa base
098300030109     C                   exsr      Mem_xBase
098400030109     C*
098500030109     C                   ENDSR
098600021223     C*================================================================
098700021223     C* Imposta la Tar. Stop x destinatari disagiati (la maggiorazione)
098800021223     C*================================================================
098900030109     C     Stop_disag    BEGSR
099000021223      *
099100021223      * richiama la routine del calcolo come per gli accessori
099200021223     C*  Calcolo Valore
099300021223     c                   eval      Se_Access ='N'
099400021223     C*               *----------------------*
099500021223     c                   exsr      Calc_VAL
099600021223     C*               *----------------------*
099700021223      *
099800021223      * poi eseguo come lo stop ma in aggiunta
099900021223     c                   clear                   w01tst
100000021223      *
100100021223     C     w01itr        IFGT      0
100200091202     C     §TASPP        MULT(h)   w01itr        w01TST
100300021223     C                   ENDIF
100400021223      *
100500021223      * Imposta il campo di ritorno
100600021223     c     Piumen        ifeq      '-'
100700021223     c                   sub       w01tst        §taTST
100800021223     c                   sub       w01tst        vocice
100900021223    1C                   else
101000021223     c                   add       w01tst        §taTST
101100021223     c                   add       w01tst        vocice
101200021223    1C                   End
101300021223     C*
101400021223     C* memorizza in schiera per eventuale storno a fronte di un accessorio
101500021223     C* in sostituzione alla tariffa base che azzera anche lo STOP
101600021223     C                   exsr      Mem_xStop
101700021223      *
101800021223     C                   ENDSR
101900020325     C*================================================================
102000020213     C* Imposta la Tariffa Picking/Etichettatura                      *
102100020213     C*================================================================
102200020213     C     Pck_Etic      BEGSR
102300020213      *
102400020213      *  Cerca quindi la tariffa di dettaglio con il giusto CAP e scaglione
102500020213     C*        CALCOLO solo il Valore della riga
102600020213     c                   eval      Se_Base ='N'
102700020213      *             *---------------------------*
102800020215     C                   exsr      Tar_Base
102900020213      *             *---------------------------*
103000020506     C********           clear                   w01tpe
103100020213      *
103200020213    1C     w01imp        IFGT      0
103300020213     C*
103400020213     C* FGTTAT = "G" --> A GIORNATA - IMPORTO FISSO
103500020213     C* FGTTAT = "S" --> A SERVIZIO - IMPORTO FISSO
103600020213    2C     Fpttpg        IFEQ      'F'
103700020506     C*
103800020506     C* se a giornata imposto in campi specifici da passare
103900020506     C*  alla conferma
104000020506     c     §tatsr        ifeq      'G'
104100020506     C*
104200021216     c                   if        fptCTD = 'ETI'
104300091202     C                   Z-ADD(h)  w01imp        w01tge
104400020506     c                   end
104500020506     C*
104600020506     c                   if        fptCTD = 'PCK'
104700091202     C                   Z-ADD(h)  w01imp        w01tgp
104800020506     c                   end
104900020506     C*
105000020506     c                   else
105100091202     C                   ADD(h)    w01imp        w01tpe
105200020506     c                   end
105300020506     C*
105400020213   X2C                   ELSE
105500020213     C*
105600020213     C* FGTTAT = "C" --> A COLLO
105700020213     C* Etichettatura se ritiro
105800020213     C     §TATSR        IFEQ      'R'
105900020213     C*
106000091202     C     §TACPE        MULT(h)   w01imp        w01tpe
106100020213     C*
106200020213     C                   ELSE
106300020213     C* Piking se consegna
106400020213     C* Solo sul totale colli delle pagate
106500020213     C     Fpttpp        IFEQ      *BLANKS
106600091202     C     §TANCL        MULT(h)   w01imp        w01tpe
106700020213     C                   ELSE
106800020213     C* Sul totale colli di tutte le spedizioni
106900020213     C     Fpttpp        IFEQ      '1'
107000091202     C     §TACPE        MULT(h)   w01imp        w01tpe
107100020213     C                   END
107200020213     C                   END
107300020213     C                   END
107400020213     C*
107500020213    2C                   ENDIF
107600020213    1C                   ENDIF
107700020213      *
107800020506      * Attenzione: se a giornata occorre dividere i valori
107900020506     c     §tatsr        ifeq      'G'
108000020506      *
108100020506     c                   if        fptCTD = 'PCK'
108200020506      * imposta il campo di ritorno
108300020506     c     Piumen        ifeq      '-'
108400020506     c                   z-sub     w01tgp        §tatgp
108500020506     c                   sub       w01tgp        vocice
108600020506    1C                   else
108700020506     c                   z-add     w01tgp        §tatgp
108800020506     c                   add       w01tgp        vocice
108900020506    1C                   End
109000020506     c                   add       §tatgp        §tatpe
109100020506    1C                   End
109200020506      *
109300021216     c                   if        fptCTD = 'ETI'
109400020506      * imposta il campo di ritorno
109500020506     c     Piumen        ifeq      '-'
109600020506     c                   z-sub     w01tge        §tatge
109700020506     c                   sub       w01tge        vocice
109800020506    1C                   else
109900020506     c                   z-add     w01tge        §tatge
110000020506     c                   add       w01tge        vocice
110100020506    1C                   End
110200020506     c                   add       §tatge        §tatpe
110300020506    1C                   End
110400020506      *
110500020506     c                   else
110600020213      * imposta il campo di ritorno
110700020213     c     Piumen        ifeq      '-'
110800020514     c****               z-sub     w01tpe        §tatpe
110900020514     c                   sub       w01tpe        §tatpe
111000020326     c                   sub       w01tpe        vocice
111100020213    1C                   else
111200020514     c****               z-add     w01tpe        §tatpe
111300020514     c                   add       w01tpe        §tatpe
111400020326     c                   add       w01tpe        vocice
111500020213    1C                   End
111600020506     c                   endif
111700020213      *
111800020213     C                   ENDSR
111900020213     C*================================================================
112000020213     C* Imposta la Tariffa Bonus                                      *
112100020213     C*================================================================
112200020213     C     Bonus         BEGSR
112300020213      *
112400020213      *   controlla se presente in testata il bonus
112500020213     C                   clear                   dsbonus
112600020213     C                   movel     fptflr        dsbonus
112700020213      *------
112800020213     C                   CLEAR                   WTBN
112900020213     C                   CLEAR                   W01TOT
113000020213     C                   CLEAR                   W01NEG
113100020213      *
113200020213     C* DETERMINO TOTALE SPEDIZIONI SU CUI CALCOLARE IL BONUS
113300020213     C* IN BASE AL TIPO PAGAMENTO BONUS
113400020213     C                   SELECT
113500020213      *
113600020213     C* Tipo pagamento = blank => solo sulle spediz. pagate
113700020321     C     §FPTTPB       WHENEQ    *BLANK
113800020213     C     §TASET        SUB       §TASNP        W01TOT
113900020213     C                   Z-ADD     §TASNP        W01NEG
114000020213      *
114100020213     C* Tipo pagamento = '1'   => solo sulle spediz. consegnate
114200020321     C     §FPTTPB       WHENEQ    '1'
114300020213     C     §TASET        SUB       §TASNT        W01TOT
114400020213     C                   Z-ADD     §TASNT        W01NEG
114500020213      *
114600020213     C* Tipo pagamento = '2'   => pagate e/o consegnate
114700020321     C     §FPTTPB       WHENEQ    '2'
114800020213     C     §TASET        SUB       §TASNE        W01TOT
114900020213     C                   Z-ADD     §TASNE        W01NEG
115000020213      *
115100020213     C                   ENDSL
115200020213     C*
115300020213     C* SU UN CERTO  N U M E R O   D I   S E R V I Z I
115400020321    2C     §FPTSAB       IFGT      0
115500020213     C*
115600020213     C* - XXX SPEDIZIONI DAL TOTALE
115700020321    3C     §FPTPMD       IFEQ      '-'
115800020321    4C     W01NEG        IFLE      §FPTSAB
115900020213     C                   ADD       1             WTBN              1 0
116000020213    4C                   ENDIF
116100020213     C*
116200020213   X3C                   ELSE
116300020213     C* + DI XXX SPEDIZIONI (XXX COMPRESO)
116400020321    4C     W01TOT        IFGE      §FPTSAB
116500020213     C                   ADD       1             WTBN              1 0
116600020213    4C                   ENDIF
116700020213    3C                   ENDIF
116800020213     C*
116900020213     C                   ELSE
117000020213     C* SE NON C'E' IL CONTROLLO E' SUPERATO
117100020213     C                   ADD       1             WTBN
117200020213    2C                   ENDIF
117300020213     C* %
117400020213     C* ALMENO  UNA CERTA  P E R C E N T U A L E
117500020321    2C     §FPTPAB       IFGT      0
117600020321     C     §TASET        MULT      §FPTPAB       W01RST
117700020213     C                   DIV(H)    100           W01RST
117800020213    3C     W01TOT        IFGE      W01RST
117900020213     C                   ADD       1             WTBN
118000020213    3C                   ENDIF
118100020213     C*
118200020213     C                   ELSE
118300020213     C* SE NON C'E' IL CONTROLLO E' SUPERATO
118400020213     C                   ADD       1             WTBN
118500020213    2C                   ENDIF
118600020213     C*-------------------------------
118700020213     C* SE ENTRAMBI I CONTROLLI SUPERATI ASSEGNO IL BONUS
118800020213    2C     WTBN          IFEQ      2
118900020213     C*
119000020213     c     Piumen        ifeq      '-'
119100091202     C                   Z-sub(h)  FptATA        §TATBN
119200091202     c                   sub(h)    fptata        vocice
119300020213     c                   else
119400091202     C                   Z-add(h)  FptATA        §TATBN
119500091202     c                   add(h)    fptata        vocice
119600020213     c                   end
119700020213     C*
119800020213    2C                   ENDIF
119900020213     C*-------------------------------
120000020213      *
120100020213     C                   ENDSR
120200020214     C*================================================================
120300030527     C*  x Solo Recupero C/A
120400020214     C*================================================================
120500030527     C     Recup_Cas     BegSR
120600030527     C*
120700030527     C     fptctd        ifeq      'FW '
120800030527     C                   move      'S'           esegui
120900030527     C                   END
121000030527      *
121100030527     C                   ENDSR
121200030527     C*================================================================
121300030527     C*  x Solo Incasso sugli  Accessori
121400030527     C*================================================================
121500030527     C     Solo_Incasso  begSR
121600030527     C*
121700020320     C* INCASSO C/ASSEGNI
121800020320     C     §TACAS        ifGT      0
121900020320     C     fptctd        andeq     'C  '
122000020320     C                   Z-ADD     §TACAS        WTACAS
122100020320     C                   EXSR      CALIMP
122200020320     C                   Z-ADD     YECIMO        W0100            13 3
122300020320     C                   move      'S'           esegui
122400020320     C                   END
122500020320      *
122600020320     C* INCASSO ASSEGNATI
122700020320     C     §TAIFP        IFGT      0
122800020320     C     fptctd        andeq     'B  '
122900020320     C                   Z-ADD     §TAIFP        W0100
123000020320     C                   move      'S'           esegui
123100020320     C                   END
123200020320      *
123300020320     C                   ENDSR
123400020320     C*================================================================
123500020320     C*  Imposta la Tariffa degli Accessori
123600020320     C*================================================================
123700020320     C     Accessori     begSR
123800020214     C*
123900020321     C     Giac_LAvv     Ifeq      'S'
124000020418     C     fptctd        andeq     §tafgc
124100020214     C                   move      'S'           esegui
124200020321     C                   end
124300020418      *
124400020214     C* INCASSO C/ASSEGNI
124500020214     C     §TACAS        ifGT      0
124600020214     C     fptctd        andeq     'C  '
124700020214     C                   Z-ADD     §TACAS        WTACAS
124800020214     C                   EXSR      CALIMP
124900020215     C                   Z-ADD     YECIMO        W0100            13 3
125000020214     C                   move      'S'           esegui
125100020214     C                   END
125200020214     C*
125300020214     C* C/A PRONTAMENTE RECUPERATI
125400020214     C     §TACAS        IFGT      0
125500020214     C     §TACA1        andgt     0
125600020214     C     fptctd        andeq     'N  '
125700020214     C                   Z-ADD     §TACA1        WTACAS
125800020214     C                   EXSR      CALIMP
125900020215     C                   Z-ADD     YECIMO        W0100
126000020214     C                   move      'S'           esegui
126100020214     C                   ENDIF
126200020214      *
126300020214     C* INCASSO ASSEGNATI
126400020214     C     §TAIFP        IFGT      0
126500020214     C     fptctd        andeq     'B  '
126600020214     C                   Z-ADD     §TAIFP        W0100
126700020214     C                   move      'S'           esegui
126800020214     C                   END
126900020214     C***
127000020214     C* E-ESPRESSO  (TARIFFA PARTICOLARE DEL TIPO SERVIZIO)
127100091009     C* H-H10:30    (TARIFFA PARTICOLARE DEL TIPO SERVIZIO)
127200020214     C                   Z-ADD     1             C
127300020214     C     §TATSP        LOOKUP    TSP(C)                                 32
127400020214     C     *IN32         IFEQ      *ON
127500020214     C     Giac_LAvv     andeq     'N'
127600020214     C                   clear                   codctd
127700020214     C                   MOVEL     TPT(C)        codctd
127800091009      *  Attenzione poichè nella 5E la H 10:30 è espressa con (h) minuscolo poichè
127900091009      *   le varie utilizzano anche lettere minuscole.
128000091009     c                   if        TPT(C) ='h'
128100091009     C                   MOVEL(p)  'H'           codctd
128200091009     c                   end
128300020214     C     fptctd        ifeq      codctd
128400091009      *
128500091009     C     fptctd        oreq      'E  '
128600091009     C     §taTSP        andeq     'H'
128700091009     C     Si_TarH1030   andeq     'NO'
128800091009      *
128900020214     C                   move      'S'           esegui
129000091009      *
129100020214     c                   end
129200020214     C                   ENDIF
129300020214     C***
129400020214     C* CONSEGNE PARTICOLARI
129500020214     C***
129600020214     C     §TATC1        IFNE      ' '
129700020214     C     §TATC1        ANDNE     'L'
129800020214     C     Giac_LAvv     andeq     'N'
129900020214     C                   clear                   codctd
130000020214     C                   MOVEL     §TATC1        codctd
130100020322      * se supermercati  i codici possono essere ("S  " o "S 2")
130200020214     C     fptctd        ifeq      codctd
130300020321     c     fptctd        oreq      'S 2'
130400020321     C     codctd        andeq     'S  '
130500020214     C                   move      'S'           esegui
130600020214     c                   end
130700020214     C                   ENDIF
130800020214     C*
130900020214     C     §TATC2        IFNE      ' '
131000020214     C     §TATC2        ANDNE     'L'
131100020214     C     Giac_LAvv     andeq     'N'
131200020214     C                   clear                   codctd
131300020214     C                   MOVEL     §TATC2        codctd
131400020322      * se supermercati  i codici possono essere ("S  " o "S 2")
131500020214     C     fptctd        ifeq      codctd
131600020321     c     fptctd        oreq      'S 2'
131700020321     C     codctd        andeq     'S  '
131800020214     C                   move      'S'           esegui
131900020214     c                   end
132000020214     C                   ENDIF
132100020214     C* ISOLA
132200020214     C     §TAFIN        IFEQ      'I'
132300020214     C     Giac_LAvv     andeq     'N'
132400020214     C                   clear                   codctd
132500020214     C                   MOVEL     §TAFIN        codctd
132600020214     C     fptctd        ifeq      codctd
132700020214     C                   move      'S'           esegui
132800020214     c                   end
132900020214     C                   ENDIF
133000020214     C***
133100020214     C* LOCALITA' DISAGIATE
133200020214     C     §TAFIN        IFEQ      'D'
133300020214     C     Giac_LAvv     andeq     'N'
133400020321     C**** fptctd        andeq     'K  '
133500020321     C     fptctd        andeq     'LOD'
133600020214     C                   move      'S'           esegui
133700020214     C                   ENDIF
133800020214     C***
133900020214     C* CENTRI STORICI
134000110906     c*  ES --> vecchia gestione ma non esiste
134100110906     C***  §TAFIN        IFEQ      'S'
134200110906     C***  Giac_LAvv     andeq     'N'
134300110906     C***  fptctd        andeq     'Q  '
134400110906     C***                move      'S'           esegui
134500110906     C***                ENDIF
134600020214     C*** ----------------------------------------- ****
134700020214     C* RICONSEGNA GIACENZA       (NON ESISTE +)
134800020214     C***
134900020214     C*****§TAFGC        IFEQ      'S'
135000020214     C*****fptctd        andeq     'G  '
135100020214     C*******            move      'S'           esegui
135200020214     C*******            ENDIF
135300020214     C*
135400020214     C* COMPETENZE CONTO ECONOMICO  (SEMPRE APPLICATA)
135500020214     C     fptctd        ifeq      'O  '
135600020214     C                   move      'S'           esegui
135700020214     C                   ENDIF
135800020322      *
135900020214     C* COMPETENZE AGGIUNTIVE 2
136000020214     C     fptctd        ifeq      'Z  '
136100020214     C                   move      'S'           esegui
136200020214     C                   ENDIF
136300020214     C***
136400020214     C* COMPETENZE AGGIUNTIVE 3
136500090928     C*   tariffa di maggiorazione a stop di consegna over 5 Kg.
136600020214     C     fptctd        ifeq      'X  '
136700090915     C     fptctd        oreq      'XT '
136800090928      *
136900090928      * viene attivata solo se le spedizioni inerenti lo stop
137000090928      *  superano i 5 Kg.
137100090928     C     Peso_Sped     ifgt      5
137200020214     C                   move      'S'           esegui
137300090928     c                   end
137400090928      *
137500020214     C                   ENDIF
137600020214     C***
137700020214     C* PRIVATI (PARTICOLARITA' VARIE)
137800020214     C     §TAGVA        IFEQ      'P '
137900020214     C     Giac_LAvv     andeq     'N'
138000020214     C     fptctd        andeq     'V  '
138100020214     C                   move      'S'           esegui
138200020214     C                   END
138300020322      *
138400020322      * ---------------
138500020322      * Nuove Tariffe
138600020322      * ---------------
138700020322      * Tariffa Scarico di Ritiro
138800020322     C     fptctd        ifeq      'SCR'
138900021211     C     §tafcs        andeq     'S'
139000020322     C                   move      'S'           esegui
139100020322     C                   ENDIF
139200020322      *
139300020322      * Tariffa Carico di Consegna
139400020322     C     fptctd        ifeq      'CAC'
139500021211     C     §tafcs        andeq     'S'
139600020322     C                   move      'S'           esegui
139700020322     C                   ENDIF
139800020325      * ------------
139900020325      *
140000020214     C                   ENDSR
140100020213     C*================================================================
140200090928     C* CALCOLO il Valore da moltiplicare in base all'unità mis.di Pagamento
140300020213     C*================================================================
140400090928     C     MoltiplicatoreBEGSR
140500020215     C*
140600090928     C*  In questa routine, in base alla tipologia di Pagamento, si deve calcolare
140700090928     C*  la quantità (in unità di misura espressa dal tipo pagamento) che dovrà
140800090928     C*  essere moltiplicata x ogni tariffa da applicare all'autista.
140900090928     C*
141000020215     C*  prende dalla tab. "TR"  il > Minimo <
141100020215     C                   MOVEL     'TR'          COD
141200020215     C                   MOVEL     *BLANKS       KEY
141300020215     C                   MOVEL     fpttpg        KEY
141400020215     C     KTAB          CHAIN     TABEL                              30
141500020215     C   30              Z-ADD     1             §TRATA
141600020215     C  N30              MOVEL     TBLUNI        DSTR
141700090928     C*
141800090928     C*  In tutti i casi comunque si calcola in una variabile a parte il
141900090928     C*  Peso tassabile per ragionamenti puramente legati al peso.
142000090928     C*
142100090928     C* SE NO RAPPORTO O VOLUME A ZERO --> PESO TASSABILE=PESO REALE
142200090928    2C     fptRPV        IFeq      0
142300090928     C     §TATVL        OReq      0
142400090928     C                   Z-ADD     §TAKFA        Peso_Sped
142500090928   X2C                   ELSE
142600090928     C*
142700090928     C     §TATVL        MULT(H)   fptRPV        VolXrpv          13 3
142800090928     C     VolXrpv       MULT      100           VolXrpv
142900090928     C*
143000090928     C* PRENDO IL PIU' GRANDE TRA IL TASSABILE DA VOLUME E IL REALE
143100090928    3C     VolXrpv       IFLT      §TAKFA
143200090928     C                   Z-ADD     §TAKFA        Peso_Sped
143300090928     C                   ELSE
143400090928     C                   Z-ADD     VolXrpv       Peso_Sped
143500090928    3C                   ENDIF
143600090928    2C                   ENDIF
143700090928     C*
143800020215     C*
143900020215     C* CTR A KILO E Q.LE --> RAPPORTO PESO /VOL E ARROTONDAMENTI
144000020215     C* CTR A COLLO       -->                      ARROTONDAMENTI
144100020215     C* CTR A SPEDIZIONE  -->
144200020213     C**
144300020213     C* KILO O Q.LE  PER TARIFFE NORMALI
144400020213     C*    SOLO KILO PER TARIFFE PARTICOLARI
144500020213    1C                   SELECT
144600020213     C**
144700020215    1C     fpttpg        WHENeq    '0'
144800020215     C     fpttpg        OReq      '3'
144900020213     C*
145000020213     C* SE NO RAPPORTO O VOLUME A ZERO --> PESO TASSABILE=PESO REALE
145100020215    2C     fptRPV        IFeq      0
145200020215     C     §TATVL        OReq      0
145300020213     C                   Z-ADD     §TAKFA        W01PTA
145400020213   X2C                   ELSE
145500020213     C*
145600020215     C     §TATVL        MULT(H)   fptRPV        VolXrpv          13 3
145700020215     C     VolXrpv       MULT      100           VolXrpv
145800020213     C*
145900020213     C* PRENDO IL PIU' GRANDE TRA IL TASSABILE DA VOLUME E IL REALE
146000020215    3C     VolXrpv       IFLT      §TAKFA
146100020213     C                   Z-ADD     §TAKFA        W01PTA
146200020213     C                   ELSE
146300020215     C                   Z-ADD     VolXrpv       W01PTA
146400020213    3C                   ENDIF
146500020213    2C                   ENDIF
146600020213     C* COLLI
146700020215   x1C     fpttpg        WHENeq    '1'
146800020213     C                   Z-ADD     §TANCL        W01PTA
146900020213     C* SPEDIZIONI
147000020215   x1C     fpttpg        WHENeq    '2'
147100020213     C                   Z-ADD     §TASET        W01PTA
147200020213     C* Bonus
147300020215     C* Fisso (Valore Fisso)
147400020215     C* Valore (% su Valore) x le TARIFFE PARTICOLARI
147500020215     C*   imposta un valore fittizio per seguenti calcoli
147600020215   x1C     fpttpg        WHENeq    'B'
147700020215   x1C     fpttpg        OReq      'F'
147800020215   x1C     fpttpg        OReq      'V'
147900020215     c                   z-add     1             W01PTA
148000020213      *
148100020213     C                   OTHER
148200020213     C**
148300020213    1C                   ENDSL
148400020215     C* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
148500020215     C* Quindi arrotonda il peso Tassabile
148600020215     C*   SOLO SE previsto arrotondamento è sulla tab.>TR<
148700020215     C*
148800020215     C* e APPLICO arrotondamento SOLTANTO SE il Valore  è >=  al MINIMO
148900020215    1C     W01PTA        IFGE      §TRATA
149000020215     C     §TRarr        andEQ     'S'
149100020215     C*                          =======
149200020215     C* ARROTONDAMENTO OLTRE
149300020215    3C     W01PTA        IFGT      fptARL
149400020215     C                   Z-ADD     fptARO        W01ARR
149500020215   X3C                   ELSE
149600020215     C* ARROTONDAMENTO FINO A
149700020215     C                   Z-ADD     fptARF        W01ARR
149800020215    3C                   ENDIF
149900020215     C*
150000020215     C* APPLICO ARROTONDAMENTO SE C'E' E SE DIVIDENDO MI DA UN RESTO
150100020215    3C     W01ARR        IFGT      0
150200020215     C     W01PTA        DIV       W01ARR        W01RIS            7 0
150300020215     C                   MVR                     W01RST           10 3
150400020215    4C     W01RST        IFGT      0
150500020215     C     W01RIS        MULT      W01ARR        W01PTA
150600020215     C                   ADD       W01ARR        W01PTA
150700020215    4C                   ENDIF
150800020215    3C                   ENDIF
150900020215     C*
151000020215    1C                   ENDIF
151100020215     C* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
151200020213     C*
151300020213     C                   ENDSR
151400020213     C* ==============================================================
151500020213     C* Si posiziona sulla Tariffa con il CAP                        *
151600020213     C* ==============================================================
151700020215     C     Tar_Base      BEGSR
151800020213      *
151900020213     C* Inizializza campo dell'importo Tariffa
152000100108     C*******            clear                   w01imp           18 3
152100100108     C                   clear                   w01imp           18 5             x prossimo step
152200020213     C*
152300020214     C*  Importo Fisso in testata
152400020214     C                   If        FptTPG = 'F' or  FptTPG = 'B'
152500020214     C*
152600091202     c                   z-add(h)  FptATA        w01imp
152700020214     C*
152800020214     c                   else
152900020214     C*
153000020322     C*  Cerca la Tariffa sugli scaglioni
153100020322     C*               *----------------------*
153200020322     c                   exsr      Dett_Tar
153300020322     C*               *----------------------*
153400020213     C*
153500020214    1C                   End
153600020322     C*
153700020322     C                   ENDSR
153800020213     C*===============================================================
153900020322     C* Dettaglio Tariffa  a scaglioni                               *
154000020213     C*===============================================================
154100020322     C     Dett_Tar      BEGSR
154200020213     C*
154300020322     C*  Importo su Dettaglio tariffario
154400020322     C*  Cerca lo scaglione
154500020322     c                   eval      Se_Access ='N'
154600020322     C*               *----------------------*
154700020322     c                   exsr      Riga_FPD
154800020322     C*               *----------------------*
154900020322     C*
155000020322      * -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
155100020322      * Ma.....Attenzione: se non si è trovata la tariffa e
155200020322     C*    solo se si sta cercando la TARIFFA BASE ......
155300020322     C* questa routine serve anche per cercare i valori del
155400020322     C* pick/etic. e di stop e per questi non deve dare il
155500020322     C* manca tariffa poichè non è obbligatoria.
155600020322      * -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
155700020322     c     Trovata_tar   ifeq      'N'
155800020322      *
155900020322      *  deve dare il manca tariffa solo se è la base
156000020322    1C     Se_Base       ifEQ      'S'
156100020322     C                   EXSR      Manca_TAR
156200020322    1C                   End
156300020322      *
156400020322      *  comunque salta tutto
156500020322    1C                   Goto      EndTariffa
156600020322    1C                   End
156700020322     C*
156800020322      * -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
156900020322     C*
157000020322    1C     Se_Base       ifEQ      'N'
157100020322     C*               -----------
157200020322     C* Valore preso dallo Scaglione
157300091202     C                   Z-ADD(h)  FpDITR        w01imp
157400020322     C*               -----------
157500020322    1C                   else
157600020322     C*               -----------
157700020322     C* PESO TASSABILE < = AL  MINIMO TASSABILE
157800020322    2C     W01PTA        IFLE      W01ATA
157900020322     C*
158000020322     C* Valore preso dallo Scaglione
158100091202     C                   Z-ADD(h)  FpDITR        w01imp
158200020322     C*
158300020322   X2C                   ELSE
158400020322     C*
158500020322     C* SE TARIFFA A Q.LE DIVIDO PER 100
158600020322     C     fpttpg        IFEQ      '0'
158700020322     C                   DIV(H)    100           W01PTA
158800020322     C                   ENDIF
158900020322     C*
159000020322     C* SCAGLIONE > DEL MINIMO TASSABILE
159100020322     C*  MOLTIPLICO TARIFFA PER IL PESO TASSABILE
159200020322     C*  PRENDO L'IMPORTO  >  TRA QUESTO E IL MINIMO
159300020322     C     W01PTA        MULT(H)   FpDITR        w01imp
159400020322     C*
159500020322    2C                   ENDIF
159600020322     C*               -----------
159700020322    1C                   End
159800020322     C*
159900020322     C* sempre esegue il test del minimo e del massimo
160000020322     C* PRENDO L'IMPORTO  >  TRA LA TARIFFA E IL MINIMO
160100020322    3C     w01imp        IFLT      FpDMIN
160200020322     C                   Z-ADD(H)  FpDMIN        w01imp
160300020322    3C                   ENDIF
160400020322     C*
160500020322     C* PRENDO L'IMPORTO MAX SE w01imp > DEL MAX (SE IMMESSO)
160600020322     C     FpDMAX        IFGT      0
160700020322     C     w01imp        ANDGT     FpDMAX
160800020322     C                   Z-ADD(H)  FpDMAX        w01imp
160900020322     C                   END
161000020322     C*
161100020322     C     EndTariffa    ENDSR
161200020322     C*===============================================================
161300020322     C* Prende la riga del FIFPD                                     *
161400020322     C*===============================================================
161500020322     C     Riga_FPD      BEGSR
161600020322     C*
161700020213     C* APPLICAZIONE TARIFFA FINITA: SE NON C'E' PRENDO IL MINIMO
161800020213     C*  DA TABELLA TR
161900020313     c                   if        Se_Access ='S'
162000020313     C                   Z-ADD     §TRATA        W01ATA
162100020313     c                   else
162200020213    1C     W01ATA        IFEQ      0
162300020213     C                   Z-ADD     §TRATA        W01ATA
162400020213    1C                   ENDIF
162500020313     c                   end
162600020213     C*
162700020213      * imposta ind.31 a ON come se non avesse trovato il record
162800020213      *  solo nella LEGFPD se trova il record lo spegne.
162900020213     C                   SETON                                        31
163000020606     C** e inizializza il flag
163100020606     c                   move      'N'           Trovata_tar
163200020213     C**
163300020213     C* CONTROLLO SE IL DETTAGLIO CONTIENE RIGHE SENZA CAP
163400020213     C                   CLEAR                   W01CAP
163500020213     C     KFPD          SETLL     fifpd000                               30
163600020213    1C     *IN30         IFEQ      *ON
163700020213     C                   EXSR      LEGFPD
163800020213    1C                   ENDIF
163900020213     C**
164000020213     C* CAP INTERO DI 5 CIFRE
164100020213    1C     *IN30         IFEQ      *OFF
164200020606     C                   MOVEL     §TACAP        W01CAP
164300020606     C**
164400020606     C* Prova con il CAP intero
164500020606     C     KFPD          setll     fifpd000                               30
164600020606    1C     *IN30         ifeq      *ON
164700020606     C                   exsr      LEGFPD
164800020606    1C                   Else
164900020606     C**
165000020606     C                   MOVE      §TACAP        WNUM2             2 0
165100020213    2C     WNUM2         IFGT      0
165200020213     C     KFPD          SETLL     fifpd000                               30
165300020213    3C     *IN30         IFEQ      *ON
165400020213     C                   EXSR      LEGFPD
165500020213    3C                   ENDIF
165600020213    2C                   ENDIF
165700020606     c                   end
165800020606      *
165900020213    1C                   ENDIF
166000020409      *
166100020213     C* CAP CON ULTIME 2 CIFRE A "00"
166200020213    1C     *IN30         IFEQ      *OFF
166300020409     C     §TAFIN        IFNE      'C'
166400110906     C     §TAFIN        andne     'T'
166500020213     C                   MOVE      '000'         WDSCAP
166600020213     C                   ELSE
166700020213     C                   MOVE      '100'         WDSCAP
166800020213     C                   ENDIF
166900020213     C     KFPD          SETLL     fifpd000                               30
167000020213    2C     *IN30         IFEQ      *ON
167100020213     C                   EXSR      LEGFPD
167200020213    2C                   ENDIF
167300020213    1C                   ENDIF
167400020213     C**
167500020213     C* CAP CON "IT" NELLE PRIME  2 CIFRE
167600020213    1C     *IN30         IFEQ      *OFF
167700020213     C                   MOVEL     'IT'          W01CAP
167800020213     C     KFPD          SETLL     fifpd000                               30
167900020213    2C     *IN30         IFEQ      *ON
168000020213     C                   EXSR      LEGFPD
168100020213    2C                   ENDIF
168200020213    1C                   ENDIF
168300020213     C*
168400020213     C                   ENDSR
168500020213     C*===============================================================
168600020213     C* LEGGO DETTAGLIO fifpd000                                     *
168700020213     C*===============================================================
168800020213     C     LEGFPD        BEGSR
168900020213     C*
169000020213     C* TROVATO ALMENO UNA RIGA DI DETTAGLIO CON IL CAP:
169100020213     C* (CAP SPECIFICO -O CAP CON 00 -O CAP IT)
169200020213     C* SE NON TROVO LO SCAGLIONE ALL'INTERNO
169300020213     C* E' TARIFFA A 0 SENZA CERCARE NEL CAP PIU' GENERICO SUCCESSIVO
169400020213     C*
169500020213     C* SE WNUM1 = ' ' SIGNIFICA CHE CI SONO SOLO RECORD ANNULLATI
169600020213     C*                DEVO CERCARE NUOVAMENTE
169700020213     C*
169800020213     C                   CLEAR                   WNUM1
169900020213     C     KFPD          READE     fifpd000                               31
170000020213     C*
170100020213     C* SE ANNULLATO COME SE AVESSE SCAGLIONE A ZERO
170200020213     C     FPDATB        IFNE      ' '
170300020213     C                   Z-ADD     0             FPDSGL
170400020213     C                   ELSE
170500020213     C                   MOVEL     1             WNUM1             1 0
170600020213     C                   ENDIF
170700020213     C*
170800030603     C* A causa di un vecchio modo di inserire gli scaglioni delle tariffe
170900030603     C*  per indicare l'inifinito si usava il 9999 o 99999 o 999999
171000030603     C*   potendo, in tal modo, non soddisfare il test successivo per una
171100030603     C*    quantità elevata soprattutto per quanto riguarda SDI.
171200030603     C* Per eliminare il problema: reimposto il campo.
171300030603     c     Fpdsgl        ifeq      9999
171400030603     c     Fpdsgl        oreq      99999
171500030603     c     Fpdsgl        oreq      999999
171600030603     c                   move      *all'9'       Fpdsgl
171700030603     c                   end
171800030603     C*
171900020213     C     *IN31         DOWEQ     *OFF
172000020213     C     FPDSGL        ANDLT     W01PTA
172100020213     C*
172200020213     C     KFPD          READE     fifpd000                               31
172300020213     C  N31FPDATB        IFNE      ' '
172400020213     C                   Z-ADD     0             FPDSGL
172500020213     C                   ELSE
172600020213     C                   MOVEL     1             WNUM1
172700020213     C                   ENDIF
172800020213     C                   ENDDO
172900020213     C*
173000020213     C* 31 ON - NON TROVATO SCAGLIONE
173100020322     c                   move      'S'           Trovata_tar       1
173200020322     c   31              move      'N'           Trovata_tar
173300020213     C*
173400020213     C* CERCO NUOVAMENTE
173500020213     C     WNUM1         IFEQ      0
173600020213     C                   SETOFF                                       30
173700020213     C                   ENDIF
173800020213     C*
173900020213     C                   ENDSR
174000020214     C*================================================================
174100020214     C* CALCOLO IMPORTO CONTRASSEGNO IN BASE AL CAMBIO                *
174200020214     C*================================================================
174300020214     C     CALIMP        BEGSR
174400020214     C*---------
174500020214     C* UTILIZZA LA YEURCO PER CONVERITRE L'IMPORTO
174600020214     C                   CLEAR                   YEURDS
174700020214     C                   MOVEL     §TAVCA        YECDVI
174800020214     C                   Z-ADD     WTACAS        YECIMI
174900020214     C                   MOVEL     §GEDCN        YECDVO
175000020214     C                   MOVE      '3'           YECDCO
175100020214     C                   MOVEL     'H'           YECTAR
175200020214     C                   CALL      'YEURCO'
175300020214     C                   PARM                    YEURDS
175400020215     C*
175500020214     C                   ENDSR
175600020214     C*===============================================================
175700020325     C* CALCOLO il valore dal dettaglio TARIFFE PARTICOLARI          *
175800020214     C*===============================================================
175900020325     C     Calc_VAL      BEGSR
176000020325     C*
176100020325     C                   CLEAR                   W01ITR
176200020325     C*
176300020325     C*  Importo Fisso in testata
176400020325     C                   If        FptTPG = 'F' or  FptTPG = 'B'
176500020325     C*
176600091202     c                   z-add(h)  FptATA        w01itr
176700020325     C*
176800020325     c                   else
176900020325     C*
177000020325     C*  Cerca lo scaglione
177100020325     C*               *----------------------*
177200020325     c                   exsr      Riga_FPD
177300020325     C*               *----------------------*
177400020325     C*
177500020325     C* TROVATA RIGA DI DETTAGLIO
177600020325     c     Trovata_tar   Ifeq      'S'
177700020325     C*
177800020325     C* SE LA TARIFFA NON E' A VALORE E
177900020325     C* SCAGLIONE < = AL  MINIMO TASSABILE
178000020325    2C     fpttpg        IFNE      'V'
178100020325     C     W01PTA        ANDLE     W01ATA
178200020325     C*
178300100108     C*******            Z-ADD(h)  FPDITR        W01ITR           18 3
178400100108     C                   Z-ADD     FPDITR        W01ITR           18 5          x prossimo step
178500020325     C*
178600020325   X2C                   ELSE
178700020325     C*
178800020325     C* SCAGLIONE > DEL MINIMO TASSABILE
178900020325     C*  MOLTIPLICO TARIFFA PER IL PESO TASSABILE
179000020325     C*  PRENDO L'IMPORTO  >  TRA QUESTO E IL MINIMO
179100041108     C*****fpttpg        IFne      'V'
179200020325     C     W01PTA        MULT(H)   FPDITR        W01ITR
179300041108     c************       endif
179400020325      *
179500020325     C* SE TARIFFA A VALORE DIVIDO PER 100 PERCHE' E' UNA PERCENTUALE
179600020325     C     fpttpg        IFEQ      'V'
179700020325     C     W01itr        MULT(H)   w0100         W01ITR
179800020325     C                   DIV(H)    100           W01ITR
179900020325     C                   ENDIF
180000020325      *
180100020325    2C                   ENDIF
180200020325     C*
180300020325     C* PRENDO L'IMPORTO  >  TRA LA TARIFFA E IL MINIMO(SE C'E')
180400020325     C     w01ITR        IFLT      FPDMIN
180500091202     C                   Z-ADD(H)  FPDMIN        W01ITR
180600020325    3C                   ENDIF
180700020325     C*
180800020325     C* SE L'IMPORTO SUPERA IL MAX PRENDO IL MAX (SE IMMESSO)
180900020325    2C     FPDMAX        IFGT      0
181000020325     C     W01ITR        ANDGT     FPDMAX
181100020325     C                   Z-ADD(H)  FPDMAX        W01ITR
181200020325    2C                   ENDIF
181300020325     C*
181400020325    1C                   ENDIF
181500020325     C*
181600020325    1C                   End
181700020325     C*
181800020325     C                   ENDSR
181900020325     C*===============================================================
182000020325     C* CALCOLO IMPORTO DA DETTAGLIO TARIFFE PARTICOLARI             *
182100020325     C*===============================================================
182200020325     C     Calc_Access   BEGSR
182300020325     C*
182400020325     C*  Calcolo Valore
182500020325     c                   eval      Se_Access ='S'
182600020325     C*               *----------------------*
182700020325     c                   exsr      Calc_VAL
182800020325     C*               *----------------------*
182900020325     C*
183000020322     C*.......................................................................
183100020514     C*  Se richiesta la SOSTITUZIONE della Tariffa Base
183200020514      *------------
183300020514     C**** FPDAIN        IFNE      'S'
183400020214     C*
183500020322     C*  L'importo di solito viene impostato nel suo cumulatore (§taITA)
183600020322     C*   oppure può essere dirottato sulla base (§taITT) tutto è pilotato
183700020322     C*    dalla tabella 8A tramite il flag "TBA" che può essere blk o "B"-> Base.
183800020426      *------------
183900020514    1c                   if        flbase <>'B'
184000020322      * se accessori
184100020214     c     Piumen        ifeq      '-'
184200020214     C                   sub(H)    w01itr        §taita
184300091202     c                   sub(h)    w01itr        vocice
184400020426     C                   else
184500020214     C                   ADD(H)    W01ITR        §TAITA
184600091202     c                   add(h)    w01itr        vocice
184700020426     C                   End
184800020514     C
184900020514    1C                   else
185000020322     C* se Base
185100020322     C*    vecchio retaggio del programma chiamante *all'9'
185200020322     C*     poichè devo eseguire una somma algebrica, azzero il campo.
185300020514     c                   if        §taitt = *all'9'
185400020514     c                   eval      §taitt = 0
185500020514     c                   end
185600020514     C*
185700020514     c     Piumen        ifeq      '-'
185800020514     C                   sub(H)    w01itr        §taitt
185900091202     c                   sub(h)    w01itr        vocice
186000020514     C                   else
186100020514     C                   ADD(H)    W01ITR        §TAITT
186200091202     c                   add(h)    w01itr        vocice
186300020514     C                   End
186400020514     C*
186500020514    1C                   End
186600020514     C***********
186700020514     C***********        ELSE
186800020514     C***********
186900020514     C***se*invece richiesta la sostituzione della Base
187000020514     c*****Piumen        ifeq      '-'
187100020514     C***********        sub(H)    w01itr        w01sia
187200091202     c***********        sub(h)    w01itr        vocice
187300020514    1C***********        else
187400020514     C***********        ADD(H)    W01ITR        w01sia
187500091202     c***********        add(h)    w01itr        vocice
187600020514    1C***********        End
187700020514     C***********
187800020514     c*****§TAPAG        IFEQ      'S'
187900020514     C***********        Z-ADD(H)  W01SIA        §TAITT
188000020514     C***********        move      'S'           Sost_ITT
188100020514     C***********
188200020514     C**PER*SOSTITUZIONE DA TAR. LASCIATO AVVISO/GIACENZA AZZERO ANCHE IMP.STOP CONSEGNA
188300020514     C*****Giac_LAvv     ifeq      'S'
188400020514     C***********        Z-ADD     *ZEROS        §TATST
188500020514     C***********        move      'S'           Sost_TST
188600020514    1C***********        End
188700020514     C***********
188800020514     C***********        ENDIF
188900020514     C***********
189000020514     C***********        ENDIF
189100020214     C*
189200020214     C                   ENDSR
189300020215     C*================================================================
189400020215     C*  Somma algebrica pilotata da Tab.8A
189500020215     C*================================================================
189600020322     C     Tab_8A        begSR
189700020215     C*
189800020215     C* Comunque viene sempre impostata la somma in positivo
189900020215     c                   move      '+'           Piumen            1
190000020322     c                   move      *blank        UtiOK             1
190100020322     c                   move      *blank        FlBase            1
190200020326     C*  Allo stesso momento azzera l'importo del C/Econ. in testa a tutto
190300020403     c                   move      *blank        VocFce            1
190400020403     c                   move      *blank        VocFc1            1
190500020403     c                   move      *blank        VocFc2            1
190600020403     c                   move      *blank        VocCce
190700020327     c                   move      *blank        VocCc1
190800020327     c                   move      *blank        VocCc2
190900020327     c                   move      *blank        Vocusc            1
191000020327     c                   move      *blank        Vocusr            1
191100020326     c                   move      *zeros        VocICE
191200020215     C*
191300020215     C                   Z-ADD     1             C
191400020215     C     fptCTD        LOOKUP    FTP(C)                                 33
191500020215     C     *IN33         IFEQ      *ON
191600020215     C*
191700020322     C* per il calcolo corretto degli accessori
191800020322     c                   movel     UTI(C)        UtiOK
191900020322     c                   movel     TBA(C)        FlBase
192000020326     C*  consegna
192100020326     C                   if        §tatsr = 'C'
192200020403     C                   if        CE1(C) <> *zeros and CE1(C) <> *blank
192300020326     C                   move      CE1(C)        VocCCE
192400020403     C                   move      fc1(C)        VocfCE
192500020326     c                   else
192600020326     C                   move      CE3(C)        VocCCE
192700020403     C                   move      fc3(C)        VocfCE
192800020326     c                   end
192900020327     c                   end
193000020326     C*  ritiri
193100020327     C                   if        §tatsr = 'R'
193200020403     C                   if        CE2(C) <> *zeros and CE2(C) <> *blank
193300020326     C                   move      CE2(C)        VocCCE
193400020403     C                   move      fc2(C)        VocfCE
193500020326     c                   else
193600020326     C                   move      CE4(C)        VocCCE
193700020403     C                   move      fc4(C)        VocfCE
193800020326     c                   end
193900020326     c                   end
194000020327     C*  giornata
194100020327     C                   if        §tatsr = 'G'
194200020327     C                   move      usc(C)        Vocusc
194300020327     C                   move      usr(C)        Vocusr
194400020327     C                   move      CE1(C)        VocCC1
194500020327     C                   move      CE2(C)        VocCC2
194600020403     C                   move      fc1(C)        Vocfc1
194700020403     C                   move      fc2(C)        Vocfc2
194800020327     c                   end
194900020327     C*
195000020215     C*  se l'importo dovrà essere negativo è indicato sulla tab.8A
195100020215     C     SPM(C)        ifeq      '-'
195200020215     c                   move      '-'           Piumen
195300020215     c                   end
195400020215     C*
195500020215    1C                   ENDIF
195600020215     C*
195700020215     C                   ENDSR
195800020215     C*****************************************************************
195900020215     C* *INZSR
196000020215     C*****************************************************************
196100020215     C     *INZSR        BEGSR
196200020215     C*
196300020215     C     KTAB          KLIST
196400020215     C                   KFLD                    CODUT             1 0
196500020215     C                   KFLD                    COD               2
196600020215     C                   KFLD                    KEY               8
196700020215     C*
196800020215     C     KTAB2         KLIST
196900020215     C                   KFLD                    CODUT
197000020215     C                   KFLD                    COD
197100020215     C*
197200020215     C     KTBE          KLIST
197300020215     C                   KFLD                    TBECOD            3
197400020215     C                   KFLD                    TBEKE1           15
197500020215      *
197600020215     C     KFGT          KLIST
197700020215     C                   KFLD                    W01PDR
197800020215     C                   KFLD                    §taSML
197900020215     C                   KFLD                    W01TSR
198000020215     C                   KFLD                    W01CTR
198100090507      *
198200090507     C     KTGT          KLIST
198300090507     C                   KFLD                    fgtPDR
198400090507     C                   KFLD                    fgtSML
198500090507     C                   KFLD                    fgtPRG
198600020215      *
198700020215     C     KFGT2         KLIST
198800020215     C                   KFLD                    W01PDR
198900020215     C                   KFLD                    §taSML
199000020215     C                   KFLD                    W01TSR
199100020215     C                   KFLD                    W01CTR
199200020215     C                   KFLD                    W01PRG
199300020215      *
199400020215     C     KFPT00        KLIST
199500020215     C                   KFLD                    W01PDR
199600020215     C                   KFLD                    §taSML
199700020215     C                   KFLD                    W01TSR
199800020215     C                   KFLD                    W01CTR
199900020215     C                   KFLD                    w01PRG
200000020215     C                   KFLD                    w01ctd
200100020215      * ---
200200020215     C     kfpt02        KLIST
200300020215     C                   KFLD                    k02PDR
200400020215     C                   KFLD                    k02SML
200500020215     C                   KFLD                    k02TSR
200600020215     C                   KFLD                    k02CTR
200700020215     C                   KFLD                    k02PRG
200800020215     C                   KFLD                    k02TSM
200900020215      *
201000020215     C     KFPD          KLIST
201100020215     C                   KFLD                    fptPDR
201200020215     C                   KFLD                    fptSML
201300020215     C                   KFLD                    fptTSR
201400020215     C                   KFLD                    fptCTR
201500020215     C                   KFLD                    fptPRG
201600020215     C                   KFLD                    fptctd
201700020215     C                   KFLD                    w01CAP
201800020215      *
201900020326     C     Kfce          KLIST
202000020326     C                   KFLD                    fceFGS
202100020326     C                   KFLD                    fcePDR
202200020326     C                   KFLD                    fceTSR
202300020326     C                   KFLD                    fceNDC
202400020326     C                   KFLD                    fceDDC
202500020326     C                   KFLD                    fceAAS
202600020326     C                   KFLD                    fceLNP
202700020326     C                   KFLD                    fceNRS
202800020326     C                   KFLD                    fceNSP
202900020326     C                   KFLD                    fceCCE
203000020327      *
203100020327     C     Kfcw          KLIST
203200020327     C                   KFLD                    fcwFGS
203300020327     C                   KFLD                    fcwPDR
203400020327     C                   KFLD                    fcwTSR
203500020327     C                   KFLD                    fcwNDC
203600020327     C                   KFLD                    fcwDDC
203700020327     C                   KFLD                    fcwAAS
203800020327     C                   KFLD                    fcwLNP
203900020327     C                   KFLD                    fcwNRS
204000020327     C                   KFLD                    fcwNSP
204100020327     C                   KFLD                    fcwCCE
204200020328      *
204300030320     C     Kbol          KLIST
204400030320     C                   KFLD                    ftwAAS
204500030320     C                   KFLD                    ftwLNP
204600030320     C                   KFLD                    ftwNRS
204700030320     C                   KFLD                    ftwNSP
204800030320      *
204900020328     C     Kftdw         KLIST
205000020328     C                   KFLD                    ftwFGS
205100020328     C                   KFLD                    ftwPDR
205200020328     C                   KFLD                    ftwTSR
205300020328     C                   KFLD                    ftwNDC
205400020328     C                   KFLD                    ftwDDC
205500020328     C                   KFLD                    ftwSTP
205600020523      *
205700030108     C     kfpt01        KLIST
205800030108     C                   KFLD                    k01PDR
205900030108     C                   KFLD                    k01SML
206000030108     C                   KFLD                    k01TSR
206100030108     C                   KFLD                    k01CTR
206200030108     C                   KFLD                    k01PRG
206300030108     C                   KFLD                    k01CTD
206400030108      *
206500020523     C*===============================================================
206600020523     C* Apertura Files                                               *
206700020523     C*===============================================================
206800020523     C* Chiamato dalla Conferma
206900020523     c                   if        §tapgm = 'CONFERM'
207000020523     c                             and  §tasml = ' '
207100020523     c                   open      FIFCEW1L
207200020523     c                   end
207300020326      *
207400020215     C*===============================================================
207500020215     C* CARICO TABELLE                                               *
207600020215     C*===============================================================
207700020215     C                   Z-ADD     1             CODUT
207800020326     C                   Z-ADD     0             C                 3 0
207900020215     C***
208000020215     C* CARICO TABELLA DEI TIPI SERVIZIO
208100020215     C***
208200020215     C                   Z-ADD     1             C
208300020215     C                   MOVEL     '1Z'          COD
208400020215     C     KTAB2         SETLL     TABEL
208500020215     C     KTAB2         READE     TABEL                                  30
208600020215     C*
208700020215     C     *IN30         DOWEQ     *OFF
208800020215     C     TBLFLG        IFEQ      ' '
208900020215     C                   MOVEL     TBLUNI        DS1Z
209000020215     C     §1ZUPD        IFEQ      'S'
209100020215     C                   MOVEL     TBLKEY        TSR(C)
209200020215     C                   MOVEL     §1ZCTR        CTS(C)
209300020215     C                   ADD       1             C
209400020215     C                   ENDIF
209500020215     C                   ENDIF
209600020215     C     KTAB2         READE     TABEL                                  30
209700020215     C                   ENDDO
209800020215     C***
209900020215     C* CARICO TABELLA DEI TIPI SERVIZIO CON TARIFFA PARTICOLARE
210000020215     C***
210100020215     C                   Z-ADD     1             C
210200020215     C                   MOVEL     '5E'          COD
210300020215     C     KTAB2         SETLL     TABEL
210400020215     C     KTAB2         READE     TABEL                                  30
210500020215     C*
210600020215     C     *IN30         DOWEQ     *OFF
210700020215     C     TBLFLG        IFEQ      ' '
210800020215     C                   MOVEL     TBLUNI        DS5E
210900020215     C     §5EFTC        IFNE      ' '
211000020215     C                   MOVEL     TBLKEY        TSP(C)
211100020215     C                   MOVEL     §5EFTC        TPT(C)
211200020215     C                   ADD       1             C
211300020215     C                   ENDIF
211400020215     C                   ENDIF
211500020215     C     KTAB2         READE     TABEL                                  30
211600020215     C                   ENDDO
211700020215     C***
211800020215     C* CARICO TARIFFE PARTICOLARI PADRONCINI
211900020215     C***
212000020215     C                   Z-ADD     1             C
212100020215     C                   MOVEL     '8A'          COD
212200020215     C     KTAB2         SETLL     TABEL
212300020215     C     KTAB2         READE     TABEL                                  30
212400020215     C*
212500020215     C     *IN30         DOWEQ     *OFF
212600020215     C     TBLFLG        IFEQ      ' '
212700020215     C                   MOVEL     TBLUNI        DS8A
212800020215     C                   MOVEL     TBLKEY        FTP(C)
212900020215     C                   MOVEL     §8AUSC        USC(C)
213000020215     C                   MOVEL     §8AUSR        USR(C)
213100020215     C                   MOVEL     §8ATBA        TBA(C)
213200020215     C                   MOVEL     §8ASPM        SPM(C)
213300020402     C                   MOVE      §8ACE1        CE1(C)
213400020402     C                   MOVE      §8ACE2        CE2(C)
213500020402     C                   MOVE      §8ACE3        CE3(C)
213600020402     C                   MOVE      §8ACE4        CE4(C)
213700020402     C                   MOVE      §8Afc1        fc1(C)
213800020402     C                   MOVE      §8Afc2        fc2(C)
213900020402     C                   MOVE      §8Afc3        fc3(C)
214000020402     C                   MOVE      §8Afc4        fc4(C)
214100020215     C                   ADD       1             C
214200020215     C                   ENDIF
214300020215     C     KTAB2         READE     TABEL                                  30
214400020215     C                   ENDDO
214500020215      *
214600020215     C* CARICO DATI TABELLA moneta CORRENTE/CONTO gestionale
214700020215     C                   MOVEL     'GED'         TBECOD
214800020215     C                   MOVEL     *BLANKS       TBEKE1
214900020215     C                   MOVEL     '1'           TBEKE1
215000020215     C     KTBE          CHAIN     TNTBE01L                           22
215100020215     C  N22              MOVEL     TBEUNI        DGED
215200020215     C*
215300090914     C* Carica tabelle Tariffe Particolari su Cliente
215400090914     C*  Memorizzandosi, una volta per tutte, tutte le tariffe inserite
215500090914      *----
215600090914     C                   Z-ADD     0             CP                5 0
215700090914     C                   movel(P)  'CTP'         tbeCOD
215800090914     c                   clear                   KCliTrovato
215900090914     c                   clear                   KCliPar
216000090914     c                   clear                   KCliParTSR
216100090914     c                   clear                   KTarStdNew
216200090914     c                   clear                   KTarStd
216300090914     c                   clear                   KTarNew
216400090914     c                   clear                   KTarObb
216500090914     c                   clear                   KTarUgu
216600090914     c                   clear                   KSTDDaExcl
216700090915     c                   clear                   KTsrNewCli
216800090914      *----
216900090914     C     tbeCOD        setll     tntbe01l
217000090914     C     tbeCOD        reade     tntbe01l
217100090914      *
217200090914     c                   dow       not %eof(tntbe01l)
217300090914     C                   add       1             CP
217400090914     C                   MOVEL     TBeKE1        KCliPar(CP)
217500090914     C                   MOVEL     TBeKE1        KCliParTSR(CP)
217600090914     C                   MOVEL     TBeKE2        primoByteTSR      1
217700090914      * il tipo servizio alla destra del codice cliente
217800090914     C                   MOVE      primoByteTSR  KCliParTSR(CP)
217900090914     C                   MOVEL     TBeKE2        KTarStd(CP)
218000090914     C                   MOVEL     TBeKE2        KTarStdNew(CP)
218100090914     C                   movel     tbeuni        dCTP
218200090914     C                   MOVE      §CTPCTD       KTarStdNew(CP)
218300090914     C                   MOVE      §CTPCTD       KTarnew(CP)
218400090914     C                   MOVEL     primoByteTSR  KTarnew(CP)
218500090914     C                   MOVEL     §CTPOBB       KTarobb(CP)
218600090914     C                   MOVEL     §CTPUGU       KTarugu(CP)
218700090915     c                   movel     KTarnew(CP)   KTsrNewCli(CP)
218800090915     c                   move      KCliPar(CP)   KTsrNewCli(CP)
218900090914     C     tbeCOD        reade     tntbe01l
219000090914     c                   enddo
219100090914      *----
219200090914     C*
219300020215     C* DEFINIZIONE CAMPI
219400020215     C     *LIKE         DEFINE    FGTPRG        W01PRG
219500020215     C     *LIKE         DEFINE    FGTPRG        k02PRG
219600020215     C     *LIKE         DEFINE    FGTATA        W01ATA
219700020215     C     *LIKE         DEFINE    FGTARL        W01ARR
219800020215     C     *LIKE         DEFINE    FGTPDR        W01PDR
219900020215     C     *LIKE         DEFINE    FGTPDR        k02PDR
220000020215     C     *LIKE         DEFINE    FGTCTR        W01CTR
220100020215     C     *LIKE         DEFINE    FGTCTR        k02CTR
220200020215     C     *LIKE         DEFINE    FGTTSR        W01TSR
220300020215     C     *LIKE         DEFINE    FGTTSR        k02TSR
220400020215     C     *like         define    fptata        w01mnt
220500020215     C     *like         define    fptata        w01tmg
220600020215     C     *like         define    fptata        w01tig
220700020215     C     *like         define    fptctd        w01ctd
220800020215     C     *like         define    fptctd        codctd
220900020215     C     *LIKE         DEFINE    FPDSGL        W01PTA
221000090928     C     *LIKE         DEFINE    FPDSGL        Peso_Sped
221100020215     C     *LIKE         DEFINE    §TAITA        W01SIA
221200020215     C     *LIKE         DEFINE    §TASET        W01TOT
221300020215     C     *LIKE         DEFINE    §TASET        W01NEG
221400020215     C     *LIKE         DEFINE    §TACAS        WTACAS
221500020215     C     *LIKE         DEFINE    §tatpe        w01tpe
221600020506     C     *LIKE         DEFINE    §tatpe        w01tge
221700020506     C     *LIKE         DEFINE    §tatpe        w01tgp
221800020215     C     *LIKE         DEFINE    §tatst        w01tst
221900020215     C     *LIKE         DEFINE    fptsml        k02sml
222000020215     C     *LIKE         DEFINE    fpttsm        k02tsm
222100020326     C     *LIKE         DEFINE    fcecce        voccce
222200020327     C     *LIKE         DEFINE    fcecce        voccc1
222300020327     C     *LIKE         DEFINE    fcecce        voccc2
222400020326     C     *LIKE         DEFINE    fceice        vocice
222500020328     C     *LIKE         DEFINE    fceice        parice
222600020328     C     *LIKE         DEFINE    fceice        restoice
222700030108     C     *like         define    fptPDR        k01PDR
222800030108     C     *like         define    fptSML        k01SML
222900030108     C     *like         define    fptTSR        k01TSR
223000030108     C     *like         define    fptCTR        k01CTR
223100030108     C     *like         define    fptPRG        k01PRG
223200030108     C     *like         define    fptCTD        k01CTD
223300020215      *
223400020215     C                   ENDSR
223500020326     C*================================================================
223600020426     C* memorizza sulle schiere gli importi da attribuire alla Base   *
223700020326     C*================================================================
223800020426     C     Mem_xBase     BEGSR
223900020426      *
224000020426      * cerca se presente altrimenti cerca il primo libero
224100020426     c                   z-add     1             vc                3 0
224200020426     c     voccce        lookup    vitt(vc)                               91
224300020426     c  N91*blank        lookup    vitt(vc)                               91
224400020426      * imposta in schiera
224500020426     c   91              add       vocice        iitt(vc)
224600020426     c   91              move      voccce        vitt(vc)
224700020426      *
224800020426     C                   ENDSR
224900020426     C*================================================================
225000020426     C* memorizza sulle schiere gli importi da attribuire allo Stop   *
225100020426     C*================================================================
225200020426     C     Mem_xStop     BEGSR
225300020426      *
225400020426      * cerca se presente altrimenti cerca il primo libero
225500020426     c                   z-add     1             vc                3 0
225600020426     c     voccce        lookup    vtst(vc)                               91
225700020426     c  N91*blank        lookup    vtst(vc)                               91
225800020426      * imposta in schiera
225900020426     c   91              add       vocice        itst(vc)
226000020426     c   91              move      voccce        vtst(vc)
226100020426      *
226200020426     C                   ENDSR
226300020426     C*================================================================
226400020426     C* Deve riadattare gli importi precedentemente scritti relativi  *
226500020426     C*  alle voci di C/E della Base della Tariffa                    *
226600020426     C*================================================================
226700020426     C     Sost_FIFCE    BEGSR
226800020426     C*
226900020426     C*  Se ci sono dei valori della base da sostituire
227000020426     c                   do        99            vc
227100020426     C*
227200020426     c                   if        vitt(vc) = *blank
227300020426     c                   leave
227400020426     c                   end
227500020426     C*
227600020426     C* Chiamato dalla valorizzazione
227700020426     c                   if        §tapgm = 'CALCVAL'
227800020426      *
227900020426     c                   move      vitt(vc)      sost_voc          3
228000020426     c                   z-add     iitt(vc)      sost_imp         10 3
228100020426      *
228200020426     c                   exsr      AGG_Sost
228300020426      *
228400020426     C* Chiamato dalla conferma
228500020426     c                   else
228600020426     c                   end
228700020426      *
228800020426     c                   if        vc = 99
228900020426     c                   leave
229000020426     c                   end
229100020426     c                   enddo
229200020426      * -------------
229300020426      *  x lo Stop
229400020426      *    Se è stato azzerato anche lo STOP
229500020426     C     Sost_TST      ifeq      'S'
229600020426      *
229700020426     c                   do        99            vc
229800020426     c                   if        vtst(vc) = *blank
229900020426     c                   leave
230000020426     c                   end
230100020426     C*
230200020426     C* Chiamato dalla valorizzazione
230300020426     c                   if        §tapgm = 'CALCVAL'
230400020426      *
230500020426     c                   move      vtst(vc)      sost_voc          3
230600020426     c                   z-add     itst(vc)      sost_imp         10 3
230700020426      *
230800020426     c                   exsr      AGG_Sost
230900020426      *
231000020426     C* Chiamato dalla conferma
231100020426     c                   else
231200020426     c                   end
231300020426      *
231400020426     c                   if        vc = 99
231500020426     c                   leave
231600020426     c                   end
231700020426     c                   enddo
231800020426      *
231900020426     c                   end
232000020426      * -------------
232100020426     C                   ENDSR
232200020426     C*================================================================
232300020426     C* Aggiorna File per il Conto Economico in sostituzione          *
232400020426     C*================================================================
232500020426     C     AGG_Sost      BEGSR
232600020426      *
232700020426     C                   z-add     §tafgs        fceFGS
232800020426     c                   if        fceFGS=0
232900020426     C                   movel     §tapdr        fceFGS
233000020426     c                   end
233100020426     C                   z-add     §tapdr        fcePDR
233200020426     C                   move      §tatsr        fceTSR
233300020426     C                   z-add     §tandc        fceNDC
233400020426     C                   z-add     §taddc        fceDDC
233500020426     C                   z-add     §taaas        fceAAS
233600020426     C                   z-add     §talnp        fceLNP
233700020426     C                   z-add     §tanrs        fceNRS
233800030320     C                   move      §tacbo        fcecbo
233900020426     c                   if        §tatsr='R' and
234000020426     c                             §tansp=§taksc
234100020426     C                   move      *zero         fceNSP
234200020426     c                   else
234300020426     C                   z-add     §tansp        fceNSP
234400020426     c                   end
234500020426     C*
234600020426     c                   move      sost_voc      fcecce
234700020426     C*
234800020426     C     Kfce          chain     fifce01l
234900020426     c                   if        %found(fifce01l)
235000020426     C*  sottrae il valore sostituito dall'accessorio
235100020426     C                   sub       sost_Imp      fceice
235200020426     C*
235300020426     c                   if        fceice <> 0
235400020426     C                   update    fifce000
235500020426     c                   else
235600020426     C*  se il risultato è = 0 allora cancella il record come se
235700020426     C*   non fosse mai stato scritto.
235800020426     C                   delete    fifce000
235900020426     c                   end
236000020426     c                   end
236100030108      *
236200030108     C                   ENDSR
236300030108     C*================================================================
236400030108     C* Imposta i dati per il Conto Economico                         *
236500030108     C*================================================================
236600030108     C     xContoEcon    BEGSR
236700030108      *
236800030108     C* solo per conteggi reali e non simulati
236900030108     C* e solo se c'è un valore valido
237000030108     C*  sia in positivo che in negativo
237100030108     c     VOCice        ifnE      0
237200030108      *
237300030108      * oppure fare attenzione alla scrittura per il Picking
237400030108     c     VOCfce        oreq      'S'
237500030108     c     fptCTD        andne     'PCK'
237600030108     c     fptCTD        andne     'ETI'
237700030108     c     fptCTD        andne     'ETC'
237800030108      *
237900030108     c     VOCfce        oreq      'S'
238000030108     c     fptCTD        andeq     'PCK'
238100030108     c     §taPPE        andeq     'S'
238200030108     c     §tatsr        andne     'G'
238300030108      *
238400030108     c     VOCfce        oreq      'S'
238500030108     c     fptCTD        andeq     'PCK'
238600030108     c     §tapgp        andeq     'S'
238700030108     c     §tatsr        andeq     'G'
238800030108      *
238900030108     c     VOCfce        oreq      'S'
239000030108     c     fptCTD        andeq     'ETI'
239100030108     c     §taPPE        andeq     'S'
239200030108     c     §tatsr        andne     'G'
239300030108      *
239400030108     c     VOCfce        oreq      'S'
239500030108     c     fptCTD        andeq     'ETC'
239600030108     c     §taPPE        andeq     'S'
239700030108     c     §tatsr        andne     'G'
239800030108      *
239900030108     c     VOCfce        oreq      'S'
240000030108     c     fptCTD        andeq     'ETI'
240100030108     c     §tapge        andeq     'S'
240200030108     c     §tatsr        andeq     'G'
240300030108      *
240400030108      * deve scrivere le voci per il picking solo se richiesto
240500030108      * altrimenti nonostante la forzatura sulla 8A non deve scrivere
240600030108      *              *-------------------------*
240700030108     c                   exsr      AGG_FIFCE
240800030108      *              *-------------------------*
240900030108      *
241000030108     c                   end
241100020426      *
241200020426     C                   ENDSR
241300020426     C*================================================================
241400020426     C* Aggiorna File per il Conto Economico                          *
241500020426     C*================================================================
241600020426     C     AGG_FIFCE     BEGSR
241700020326     C*
241800020326     C* Chiamato dalla valorizzazione
241900020426     c                   if        §tapgm = 'CALCVAL'
242000020326     C*
242100020328     C* se il tipo somma è a stop
242200020328     C*  deve ripartire la voce di c/economico su tutte le bolle
242300020328     C*   appartenenti allo STOP.
242400020328     c     §tatsm        ifeq      'T'
242500020328     C*                *--------------------*
242600020328     c                   exsr      VOC_Stop
242700020328     C*                *--------------------*
242800020328     c                   else
242900020328     C*
243000020326     C                   z-add     §tafgs        fceFGS
243100020326     c                   if        fceFGS=0
243200020326     C                   movel     §tapdr        fceFGS
243300020326     c                   end
243400020326     C                   z-add     §tapdr        fcePDR
243500020326     C                   move      §tatsr        fceTSR
243600020326     C                   z-add     §tandc        fceNDC
243700020326     C                   z-add     §taddc        fceDDC
243800020326     C                   z-add     §taaas        fceAAS
243900020326     C                   z-add     §talnp        fceLNP
244000020326     C                   z-add     §tanrs        fceNRS
244100030320     C                   move      §tacbo        fcecbo
244200020326     c                   if        §tatsr='R' and
244300020326     c                             §tansp=§taksc
244400020326     C                   move      *zero         fceNSP
244500020326     c                   else
244600020326     C                   z-add     §tansp        fceNSP
244700020326     c                   end
244800020326     c                   move      voccce        fcecce
244900020326     C*
245000020326     C     Kfce          chain     fifce01l
245100020326     c                   if        %found(fifce01l)
245200020326     C*  somma algebrica del valore
245300020326     C                   add       vocice        fceice
245400020326     C                   update    fifce000
245500020326     C*
245600020326     c                   else
245700020326     C                   clear                   fifce000
245800020326     C                   z-add     §tafgs        fceFGS
245900020326     c                   if        fceFGS=0
246000020326     C                   movel     §tapdr        fceFGS
246100020326     c                   end
246200020326     C                   z-add     §tapdr        fcePDR
246300020326     C                   move      §tatsr        fceTSR
246400020326     C                   z-add     §tandc        fceNDC
246500020326     C                   z-add     §taddc        fceDDC
246600020326     C                   z-add     §taaas        fceAAS
246700020326     C                   z-add     §talnp        fceLNP
246800020326     C                   z-add     §tanrs        fceNRS
246900030320     C                   move      §tacbo        fcecbo
247000020326     c                   if        §tatsr='R' and
247100020326     c                             §tansp=§taksc
247200020326     C                   move      *zero         fceNSP
247300020326     c                   else
247400020326     C                   z-add     §tansp        fceNSP
247500020326     c                   end
247600020326     c                   move      voccce        fcecce
247700020326     C*  somma algebrica del valore
247800020326     C                   add       vocice        fceice
247900020326     C                   write     fifce000
248000020326     c                   end
248100020326     C*
248200020328     c                   end
248300020328     C*
248400020328     c                   else
248500020327     C*  ------------------------>
248600020327     C*  chiamato dalla Conferma   solo per il calcolo della Base
248700020327     C*  ------------------------>
248800020327     C*   deve creare un file di work poichè la conferma conteggi imposta
248900020327     C*  i dati a video che in seguito vengono confermati.
249000020327     C*   A causa di ciò il programma di calcolo deve preparare le voci in
249100020327     C*  anticipo e poi sommarle alle righe già impostate dalla valorizzazione.
249200020327     C*  ------------------------>
249300020327     C*  ma non deve scrivere gli importi e i records della giornata
249400020327     C*  ------------------------>
249500020327      *
249600020327     C                   z-add     §tafgs        fcWFGS
249700020327     c                   if        fcWFGS=0
249800020327     C                   movel     §tapdr        fcWFGS
249900020327     c                   end
250000020327     C                   z-add     §tapdr        fcWPDR
250100020328     C                   move      §tatsr        fcWTSR
250200020327     C                   z-add     0             fcWNDC
250300020617     C                   if        §tatsr ='G'
250400020617     c                   z-add     999           fcWNDC
250500020617     c                   end
250600020327     C                   z-add     §taddc        fcWDDC
250700020327     C                   z-add     §taaas        fcWAAS
250800020327     C                   z-add     §talnp        fcWLNP
250900020327     C                   z-add     §tanrs        fcWNRS
251000020327     c                   if        §tatsr='R' and
251100020327     c                             §tansp=§taksc
251200020327     C                   move      *zero         fcWNSP
251300020327     c                   else
251400020327     C                   z-add     §tansp        fcWNSP
251500020327     c                   end
251600020617     c                   move      *blank        codt8a
251700020617      *
251800020617      *  imposta se a prestazione il codice della tariffa nel campo T8A
251900020617     c                   if        §tactr=999
252000020617     c                   movel     §tatsr        fld4              4
252100020617     c                   move      §tactr        fld4
252200020617     c                   movel     fld4          codt8a            8
252300020617     c                   move      fptCTD        codt8a
252400020617     c                   end
252500020617      *
252600020328      * consegne/ritiri
252700020327     c     §TAtsr        ifne      'G'
252800020617      *
252900020327     c                   move      voccce        fcWcce
253000020617      *
253100020327     c                   else
253200020328      *  se a giornata
253300020328      * se solo consegne
253400020327     c     vocusc        ifeq      'S'
253500020327     c     vocusr        andeq     ' '
253600020327     c                   move      voccc1        voccce
253700020327     c                   move      voccc1        fcWcce
253800020328     C                   move      'C'           fcWTSR
253900020327     c                   end
254000020328      * se solo ritiri
254100020327     c     vocusr        ifeq      'S'
254200020327     c     vocusc        andeq     ' '
254300020327     c                   move      voccc2        voccce
254400020327     c                   move      voccc2        fcWcce
254500020328     C                   move      'R'           fcWTSR
254600020327     c                   end
254700020327      * da ripartire su entrambe consegne e ritiri
254800020328      *  in base ai pesi ??????
254900020327     c     vocusc        ifeq      'S'
255000020327     c     vocusr        andeq     'S'
255100020327      *
255200020327      *
255300020327     c                   end
255400020617      *
255500020503      * Attenzione:
255600020503      * per il Picking/Etichettatura a GIORNATA
255700020503      * per le vecchie tariffe convertite deve riproporzionare
255800020503      *  l'importo impostato per la voce tenendo conto che deve
255900020503      *   essere ripartito fra Ritiri e Consegne
256000020503      *   ...per le nuove tariffe invece non deve più effettuare la
256100020503      *    ripartizione fra Ritiri e Consegne.
256200021216     C                   if        fptCTD = 'PCK' or fptCTD = 'ETI'
256300020503      *
256400020503     c     §takgc        add       §takgr        tot_kgcr          9 1
256500020506      *********
256600020506      * asteriscato dopo avere sdoppiato ETI e PCK per le nuove tariffe a giornata
256700020506      *********
256800020506     C**** fcWTSR        ifeq      'C'
256900020506     c**** tot_kgcr      andne     0
257000020506     c*********          eval      vocice = vocice * §takgc /tot_kgcr
257100020506     c*********          end
257200020506     C**** fcWTSR        ifeq      'R'
257300020506     c**** tot_kgcr      andne     0
257400020506     c*********          eval      vocice = vocice * §takgr /tot_kgcr
257500020506     c*********          end
257600020506      *********
257700020503     c                   endif
257800020328      *
257900020327     c                   end
258000020327      *
258100020327     C     Kfcw          chain     fifceW1l
258200020327      *
258300020327     c                   if        %found(fifceW1l)
258400020327      *
258500020327     C*  somma algebrica del valore
258600020329     C                   add       vocice        fcwice
258700020617      *
258800020617     c                   if        fcWt8a<>codT8a and                            prestazione
258900020619     c                             §tactr=999                                    e giornata
259000020619     c                             or §tactr<>999                               o non prestaz.
259100020617      *
259200020327     C                   update    fifcW000
259300030415     c                   feod      fifcew1L
259400020619     c                   else
259500020619     c                   unlock    fifcew1l
259600020619     c                   end
259700020327     C*
259800020327     c                   else
259900020327     C*
260000020327     C                   clear                   fifcW000
260100020327     C                   z-add     §tafgs        fcWFGS
260200020327     c                   if        fcWFGS=0
260300020327     C                   movel     §tapdr        fcWFGS
260400020327     c                   end
260500020327     C                   z-add     §tapdr        fcWPDR
260600020328      *
260700020328     c     §tatsr        ifne      'G'
260800020327     C                   move      §tatsr        fcWTSR
260900020328     c                   else
261000020328      * solo a giornata
261100020328     c                   select
261200020328      * se solo consegne
261300020328     c     vocusc        wheneq    'S'
261400020328     c     vocusr        andeq     ' '
261500020328     C                   move      'C'           fcWTSR
261600020328      * se solo ritiri
261700020328     c     vocusr        wheneq    'S'
261800020328     c     vocusc        andeq     ' '
261900020328     C                   move      'R'           fcWTSR
262000020328      *
262100020328     c                   other
262200020328     C                   move      §tatsr        fcWTSR
262300020328     c                   endsl
262400020328     c                   endif
262500020328      *
262600020327     C                   z-add     0             fcWNDC
262700020617     C                   if        §tatsr ='G'
262800020617     c                   z-add     999           fcWNDC
262900020617     c                   end
263000020327     C                   z-add     §taddc        fcWDDC
263100020327     C                   z-add     §taaas        fcWAAS
263200020327     C                   z-add     §talnp        fcWLNP
263300020327     C                   z-add     §tanrs        fcWNRS
263400020327     c                   if        §tatsr='R' and
263500020327     c                             §tansp=§taksc
263600020327     C                   move      *zero         fcWNSP
263700020327     c                   else
263800020327     C                   z-add     §tansp        fcWNSP
263900020327     c                   end
264000020327     c                   move      voccce        fcWcce
264100020327     C*  somma algebrica del valore
264200020402     C                   z-add     §takgc        fcwkgc
264300020402     C                   z-add     §takgr        fcwkgr
264400020327     C                   add       vocice        fcWice
264500020617     c                   movel     codt8a        fcWt8a
264600020327     C                   write     fifcW000
264700030415     c                   feod      fifceW1L
264800020327     c                   end
264900020327     C*
265000020326     c                   end
265100020326     C*
265200020326     C                   EndSR
265300020214     C*===============================================================
265400020328     C* Ripartisce il valore Voce C/E x stop sulle bolle dello stop
265500020328     C*===============================================================
265600020328     C     VOC_Stop      BegSR
265700020328     C*
265800020328     C                   z-add     §tafgs        ftwFGS
265900020328     c                   if        §taFGS=0
266000020328     C                   movel     §tapdr        ftwFGS
266100020328     c                   end
266200020328     C                   z-add     §tapdr        ftwPDR
266300020328     C                   move      §tatsr        ftwTSR
266400020328     C                   z-add     §tandc        ftwNDC
266500020328     C                   z-add     §taddc        ftwDDC
266600020328     C                   z-add     §tastp        ftwSTP
266700020328     C                   z-add     vocice        restoICE
266800020328     C*
266900020328     C* ciclo su tutte le bolle appartenenti allo STOP
267000020328     C     Kftdw         setll     fiftdw1l
267100020328     C     Kftdw         reade     fiftdw1l
267200020328     c                   dow       not %eof(fiftdw1l)
267300020328     C*
267400020328     C* Crea il valore da ripartire:
267500020328     C*  proporziona la PARte relativa al peso della spedizione e si
267600020328     C*   memorizza il resto
267700020403     c                   eval      parice = 0
267800020403     c                   if        §takfa > 0
267900020329     c                   eval      parice = vocice * ftwpkl / §takfa
268000020403     c                   end
268100020328     c                   eval      restoice = restoice - parice
268200020328     C*
268300020328     C                   z-add     ftwfgs        fceFGS
268400020328     c                   if        fceFGS=0
268500020328     C                   movel     ftwpdr        fceFGS
268600020328     c                   end
268700020328     C                   z-add     ftwpdr        fcePDR
268800020328     C                   move      ftwtsr        fceTSR
268900020328     C                   z-add     ftwndc        fceNDC
269000020328     C                   z-add     ftwddc        fceDDC
269100020328     C                   z-add     ftwaas        fceAAS
269200020328     C                   z-add     ftwlnp        fceLNP
269300020328     C                   z-add     ftwnrs        fceNRS
269400020328     C                   z-add     ftwnsp        fceNSP
269500030320     c                   if        ftwtsr = 'R'
269600030320     c     kbol          chain     fnblp01l
269700030320     c                   move      blpcbo        fcecbo
269800030320     c                   else
269900030320     c     kbol          chain     fnarb01l
270000030320     c                   move      arbcbo        fcecbo
270100030320     c                   end
270200020328     c                   move      voccce        fcecce
270300020328     C*
270400020328     C     Kfce          chain     fifce01l
270500020328     c                   if        %found(fifce01l)
270600020328     C*  somma algebrica del valore
270700020328     C                   add       parice        fceice
270800020328     C                   update    fifce000
270900020328     C*
271000020328     c                   else
271100020328     C*
271200020328     C                   clear                   fifce000
271300020328     C                   z-add     ftwfgs        fceFGS
271400020328     c                   if        fceFGS=0
271500020328     C                   movel     ftwpdr        fceFGS
271600020328     c                   end
271700020328     C                   z-add     ftwpdr        fcePDR
271800020328     C                   move      ftwtsr        fceTSR
271900020328     C                   z-add     ftwndc        fceNDC
272000020328     C                   z-add     ftwddc        fceDDC
272100020328     C                   z-add     ftwaas        fceAAS
272200020328     C                   z-add     ftwlnp        fceLNP
272300020328     C                   z-add     ftwnrs        fceNRS
272400020328     C                   z-add     ftwnsp        fceNSP
272500020328     c                   move      voccce        fcecce
272600020328     C*  somma algebrica del valore
272700020328     C                   add       parice        fceice
272800030320     c                   if        ftwtsr = 'R'
272900030320     c     kbol          chain     fnblp01l
273000030320     c                   move      blpcbo        fcecbo
273100030320     c                   else
273200030320     c     kbol          chain     fnarb01l
273300030320     c                   move      arbcbo        fcecbo
273400030320     c                   end
273500020328     C                   write     fifce000
273600020328     c                   end
273700020328     C*
273800020328     C     Kftdw         reade     fiftdw1l
273900020328     c                   enddo
274000020328     C*
274100020328     C*  se c'è un resto ancora da attribuire lo ripartiamo sull'ultima
274200020328     C*  bolla letta.
274300020423     c                   if        restoICE <> 0
274400020328     C*
274500020328     C     Kftdw         setgt     fiftdw1l
274600020328     C     Kftdw         readpe    fiftdw1l
274700020328     c                   if        not %eof(fiftdw1l)
274800020328     C*
274900020328     C                   z-add     ftwfgs        fceFGS
275000020328     c                   if        fceFGS=0
275100020328     C                   movel     ftwpdr        fceFGS
275200020328     c                   end
275300020328     C                   z-add     ftwpdr        fcePDR
275400020328     C                   move      ftwtsr        fceTSR
275500020328     C                   z-add     ftwndc        fceNDC
275600020328     C                   z-add     ftwddc        fceDDC
275700020328     C                   z-add     ftwaas        fceAAS
275800020328     C                   z-add     ftwlnp        fceLNP
275900020328     C                   z-add     ftwnrs        fceNRS
276000020328     C                   z-add     ftwnsp        fceNSP
276100020328     c                   move      voccce        fcecce
276200020328     C*
276300020328     C     Kfce          chain     fifce01l
276400020328     c                   if        %found(fifce01l)
276500020328     C*  somma algebrica del valore
276600020328     C                   add       restoice      fceice
276700020328     C                   update    fifce000
276800020328     c                   end
276900020328     C*
277000020328     c                   end
277100020328     C*
277200020328     c                   end
277300020328     C*
277400030108     C                   EndSR
277500030108     C*===============================================================
277600030108     C* ECCEZIONI :
277700030108     C*===============================================================
277800030108     C* Ritiri Annullati fanno Eccezione:
277900030108     C*===============================================================
278000030108     C     Ritiri_Ann    BegSR
278100030108     C*
278200030108     C* Poichè vengono calcolati al momento in cui il programma è chiamato
278300030108     C* a prestazione ossia con tariffa R999 e hanno una tariffa definita
278400030108     C* a dettaglio anche se poi in fondo è un valore fisso moltiplicato
278500030108     C* per il numero dei ritiri.
278600030108     C*
278700030108     C*---------------------------------------------------------------*
278800030108     C                   clear                   ficnb3
278900030108     C                   movel     §tapdr        §t3pdr
279000030108     C                   movel     §tasml        §t3sml
279100030108     C                   movel     'V'           §t3fvl
279200030108     C                   movel     §tatsr        §t3tsr
279300030108     C                   z-add     §tandc        §t3ndc
279400030108     C                   z-add     §taddc        §t3ddc
279500030108     C                   movel     §tadiv        §t3div
279600030108     C                   movel     'S'           §t3ran
279700030108     C                   CALL      'FICNB3R1'
279800030108     C                   PARM                    ficnb3
279900030331      * se non c'è una tariffa
280000030331     C                   if        §t3CTR = '???'
280100030331     c                   goto      fine_ritann
280200030331     c                   endif
280300030108      ****
280400030108      * reperisce il progressivo giusto per prendere la tariffa corretta
280500030108     C                   movel     §t3PDR        W01PDR
280600030108     C                   movel     §t3CTR        W01CTR
280700030108     C                   movel     §t3TSR        W01TSR
280800030108      *               *-------------------------*
280900030108     C                   exsr      Ric_Tariffa
281000030108      *               *-------------------------*
281100030108     C     Tar_valida    ifeq      'N'
281200030108     c                   goto      fine_ritann
281300030108     C                   endIF
281400030108      *
281500030108     C* SE C'E' RIFERIMENTO ALTRA TARIFFA USO QUESTO PER IL DETTAGLIO
281600030108    1C     FGTRAP        IFGT      0
281700030108     C                   Z-ADD     FGTRAP        W01PDR
281800030108     C                   Z-ADD     FGTRCT        W01CTR
281900030108      *               *-------------------------*
282000030108     C                   EXSR      Ric_Tariffa
282100030108      *               *-------------------------*
282200030108     C     Tar_valida    ifeq      'N'
282300030108     c                   goto      fine_ritann
282400030108    1C                   end
282500030108    1C                   ENDIF
282600030108     C*
282700030108     C*---------------------------------------------------------------*
282800030108      * Chiave di lettura
282900030108     C                   eval      k01PDR  =   fgtPDR
283000030108     C                   eval      k01SML  =   fgtSML
283100030108     C                   eval      k01TSR  =   fgtTSR
283200030108     C                   eval      k01CTR  =   fgtCTR
283300030108     C                   eval      k01PRG  =   fgtPRG
283400030108     C                   move      'RAN'         k01CTD
283500030108      *
283600030108      * Leggendo per livello di Tipo Somma
283700030108     C     kfpt01        chain     fifpt01l
283800030108      *
283900030108     C                   if        %found(fifpt01L) and F01atb=*blank
284000030108     c                   move      f01CTD        fptctd
284100030404     C*
284200030404     C*  Aggancia la Tabella per prendere la Voce del Conto.
284300030404     c                   Exsr      Tab_8A
284400030108     C*
284500030108     c     f01ata        mult      §tatbs        w01RAN           18 3
284600030108      *
284700030108     c     Piumen        ifeq      '-'
284800030108     C                   z-sub     w01RAN        §TARAN
284900030108     c                   sub       w01RAN        vocice
285000030108     c                   else
285100030108     C                   z-add     w01RAN        §TARAN
285200030108     c                   add       w01RAN        vocice
285300030108     c                   end
285400030108      *
285500030108      *  Non deve aggiornare se in simulazione
285600030108      *   Prepara i dati per il FIFCE per comparazione al Conto Economico
285700030108     c                   if        §tasml=' '
285800030108     c                   Exsr      xContoEcon
285900030108     c                   end
286000040506     C*
286100040506     c                   else
286200040506     C*
286300040506     C* se non trovata tariffa su tariffa di riferimento
286400040506     C                   eval      Tar_valida = 'N'
286500030108     C*
286600030108     c                   end
286700030108     C*
286800030108     C     fine_ritann   EndSR
286900020328     C*===============================================================
287000030529     C* Ci sono Tariffe Particolari ?! da sapere prima di tassare ?
287100030529     C*===============================================================
287200030529     C     Tariffe_Part  BegSR
287300030529     C*
287400030529     C*  Pulisce i flags Particolari da impostare
287500030529     C*    x tariffa FW
287600030529     c                   clear                   Bolla_FW          1
287700030821     c                   clear                   Bolla_2R          1
287800091009     c                   move      'NO'          Si_TarH1030       2
287900030529     C* ------
288000030529      * Chiave di lettura generale prima a Spedizione e poi a Stop
288100030529     C                   eval        k02PDR  =   fgtPDR
288200030529     C                   eval        k02SML  =   fgtSML
288300030529     C                   eval        k02TSR  =   fgtTSR
288400030529     C                   eval        k02CTR  =   fgtCTR
288500030529     C                   eval        k02PRG  =   fgtPRG
288600030529      *
288700030529     c                   do        2             volte             3 0
288800030529      * imposta il Tipo Somma
288900030529     c                   if        volte = 1
289000030529     C                   eval        k02TSM  =   'S'
289100030529     c                   end
289200030529     c                   if        volte = 2
289300030529     C                   eval        k02TSM  =   'T'
289400030529     c                   end
289500030529      *
289600030529     C     kfpt02        setll     fifpt002
289700030529     C     kfpt02        READE     fifpt002
289800030529      *
289900030529     C                   Dow       not %Eof(Fifpt02l)
290000030529     C* ------>
290100100108      ***++++++++++++
290200091009      *  se NON presente tariffa H10:30 se lo memorizza poichè, le spedizioni in ESPRESSO
290300091009      *   dovranno allora essere valorizzate con la vecchia "E" Espresso = Priority.
290400100108      ***++++++++++++
290500100108     c***++++++++++++    if        fptctd='H  ' and §taTSP='H' and fptatb=' '
290600100108     c***++++++++++++    eval      Si_TarH1030 = 'SI'
290700100108     c***++++++++++++    end
290800100108     c***++++++++++++  SI E'DECISO DI NON AVERE PIU' UNA TARIFFA SPECIFICA X H_10:30
290900100108     c***++++++++++++   altrimenti basta disasteriscare queste 3 specifiche.
291000091009      *
291100030529     C* Se è una bolla Particolare. x Recupero C/A
291200030529     C*  questo tipo di bolla non deve valorizzarsi con tutte le tariffe
291300030529     C* ma solo con un fisso particolare.
291400030529     c                   if        fptctd = 'FW ' and §taCBO ='FW' and
291500030529     c                             fptatb = ' '
291600030821     C* sempre a spedizione e a stop solo se non ha altre spedizioni nello stesso Stop
291700030821     c*******            if        §taTSM ='S' or
291800030821     c*******                      §taTSM ='T' and (§tancl = 1 or §tacpe = 1)
291900030821     c                   if        §tanss = 1 or
292000030821     c                             §taNSS = §taFW
292100030529     c                   move      'S'           Bolla_FW
292200030529     C                   End
292300030529     C                   End
292400030529     C* ------>
292500030821     C* Se è una bolla Particolare. x Ritiro Contestuale alla Consegna
292600030821     C*  questo tipo di bolla non deve valorizzarsi con tutte le tariffe
292700030821     C* ma solo con un fisso particolare.
292800030821     c                   if        fptctd = 'RCC' and §taCBO ='2R' and
292900030821     c                             fptatb = ' '
293000030821     C* sempre a spedizione e a stop solo se non ha altre spedizioni nello stesso Stop
293100030821     c                   if        §taNSS = 1 or
293200030821     c                             §taNSS = §taRCC
293300030821     c                   move      'S'           Bolla_2R
293400030821     C                   End
293500030821     C                   End
293600030821     C* ------>
293700030529      *
293800030529     C     kfpt02        READE     fifpt002
293900030529     C                   EndDO
294000030529      *
294100030529     C                   EndDO
294200030529     C* ------
294300090914      * Carica Tariffe Particolari
294400090914      *   Se il programma è chiamato in modalità "S" a Spedizione ossia il TSM ="S"
294500090914      *   deve essere passato il codice dell'autista con tariffe particolari
294600090914      *   con il quale andare a controllare sulla tabella CTP se ci sono delle tariffe
294700090914      *   da sostituire.
294800090914      *   Se il programma è chiamato in modalità "T" a STOP ossia il TSM ="T" può essere
294900090914      *   passato il codice dell'autista con tariffe particolari o 9999999 se all'interno
295000090914      *   dello stop ci sono spedizioni di clienti con Tariffe Particolari per i
295100090914      *   quali andare a controllare sulla tabella CTP se ci sono tariffe da sostituire.
295200090914      *
295300090914      *   Nel caso con 9999999 non è possibile identificare i clienti con quali tariffe
295400090914      *   quindi si paga in maniera standard.
295500090914      *
295600090914     c                   eval      Tar_da_sostituire = ' '
295700090914     c                   eval      x_il_cliente   = 0
295800090914      *
295900090914      *  deve essere però specifica sul cliente x essere applicata
296000090915     c                   if        (§TACLIPAR > 0 and §TACLIPAR < 9999999)
296100090915     c                               or §TATUTTE  ='S'
296200090915      *
296300090915     **  quindi ci sono tariffe particolari sul cliente da prendere in considerazione
296400090914     c                   eval      Tar_da_sostituire = 'S'
296500090914     c                   eval      x_il_cliente      = §TACLIPAR
296600090914      *
296700090914     C                   End
296800090914      *
296900030529     C                   EndSR
297000030529     C*===============================================================
297100090914      *
297200090914     C*===============================================================
297300090914     C     Ignora_TariffaBegSR
297400090915      *
297500090915     c                   clear                   Tariffa_Letta     4
297600090915     c                   eval      Tariffa_Letta = fptTSR + fptCTD
297700090915      * >>>>>>>>>>>
297800090915     c                   IF         Tar_da_sostituire = *blank
297900090915      * >>>>>>>>>>>
298000090915      *  Deve comunque controllare se la tariffa letta è una tariffa di quelle
298100090915      *   da utilizzare SOLO in sostituzione
298200090915      * se c'è in tabella allora è una di quelle da saltare
298300090915     c     Tariffa_Letta lookup    KtarNEW                                33
298400090915     c                   if        *in33
298500090915     c                   eval      tariffa_da_saltare = 'S'
298600090915     c                   end
298700090915      *
298800090915      * >>>>>>>>>>>
298900090915     c                   ElseIF     Tar_da_sostituire = 'S'
299000090915      * >>>>>>>>>>>
299100090915      *  cicla su tutte le tariffe
299200090915      * deve applicara le tariffe in sostituzione x l'autista
299300090914     c                   z-add     1             CP
299400090915     c                   doW         CP <= %elem(KCliPar)
299500090915      *
299600090915      *   esce quando ha finito gli elementi validi
299700090915     c                   if          KCliPar(CP) = 0
299800090915     c                   leave
299900090915     c                   end
300000090915      * --------
300100090915      *  se è quelle da considerare del cliente per la sostituzione
300200090915     c                   iF          x_il_cliente = KCliPar(CP)
300300090915      *
300400090915      *  controlla se è la tariffa particolare
300500090915     c                   if        KtarNew(CP) = Tariffa_Letta
300600090915      * allora NON deve poi utilizzare la relativa tariffa Standard collegata.
300700090914     c                   eval      KSTDDaExcl(CP) = KtarSTD(CP)
300800090914     c                   end
300900090915      *
301000090914      *  controlla se la tariffa Standard è già stata sostituita nel calcolo
301100090915     c                   if        KtarSTD(CP) = Tariffa_Letta  and
301200090914     c                             KSTDDaExcl(CP) = KtarSTD(CP)
301300090915      *  e quindi NON deve essere utilizzata.
301400090914     c                   eval      tariffa_da_saltare = 'S'
301500090915     c                   end
301600090915      * --------
301700090915     c                   else
301800090915      * --------
301900090915      *  Dopo aver controllato fra le tariffe specifiche del cliente
302000090915      *  occorre verificare che la tariffa letta non sia fra quelle necessarie
302100090915      *  per la sostituzione per altri clienti
302200090915     c                   if        KtarNew(CP) = Tariffa_Letta
302300090915     c                   movel     Tariffa_Letta Tar_Letta_CLI    11
302400090915     c                   move      x_il_cliente  Tar_Letta_CLI    11
302500090915     c     Tar_Letta_CLI lookup    KTsrNewCli                             33
302600090915      *  se non è stato trovato nello specifico sul cliente allora è una
302700090915      *   tariffa sostitutiva di un altro cliente ed in quanto tale non deve
302800090915      *   essere presa in considerazione su questo cliente.
302900090915     c  N33              eval      tariffa_da_saltare = 'S'
303000090915     c                   end
303100090915      *
303200090915     c                   end
303300090915      * --------
303400090914      *   incrementa il contatore di schiera da leggere
303500090915     c                   eval        CP = CP + 1
303600090914     c                   endDo
303700090915      * >>>>>>>>>>>
303800090914     C                   END
303900090915      * >>>>>>>>>>>
304000090914      *
304100090914     C                   EndSR
304200090914     C*===============================================================
