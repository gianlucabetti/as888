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
001800020129     D TSR             S              1    DIM(99)                              TIPI SERVIZIO VIZ
001900020129     D CTS             S              3    DIM(99)                              CDTAR TIPI SERVIZ
002000020129     D FTP             S              3    DIM(99)                              CONS PART     VIZ
002100020129     D USR             S              1    DIM(99)                              UTILIZZO "R"  VIZ
002200020129     D USC             S              1    DIM(99)                              UTILIZZO "C"  VIZ
002300020204     D TBA             S              1    DIM(99)                              base/accessori
002400020204     D SPM             S              1    DIM(99)                              più/meno
002500020402     D fc1             S              1    DIM(99)                              C/E consegna base
002600020402     D CE1             S              3    DIM(99)                              C/E consegna base
002700020402     D fc2             S              1    DIM(99)                              C/E consegna base
002800020402     D CE2             S              3    DIM(99)                              C/E ritiri   base
002900020402     D fc3             S              1    DIM(99)                              C/E consegna base
003000020402     D CE3             S              3    DIM(99)                              C/E consegna acc.
003100020402     D fc4             S              1    DIM(99)                              C/E consegna base
003200020402     D CE4             S              3    DIM(99)                              C/E ritiri   acc.
003300020129     D UTI             S              1    DIM(99)                              UTILIZZO      VIZ
003400020129     D TSP             S              1    DIM(99)                              TIPI SERVIZIO VIZ
003500020129     D TPT             S              3    DIM(99)                              TAR PART.X TSPVIZ
003600020426      *-  schiere per storno voci C/E in presenza di accessori in
003700020426      *-  sostituzione alla tariffa base.
003800020426     D iitt            S             10  3 DIM(99)                              Importo §TAITT
003900020426     D vitt            S              3    DIM(99)                              Importo §TAITT
004000020426     D itst            S             10  3 DIM(99)                              Importo §TATST
004100020426     D vtst            S              3    DIM(99)                              Importo §TATST
004200020426      *-
004300020125     D Se_Base         S              1                                         se Tar.Base S/N
004400020313     D Se_Access       S              1                                         se Tar.Base S/N
004500021212     D DiskC_ETC       S              1                                         su Ritiri
004600090914      *
004700090914     D   DCTP        E DS
004800090914     D KCliPar         S              7s 0 DIM(9999)                            Tar.Part.sul cliente
004900090914     D KCliParTSR      S              8a   DIM(9999)                            Tar.Part. + TSR
005000090914     D KTarStdNew      S              7A   DIM(9999)                            Tar.Part.+ Tar.Std.
005100090914     D KTarStd         S              4A   DIM(9999)                            Tar.da Sostituire
005200090914     D KTarNew         S              4A   DIM(9999)                            Tar.Sostituente
005300090914     D KTarObb         S              1A   DIM(9999)                            se Obbligatoria
005400090914     D KTarUgu         S              1A   DIM(9999)                            se Uguale come defin
005500090914     D KCliTrovato     S              7s 0 DIM(9999)                            Cliente Presente CTP
005600090914     D KStdNewCli      S             14A   DIM(9999) DESCEND                    Tar.STD+NEW+cliente
005700090915     D KTSRNewCli      S             11A   DIM(9999)                            Tar.TSR+NEW+cliente
005800090914     D KSTDDaExcl      S              4A   DIM(9999)                            Tar.già Sostituite
005900090914      *
006000090914     D tariffa_da_saltare...
006100090914     D                 S              1A
006200090914      *
006300090914     D Tar_da_sostituire...
006400090914     D                 S              1A
006500090914     d x_il_cliente    s              7s 0
006600090914      *
006700090914     D KtutteBolle     S              1A
006800090914      *
006900910423     D WLBDAT          DS
007000910423     D  G02DAT                 1      6  0
007100910423     D  G02INV                 7     12  0
007200910423     D  G02ERR                13     13
007300910423     D  G02TGI                14     18  0
007400910423     D WGIDAT          DS
007500910423     D  GIODAT                 1      6  0
007600910423     D  GIOINV                 7     12  0
007700910423     D  GIOTGI                13     17  0
007800950127     D                 DS
007900950127     D  W01CAP                 1      9
008000971001     D  WDSCAP                 3      5
008100020321      *  per campi relativi al bonus
008200020321     d dsbonus       e ds                  EXTNAME(DFPT01)
008300020125      *
008400020213     d   tar_valida    S              1    INZ('N')
008500020213      *
008600011106     D YEURDS        E DS                  EXTNAME(YEURCODS)
008700011106     D DGED          E DS
008800931022     D DSTR          E DS
008900931025     D DS1Z          E DS
009000931025     D DS8A          E DS
009100980224     D DS5E          E DS
009200020122     D ficnB2        E DS                  EXTNAME(ficnB2DS)
009300030108     D FIcnB3        E DS                  EXTNAME(FICNB3DS)
009400900521     C****************************************************************
009500900521     C*  RIEPILOGO INDICATORI
009600900521     C***************************************************************
009700931025     C* 01    - NON TASSABILI ACCESSORI PER MANCA TARIFFA
009800931025     C* 03    - SOSTITUZIONE DETTAGLIO TARIFFA CON TAR PARTICOLARI
009900931025     C* 04    - TARIFFA BASE GIA' ESISTENTE
010000000621     C* 51    - SOSTITUZIONE DETTAGLIO TARIFFA CON TAR PARTICOLARI (LASCIATO AVVISO/GIACENZA)
010100931022     C* 90    - ON INDICA CHE C'E' UN ERRORE IN TASSAZIONE
010200900521     C*****************************************************************
010300000000     C     *ENTRY        PLIST
010400020122     C                   PARM                    ficnB2
010500030527     C*
010600030527     C* Reset del Flag di lettura della tariffa Etichettatura x Disk C
010700030527     c                   clear                   DiskC_ETC
010800021212     C*
010900020215     C*  se chiamato solo per dare il manca tariffa
011000020215     C*   il programma va subito a fine.
011100020215     c                   if        §taFLG ='M'
011200020215     C                   movel     §taPDR        W01PDR
011300020215     C                   exsr      Manca_TAR
011400020215     C                   goto      fine
011500020215     C                   ENDIF
011600020215     C* o per chiudere
011700020215     c                   if        §taFLG ='C'
011800020215     C                   seton                                        LR
011900020215     C                   goto      fine
012000020215     C                   ENDIF
012100020215     C*
012200020215     C*  Normalmente esegue tutto da qui in avanti.
012300020213     C                   CLEAR                   W01SIA
012400020426     C                   Clear                   iitt
012500020426     C                   Clear                   vitt
012600020426     C                   Clear                   itst
012700020426     C                   Clear                   vtst
012800020426     C                   move      'N'           Sost_ITT          1
012900020426     C                   move      'N'           Sost_TST          1
013000020506      *
013100020506      * Per tariffa a giornata pulisce i campi di pck/eti
013200020506      *  e comunque pulisce i campi del calcolo del picking/etichettatura
013300020506     C                   clear                   w01tge
013400020506     C                   clear                   w01tgp
013500020506     C                   clear                   w01tpe
013600020214      *
013700020214     C* SE TAFGC E' UGUALE A J-GIACENZA O L-LASCIATO AVVISO NON CALCOLO TUTTE LE ALTRE
013800020214     C* TARIFFE PARTICOLARI   A PARTE INCASSI CONTRASSEGNO /ASSEGNATI/COMPETENZE
013900020215     C                   move      'N'           Giac_LAvv         1
014000020214     C     §TAFGC        IFEQ      'J'
014100020214     C     §TAFGC        OREQ      'L'
014200020215     C                   move      'S'           Giac_LAvv
014300020214     C                   ENDIF
014400020325      *
014500020325      * se Isola:
014600020325     c                   move      'N'           se_isola          1
014700020325     c     §tafin        Ifeq      'I'
014800020325     c     §tatsm        andeq     'S'
014900020325     c     §taiso        oreq      'S'
015000020325     c     §tatsm        andeq     'T'
015100020325     c                   move      'S'           se_isola          1
015200020325     c                   end
015300020130      *
015400030403      * In Conferma se a Prestazione Ritiri c'è solo una distinta x Ritiri Annullati
015500030403      *  imposta il flag per NON reperire tariffe a prestazione IAD o ETI
015600030403     c                   move      'N'           Solo_Rit_Ann      1
015700030403     c     §tactr        Ifeq      999
015800030403     c     §tatsr        andeq     'R'
015900030403     c     §tatbs        andgt     0
016000030403     c     §takgr        andeq     0
016100030403     c     §tapgm        andeq     'CONFERM'
016200030403     c                   move      'S'           Solo_Rit_Ann
016300030403     c                   End
016400030403      *
016500020130     C                   clear                   UTI
016600020215     C     §taTSR        IFEQ      'R'
016700020130     C                   MOVEA     USR           UTI                             RITIRI
016800020130     C                   ELSE
016900020130     C                   MOVEA     USC           UTI                             CONSEGNE
017000020130     C                   ENDIF
017100020215     C*
017200020215      *  Prende la Tariffa Principale
017300020215      *    dalla Testata FIFGT
017400020215      *               *-------------------------*
017500020225     C                   exsr      Tariffa_Princ
017600020215      *               *-------------------------*
017700020213      *
017800030529      * Voglio sapere in anticipo se c'è qualche tariffa in Particolare
017900030529      *               *-------------------------*
018000030529     C                   exsr      Tariffe_Part
018100030529      *               *-------------------------*
018200030529      *
018300020213      *    ----------------------------------------------
018400020213      * Una volta presa la tariffa di testata principale entro in
018500020213      *   dettaglio dei singoli codici tariffe.
018600020213      *    ----------------------------------------------
018700020215      * Chiave di lettura
018800020215     C                   eval        k02PDR  =   fgtPDR
018900020215     C                   eval        k02SML  =   fgtSML
019000020215     C                   eval        k02TSR  =   fgtTSR
019100020215     C                   eval        k02CTR  =   fgtCTR
019200020215     C                   eval        k02PRG  =   fgtPRG
019300020215     C                   eval        k02TSM  =   §taTSM
019400020213      *
019500020214      * Leggendo per livello di Tipo Somma
019600020213     C     kfpt02        setll     fifpt002
019700020213     C     kfpt02        reade     fifpt002
019800020213      *
019900020214     c                   dow       not %EOF(fifpt02l)
020000090914     C*
020100090914     C*   Attenzione se ci sono delle tariffe particolari sul cliente
020200090914     C*    che devono essere sostituite con altre.....
020300090914     c                   if        fptatb =' '
020400090914     c                   eval      tariffa_da_saltare = *blank
020500090914     C*
020600090914     C*  controlla se deve ignorare la tariffa
020700090914     C                   exsr      Ignora_Tariffa
020800090914     C*
020900090914     C                   end
021000020215      *
021100020213      *  solo quelle valide
021200020213     c     fptatb        ifeq      *blank
021300090914     c                   IF        tariffa_da_saltare = *blank
021400020215     C*
021500090914     C*
021600020215     C*  Imposta il Peso Tassabile
021700090928     C                   EXSR      moltiplicatore
021800090928      *
021900020215     C*  segno in tab.8A x somma algebrica
022000020322     C                   Exsr      Tab_8A
022100020213      *
022200020213     c                   select
022300020215      * Tariffa Base
022400020213     c                   When      fptCTD = 'BAC' or
022500020213     c                             fptCTD = 'BAR'
022600090915      ***
022700020213      * solo se Sped.da Pagare
022800020325      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
022900020419      *  e non è un solo incasso
023000030527      *  e non è una bolla rec.C/A (FW)
023100030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
023200020213     c     §TAPAG        ifeq      'S'
023300020605     c**** se_isola      andeq     'N'
023400020422     c     §tasic        andne     'S'
023500020514     C     Giac_LAvv     andeq     'N'
023600030821     c     Bolla_FW      andeq     *blank
023700020213     c                   exsr      Base
023800020213     c                   endif
023900020213      *
024000020215      * Tariffa Stop
024100030902     C                   when      fptCTD = 'STC' or
024200090915     c                             fptCTD = 'STT' or
024300030902     c                             fptCTD = 'STR' and Bolla_2R = *blank or
024400030901     c                             fptCTD = 'RCC' and Bolla_2R = 'S'
024500090914      *
024600020215      * solo se Sped.da Pagare e Stop validi
024700020325      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
024800020419      *  e non è un solo incasso
024900030527      *  e non è una bolla rec.C/A (FW)
025000030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
025100020213     c     §TAPAG        ifeq      'S'
025200020215     c     §taSPP        andgt     0
025300020605     c**** se_isola      andeq     'N'
025400020422     c     §tasic        andne     'S'
025500020514     C     Giac_LAvv     andeq     'N'
025600030821     c     Bolla_FW      andeq     *blank
025700020213     c                   exsr      Stop
025800020213     c                   endif
025900020213      *
026000020215      * Tariffa Pick/Etic
026100020213     C                   when      fptCTD = 'PCK' or
026200021212     c                             fptCTD = 'ETI' or
026300021212     c                             fptCTD = 'ETC'
026400030403      *
026500030403      * Quando si è a prestazione di Ritiro R999 e ci sono solo Ritiri Annullati
026600030403      *  non deve prendere la prestazione di Etichettatura
026700030527      *  o non è una bolla rec.C/A (FW)
026800030821      *  o non è una bolla di Ritiro Contestuale alla Consegna (2R)
026900030403     c                   if        Solo_Rit_Ann = 'S' and fptCTD = 'ETI'
027000030527     c                             or Bolla_FW  = 'S'
027100030403     c                   goto      No_Tar_ETI
027200030403     c                   end
027300030403      *
027400021212      * Attenzione:
027500021212      *  sui Ritiri per il cliente abilitato al Disk C (tramite il codice
027600021212      *  trattamento Merce in Bolla) deve valorizzare con la Tariffa "ETC"
027700021212      *  al posto della Tariffa "ETI" (in alternativa) e se non fosse presente
027800021212      *  la tariffa per il Disk C allora deve valorizzare con la normale "ETI".
027900021212      * Per tutti gli altri casi sempre con la normale "ETI".
028000021216      *   E'importantissimo che le 2 tariffe ETC e ETI siano ordinate altrimenti
028100021216      *   il test non può funzionare. Prima deve leggere ETC e in seguito ETI.
028200030527      * Attenzione:
028300030527      *  Variazione Importante alla regola: quando è un disk C, e quindi non etichettato
028400030527      *  dal cliente, deve comunque essere pagato l'etichettatura al padroncino anche se
028500030527      *  questi in testata aveva "N" di etichettatura. (Quindi gli verranno pagati sempre
028600030527      *  i diskC) -> entrerà in vigore da giugno 2003
028700021212     c                   if        §taTSR = 'C' or
028800030217     c                             §taTSR = 'G' or
028900021219     c                             §taTSR = 'R' and fptCTD = 'ETC' and
029000021219     c                             §taDKC = 'S'                    or
029100021212     c                             §taTSR = 'R' and fptCTD = 'ETI' and
029200021212     c                                DiskC_ETC <> 'S'
029300020215      * solo se da Pagare
029400020419      *  e non è un solo incasso
029500020215     c     §taPPE        ifeq      'S'
029600020513     c     §tatsr        andne     'G'
029700021212      *
029800030527      * oppure da giugno 2003 basta che sia un Disk C x etichettatura da dare
029900030527      * al padroncino
030000030527     c     §taDKC        oreq      'S'
030100030527     c     §tatsr        andeq     'R'
030200030527      *
030300020506     c     §tapgp        oreq      'S'
030400020507     c     fptCTD        andeq     'PCK'
030500020513     c     §tatsr        andeq     'G'
030600021212      *
030700020506     c     §tapge        oreq      'S'
030800020507     c     fptCTD        andeq     'ETI'
030900020513     c     §tatsr        andeq     'G'
031000020506      *
031100020506     c     §tasic        ifne      'S'
031200021212      *
031300021216     c                   if        fptCTD = 'ETC'
031400021212      * Per memorizzare che era presente la tariffa x etic. Disk C
031500021212      *  e non permettere il calcolo quindi con la tariffa "ETI"
031600021212      *   se presenti entrambe.
031700021212     c                   move      'S'           DiskC_ETC
031800021212     c                   end
031900021212      *
032000020213     c                   exsr      Pck_Etic
032100020215     c                   endif
032200020506      *
032300020506     c                   endif
032400021212      *
032500021212     c                   endiF
032600030403      *
032700030403     c     No_Tar_ETI    tag
032800020325      *----------------
032900020325      * Tariffa  Espresso Carriaggio di consegna
033000030527      *  e non è una bolla rec.C/A (FW)
033100030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
033200030527     C                   when      fptCTD = 'ECA' and
033300030901     c                             Bolla_FW = *blank
033400020325     C     §taXPR        ifeq      'S'
033500020325     C     §tatsm        andeq     'T'
033600020325     C     §taTSP        oreq      'E'
033700020325     C     §tatsm        andeq     'S'
033800020325     C*
033900091008     C     §taTSP        oreq      'H'
034000091008     C     §tatsm        andeq     'S'
034100091008     C*
034200020419      * solo se Sped.da Pagare e Stop validi
034300020419      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
034400020419      *  e non è un solo incasso
034500020325     c     §TAPAG        ifeq      'S'
034600020605     c**** se_isola      andeq     'N'
034700020422     c     §tasic        andne     'S'
034800020325     c                   exsr      Base_eca
034900020325     c                   endif
035000020325     C*
035100020325     C                   end
035200020325      *
035300020325      * Tariffa  Espresso Stop di consegna
035400030527      *  e non è una bolla rec.C/A (FW)
035500030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
035600030527     C                   when      fptCTD = 'EST' and
035700030901     c                             Bolla_FW = *blank
035800020325     C     §taXPR        ifeq      'S'
035900020325     C     §tatsm        andeq     'T'
036000020325     C     §taTSP        oreq      'E'
036100020325     C     §tatsm        andeq     'S'
036200020325     C*
036300091008     C     §taTSP        oreq      'H'
036400091008     C     §tatsm        andeq     'S'
036500091008     C*
036600020419      * solo se Sped.da Pagare e Stop validi
036700020419      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
036800020419      *  e non è un solo incasso
036900020325     c     §TAPAG        ifeq      'S'
037000020325     c     §taSPP        andgt     0
037100020605     c**** se_isola      andeq     'N'
037200020422     c     §tasic        andne     'S'
037300020325     c                   exsr      Stop_est
037400020325     c                   endif
037500020325     C*
037600020325     C                   end
037700020321      * ------------
037800030109     C* Tariffa Destinatari disagiati (Maggiorazione Carriaggio)
037900030527      *  e non è una bolla rec.C/A (FW)
038000030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
038100030527     C                   when      fptCTD = 'XC ' and
038200030901     c                             Bolla_FW = *blank
038300030109      *
038400030109      *  Se è impostato a Stop come per la maggiorazione a Stop
038500030109      *   deve avere le spedizioni all'interno dello Stop
038600030109      *  tutte omogenee alrimenti non da la maggiorazione.
038700030109     C                   If        §tatsm = 'S' or
038800030109     C                             §tatsm = 'T' and §TADED = 'S'
038900030109      *
039000030109      * se Bolla con segnalazione Dest.Disagiato
039100030109     C* comunque se impostata "X" in uno dei 2 campi la data Consegna
039200030109     C*  su ARBDCM è > 0 come controlla il FICNB1R prima di passare i parametri
039300030109     C                   If        §TATC1 = 'X' or §TATC2 = 'X'
039400030109     C*
039500030109      * solo se Sped.da Pagare
039600030109      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
039700030109      *  e non è un solo incasso
039800030109     c     §TAPAG        ifeq      'S'
039900030109     c**** se_isola      andeq     'N'
040000030109     c     §tasic        andne     'S'
040100030109     C     Giac_LAvv     andeq     'N'
040200030109      * Maggiorazione Carriaggio
040300030109     c                   exsr      Base_Disag
040400030109     c                   end
040500030109     C                   endIF
040600030109      *
040700030109     C                   endIF
040800030109      *
040900030109      * ------------
041000030109      * Tariffa  Stop Destinatari Disagiati (Maggiorazione Stop)
041100030527      *  e non è una bolla rec.C/A (FW)
041200030821      *  e non è una bolla di Ritiro Contestuale alla Consegna (2R)
041300030527     C                   when      fptCTD = 'XS ' and
041400030901     c                             Bolla_FW = *blank
041500030109      *  Essendo a Stop
041600030109      *   deve avere le spedizioni all'interno dello Stop
041700030109      *  tutte omogenee alrimenti non da la maggiorazione.
041800030109     C                   If        §tatsm = 'T' and §TADED = 'S'
041900030109      *
042000021223      * se Bolla con segnalazione Dest.Disagiato
042100030109     C* comunque se impostata "X" in uno dei 2 campi la data Consegna
042200030109     C*  su ARBDCM è > 0 come controlla il FICNB1R prima di passare i parametri
042300021223     C                   If        §TATC1 = 'X' or §TATC2 = 'X'
042400030109      *
042500021223      * solo se Sped.da Pagare e Stop validi
042600021223      *  e non deve essere "ISOLE" ossia anteporto §tafin="I"
042700021223      *  e non è un solo incasso
042800021223     c     §TAPAG        ifeq      'S'
042900021223     c     §taSPP        andgt     0
043000021223     c**** se_isola      andeq     'N'
043100021223     c     §tasic        andne     'S'
043200021223     C     Giac_LAvv     andeq     'N'
043300030109      * Maggiorazione Stop
043400030109     c                   exsr      Stop_disag
043500021223     c                   endif
043600021223      *
043700021223     c                   endIF
043800030109      *
043900030109     c                   endIF
044000021223      * ------------
044100030527      * Tariffa Bonus
044200030402      *  può dare il Bonus solo in presenza di consegne.
044300030527     C                   when      fptCTD = 'BON'
044400030410     c********                     and §takgc > 0
044500020215      *  solo se c'è un Bonus da attribuire
044600020215     c     FptATA        ifgt      0
044700020213     c                   exsr      Bonus
044800020215     c                   end
044900020213      *
045000020215      * Tariffa Minimo Garantito
045100030527     C                   when      fptCTD = 'MGA'
045200020312    4C     §TAtsr        IFEQ      'G'
045300020215     c     Piumen        ifeq      '-'
045400091202     C                   z-sub(h)  fptata        §TAMNT
045500020215     c                   else
045600091202     C                   z-add(h)  fptata        §TAMNT
045700020215     c                   end
045800020312    4C                   ENDIF
045900020213      *
046000020215      * Tariffa Giornata Intera
046100020213     C                   when      fptCTD = 'TIG'
046200020312    4C     §TAFGR        IFEQ      'I'
046300020312    4C     §TAtsr        andeq     'G'
046400020215     c     Piumen        ifeq      '-'
046500091202     C                   z-sub(h)  fptata        §TATIG
046600020215     c                   else
046700091202     C                   z-add(h)  fptata        §TATIG
046800020215     c                   end
046900020312    4C                   ENDIF
047000030527      *
047100020215      * Tariffa Mezza Giornata
047200020213     C                   when      fptCTD = 'TMG'
047300020312    4C     §TAFGR        IFEQ      'M'
047400020312    4C     §TAtsr        andeq     'G'
047500020215     c     Piumen        ifeq      '-'
047600091202     C                   z-sub(h)  fptata        §TATIG
047700020215     c                   else
047800091202     C                   z-add(h)  fptata        §TATIG
047900020215     c                   end
048000020312    4C                   ENDIF
048100020213      *
048200020215      * Tariffa Importo Addizionale
048300020213     C                   when      fptCTD = 'IAD'
048400030403      * A Prestazione se sono solo ritiri annullati deve non prendere
048500030403      *  la tariffa Addizionale.
048600030403     c     Solo_Rit_Ann  ifeq      'S'
048700030403     c                   goto      No_tar_IAD
048800030403     c                   endIF
048900020312    4C     §TAtsr        IFNE      'G'
049000020215     c     Piumen        ifeq      '-'
049100091202     C                   z-sub(h)  fptata        §TAIAD
049200091202     c                   sub(h)    fptata        vocice
049300020215     c                   else
049400091202     C                   z-add(h)  fptata        §TAIAD
049500091202     c                   add(h)    fptata        vocice
049600020215     c                   end
049700020312     c                   END
049800030403     c     No_tar_IAD    tag
049900020213      *
050000020213     c                   other
050100020215      * Tariffa Accessori
050200020311     c     §taita        ifne      *all'9'
050300020606     C* oppure accessori particolari come la maggiorazione dello stop oltre i 5 kg.
050400020604     c     fptctd        oreq      'X  '
050500090915     c     fptctd        oreq      'XT '
050600020320     C*
050700020320     C* inizializza il flag di calcolo accessori
050800020320     C                   move      'N'           esegui            1
050900020320      *
051000030527      *  Solo Recupero C/A
051100030527     c     Bolla_FW      ifeq      'S'
051200030527     c                   exsr      Recup_Cas
051300030527     c                   else
051400030527      *  Solo Incasso
051500020320     c     §tasic        ifeq      'S'
051600020320     c                   exsr      Solo_Incasso
051700020320     c                   else
051800030527      *  Accessori
051900020320     c                   exsr      Accessori
052000030821     c                   end
052100030527     c                   end
052200020322      *
052300020320      * se deve eseguire il calcolo dell'accessorio
052400020320     C     Esegui        ifeq      'S'
052500020322     C     UtiOK         andeq     'S'
052600020322     C                   EXSR      Calc_Access
052700020320     C                   End
052800020320      *
052900020311     c                   end
053000020213      *
053100020213     c                   endsl
053200020326      *
053300020326      *  Non deve aggiornare se in simulazione
053400030108      *   Prepara i dati per il FIFCE per comparazione al Conto Economico
053500020326     c                   if        §tasml=' '
053600030108     c                   Exsr      xContoEcon
053700020403     c                   end
053800020326      *
053900090914     c                   endIF
054000020327     c                   end
054100020213     C     kfpt02        reade     fifpt002
054200020213     c                   enddo
054300030110     C*    ----------------------------------------------
054400020426      *
054500020426      *  A fine ciclo occorre vedere se sono avvenute delle sostituzioni
054600020426      *   della tariffa base da parte di alcune tariffe accessorie e
054700020426      *    di conseguenza occorre riadattare i valori di conto economico
054800020426     C     Sost_ITT      ifeq      'S'
054900020426      *              *-------------------------*
055000020426     c                   exsr      Sost_FIFCE
055100020426      *              *-------------------------*
055200020426     c                   end
055300030110      *
055400030110     C*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
055500030108     C* Prima di uscire, controlla le Eccezioni:
055600030108      *
055700030108     C*   Se ci sono da valorizzare dei Ritiri Annullati.
055800030108      *    a tempo di totali a prestazione ovviamente in CONFERMA
055900030108     C                   If        §taTBS > 0 and §taTSR = 'R' and
056000030108     C                             §taCTR = 999 and §tapgm = 'CONFERM'
056100030110     C*
056200030110     C*            >>-->>-*-*-*-*-*-*-*-*-*-*-<<--<<
056300030108     c                   Exsr      Ritiri_Ann
056400030110     C*            >>-->>-*-*-*-*-*-*-*-*-*-*-<<--<<
056500030108     C*
056600030108     c                   EndIf
056700030110     C*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
056800030108     C*
056900020226     c                   if        §taitt = *all'9'
057000020226     c                   if        §tatpe > 0 or §tatst > 0
057100020226     c                   eval      §taitt = 0
057200020226     c                   end
057300020226     c                   end
057400020225      *
057500020213     C     fine          tag
057600020213     C                   RETURN
057700020215     C*================================================================
057800020215     C* VEDO SE MANCA TARIFFA                                         *
057900020215     C*================================================================
058000020215     C     Manca_TAR     BEGSR
058100020215     C*
058200020215     C* RICERCO TARIFFA A GIORNATA
058300020215     C                   Z-ADD     1             C
058400020215     C     'G'           LOOKUP    TSR(C)                                 30
058500020215     C  N30              MOVEL     999           W01CTR
058600020215     C   30              MOVEL     CTS(C)        W01CTR
058700020215     C                   MOVEL     'G'           W01TSR
058800020215     C*               *-------------------------*
058900020215     C                   EXSR      Ric_Tariffa
059000020215     C*               *-------------------------*
059100020215     C*
059200020215     C* VERRA' SEGNALATO "MANCA TARIFFA" SE:
059300020215     C*                   -------------
059400020215     C* 1) NON C'E TARIFFA A GIORNATA
059500020215     C* 2) C'E' TARIFFA A GIORNATA CON IL MINIMO ESPRESSO
059600020215     C* 3) C'E' TARIFFA A GIORNATA MA GLI IMPORTI TARIFFA INT/MEZZA
059700020215     C*    GIORNATA E MINIMO SONO VUOTI
059800020215     C*
059900020215     C     tar_valida    ifeq      'N'
060000020215     C                   MOVEL     'E'           §TAERR
060100020215     C                   else
060200020215     C*
060300020215     C* ricerca valori Tariffa a Giornata
060400020215     C                   EXSR      Tar_Giornata
060500020215     C*
060600020215     C     w01mnt        IFgt      0
060700020215     C     w01tmg        OReq      0
060800020215     C     w01tig        ANDeq     0
060900020215     C                   eval      tar_valida = 'N'
061000020215     C                   MOVEL     'E'           §TAERR
061100020215     C                   endif
061200020215     C*
061300020215     C                   end
061400020215     C*
061500020215     C                   ENDSR
061600020213     C*================================================================
061700020215     C* RICERCO TARIFFA                                               *
061800020213     C*================================================================
061900020215     C     Tariffa_Princ BEGSR
062000020215      *
062100020215      * Prende la Tariffa valida impostando il progressivo
062200020215      * e se la tariffa non è valida SEGNALA MANCA TARIFFA e va a fine pgm
062300020215     C                   movel     §taPDR        W01PDR
062400020215     C                   movel     §taCTR        W01CTR
062500020215     C                   movel     §taTSR        W01TSR
062600020215      *               *-------------------------*
062700020215     C                   exsr      Ric_Tariffa
062800020215      *               *-------------------------*
062900020319     C     Tar_valida    ifeq      'N'
063000020319     C     §tactr        andeq     999
063100030331      *
063200030331      * In eccezione x i ritiri annullati:
063300030331      *  Se non esiste la tariffa a prestazione e stiamo prendendo in esame
063400030331      *   dei ritiri in cui ci sono solo dei ritiri annullati, eseguo solo.
063500030331      *    la Routine per i ritiri annullati (per prenderne il valore) poi
063600030331      *     comunque esco come faceva prima il programma.
063700030331     C                   If        §taTBS > 0 and §taTSR = 'R' and
063800030331     C                             §taCTR = 999 and §tapgm = 'CONFERM'
063900030331     C*            >>-->>-*-*-*-*-*-*-*-*-*-*-<<--<<
064000030331     c                   Exsr      Ritiri_Ann
064100030331     C*            >>-->>-*-*-*-*-*-*-*-*-*-*-<<--<<
064200040506      *
064300040506      * Prima controlla che tutto sia OK dopo i ritiri annullati
064400040506     C     Tar_valida    ifeq      'N'
064500040510     C****               MOVEL     'E'           §TAERR
064600040506      * imposta A = Manca tariffa ritiri Annullati
064700040510     C*****              MOVEL     'A'           §TAter
064800040506     C                   ENDIF
064900040506      *
065000030331     c                   EndIf
065100030331      *  dopo esce
065200020319     c                   goto      fine
065300020319     C                   ENDIF
065400020319      *
065500020215     C     Tar_valida    ifeq      'N'
065600020215     C                   exsr      Manca_TAR
065700020215     C     §taerr        cabEQ     'E'           fine
065800020215     C                   ENDIF
065900020215      *
066000020215     C* SE C'E' RIFERIMENTO ALTRA TARIFFA USO QUESTO PER IL DETTAGLIO
066100020215    1C     FGTRAP        IFGT      0
066200020215     C                   Z-ADD     FGTRAP        W01PDR
066300020215     C                   Z-ADD     FGTRCT        W01CTR
066400020215      *               *-------------------------*
066500020215     C                   EXSR      Ric_Tariffa
066600020215      *               *-------------------------*
066700020215    2C     Tar_valida    ifeq      'N'
066800020215     C                   exsr      Manca_TAR
066900020215     C     §taerr        cabEQ     'E'           fine
067000020215    2c                   end
067100020215    1C                   ENDIF
067200020215     C*
067300020215     C                   ENDSR
067400020215     C*================================================================
067500020215     C* RICERCO PROGRESSIVO TARIFFA VALIDO                            *
067600020215     C*================================================================
067700020215     C     Ric_Tariffa   BEGSR
067800020215     C*
067900020213     C                   CLEAR                   WALF3
068000020213     C*
068100031024     C     riprova       tag
068200020215     C     KFGT          chain     fifgt000                           30
068300091112     C*
068400041213      ******* Era errato averlo:qui occorre spostarlo all'interno del ciclo
068500041213      *******  in questo punto veniva testata solo la prima tariffa a prescindere
068600041213      *******  dalle date di scadenza tariffa poi sporcava irreparabilmente
068700041213      *******  la chiave di ricerca tariffa
068800041213      * se ho tariffa a giornata riferita ad altro padroncino
068900041213    1C**N30FGTRAP        IFGT      0
069000041213     c*****§taTSR        andeq     *blank
069100041213     c*****§taCTR        andeq     *zeros
069200041213     c*****§taAAS        andeq     *zeros
069300041213     c*****§taLNP        andeq     *zeros
069400041213     c*****§taNRS        andeq     *zeros
069500041213     c*****§taNSP        andeq     *zeros
069600041213     C*******            Z-ADD     FGTRAP        W01PDR
069700041213     C*******            Z-ADD     FGTRCT        W01CTR
069800041213     c*******            goto      riprova
069900041213     c*******            end
070000041213     C*******
070100020213    1C     *in30         DOWeq     *off
070200020213     C*
070300020213     C* TARIFFA VALIDA:
070400020213     C*  Non deve essere annullata e deve avere la divisa richiesta
070500020213      **   Data Documento deve essere compresa fra le date di attivazione e
070600020213      ***   scadenza e deve essere stata stampata (quindi convalidata)
070700020213      ****   >altrimenti è un Manca Tariffa<
070800020213    2C     FGTATB        ifEQ      ' '
070900020213     c     §tadiv        andeq     fgtdiv
071000091112      *
071100091112     C* Se tariffa del periodo
071200091112     C     Ktgt          CHAIN     FItgt01l
071300091112 2/3 c                   if        %Found(FItgt01l) and
071400091112     c                             §TADDC >= tgtDDT and §TADDC <= tgtDST
071500091112     *****
071600091112     C**** §TADDC        IFge      FGTDDT
071700091112     C**** §TADDC        andLE     FGTDST
071800091112     *****
071900091112   3 c     §tasml        IFeq      *blank
072000091112     C     tgtDTS        aNDgt     0
072100091112     C**** fgtDTS        aNDgt     0
072200091112     c     §tasml        OReq      'S'
072300020417      *
072400041213      * Il controllo viene portato all'interno del ciclo per gestire
072500041213      * correttamente la tariffa di riferimento
072600041213      * se ho tariffa a giornata riferita ad altro padroncino
072700091112    4C  N30FGTRAP        IFGT      0
072800041213     c     §taTSR        andeq     *blank
072900041213     c     §taCTR        andeq     *zeros
073000041213     c     §taAAS        andeq     *zeros
073100041213     c     §taLNP        andeq     *zeros
073200041213     c     §taNRS        andeq     *zeros
073300041213     c     §taNSP        andeq     *zeros
073400041213     C                   Z-ADD     FGTRAP        W01PDR
073500041213     C                   Z-ADD     FGTRCT        W01CTR
073600041213     c                   goto      riprova
073700091112    4c                   end
073800041213      *
073900020213     C                   MOVEL     FGTPRG        WALF3             3
074000020213     C                   leave
074100091112      *
074200091112   3 C                   end
074300091112     C                   end
074400091112   2 C                   end
074500091112     C*
074600020213     C  N30KFGT          READE     fifgt000                               30
074700091112   1 C                   ENDDO
074800020213     C*
074900020215     C*  TARIFFA INESISTENTE O ANCORA DA DECORRERE - 90 ON -
075000020213     C     WALF3         ifeq      *BLANKS
075100020213     C                   SETON                                        90
075200020213     C                   else
075300020213     C                   MOVEL     WALF3         W01PRG
075400020213     C     KFGT2         CHAIN     fifgt000                           90
075500020213     C                   end
075600020213     C*
075700020213     C* se ha trovato la tariffa valida --> 90 Off
075800020213     C                   move      'N'           tar_valida
075900020213     C  n90              move      'S'           tar_valida
076000020213     C  n90              movel     fgtprg        W01PRG
076100020213     C*
076200090507     C*  se ha trovato la tariffa aggancia la Supertestata
076300090507     C*   per reperire nuove informazioni come la società e il Fornitore
076400091112     C                   clear                   §taSOC
076500091112     C                   clear                   §taCDF
076600090507     C                   if         tar_valida = 'S'
076700090507     C     Ktgt          CHAIN     fitgt01l
076800090507     c                   if        %Found(fitgt01l)
076900090507     C                   move      tgtSOC        §taSOC
077000090507     C                   move      tgtCDF        §taCDF
077100090507     C                   end
077200090507     C*
077300090507     C                   end
077400090507     C*
077500020213     C                   ENDSR
077600020215     C*================================================================
077700020215     C* ricerca valori Tariffa a Giornata                             *
077800020215     C*================================================================
077900020215     C     Tar_Giornata  BEGSR
078000020215     C*
078100020215     C*  per la tariffa a Giornata la tariffa è una "G/999"
078200020215     C                   Z-ADD     1             C
078300020215     C     'G'           LOOKUP    TSR(C)                                 30
078400020215     C  N30              MOVEL     999           W01CTR
078500020215     C   30              MOVEL     CTS(C)        W01CTR
078600020215     C                   MOVEL     'G'           W01TSR
078700020215     C*
078800020215     C* >>>>>>>>>>>> se non c'è in testata FPT il minimo garantito  "MGA"
078900020215     C*  <<<<<<<<<    se non c'è  "  "    "    la mezza giornata    "TMG"
079000020215     C*  <<<<<<<<<    se non c'è  "  "    "    la giornata intera   "TIG"
079100020215     C* >>>>>>>>>>>> tutti i valori si trovano sul campo FPTATA di testata.
079200020215     C*
079300020215     C* Minimo Garantito
079400020215     C                   z-add     0             w01mnt
079500020215     C                   movel     'MGA'         w01ctd
079600020215     C     KFPT00        chain     fifpt01L
079700020215     C                   if        %found(fifpt01L) and F01atb=*blank
079800020215     C                   z-add     F01ata        w01mnt
079900020215     c                   end
080000020215     C*----------
080100020215     C* Tariffa Mezza  Giornata
080200020215     C                   z-add     0             w01tmg
080300020215     C                   movel     'TMG'         w01ctd
080400020215     C     KFPT00        chain     fifpt01L
080500020215     C                   if        %found(fifpt01L) and F01atb=*blank
080600020215     C                   z-add     F01ata        w01tmg
080700020215     c                   end
080800020215     C*----------
080900020215     C* Tariffa Giornata Intera
081000020215     C                   z-add     0             w01tig
081100020215     C                   movel     'TIG'         w01ctd
081200020215     C     KFPT00        chain     fifpt01L
081300020215     C                   if        %found(fifpt01L) and F01atb=*blank
081400020215     C                   z-add     F01ata        w01tig
081500020215     c                   end
081600020215     C*----------
081700020215     C                   ENDSR
081800020213     C*================================================================
081900020213     C* Imposta la Tariffa Base                                       *
082000020213     C*================================================================
082100020213     C     Base          BEGSR
082200020213     C*
082300020213     C* SALVO APPLICAZIONE TARIFFA FINITA ora sulla testata particolare
082400020213     C*  della BAC/BAR in altri casi il campo contiene altri valori.
082500091202     C                   z-add(h)  fptata        w01ata
082600020213      *
082700020213      *  esce da questa routine impostando il valore, se trovato, "w01imp".
082800020213      *   prima di essere chiamata deve essere impostato il codice dettaglio
082900020213      *    su cui ricercare la tariffa e quindi l'importo w01imp.
083000020213      *
083100020213      *  Cerca quindi la tariffa di dettaglio con il giusto CAP e scaglione
083200020213     c                   eval      Se_Base ='S'
083300020213      *             *---------------------------*
083400020215     C                   exsr      Tar_Base
083500020213      *             *---------------------------*
083600020514     C*
083700020514     c                   if        §taitt = *all'9' and w01imp<>0
083800020514     c                   eval      §taitt = 0
083900020514     c                   end
084000020213     C*
084100020213      *  ----> quindi imposta il risultato ottenuto
084200020213     c     Piumen        ifeq      '-'
084300091202     c****               z-sub(h)  w01imp        §taitt
084400091202     c                   sub(h)    w01imp        §taitt
084500091202     c                   sub(h)    w01imp        vocice
084600020213    1C                   else
084700091202     c****               z-add(h)  w01imp        §taitt
084800091202     c                   add(h)    w01imp        §taitt
084900091202     c                   add(h)    w01imp        vocice
085000020213    1C                   End
085100020213     C*
085200020426     C* memorizza in schiera per eventuale storno a fronte di un accessorio
085300020426     C* in sostituzione alla tariffa base
085400020426     C                   exsr      Mem_xBase
085500020426     C*
085600020213     C                   ENDSR
085700020213     C*================================================================
085800020325     C* Imposta la Tariffa Base x espresso (la maggiorazione)         *
085900020213     C*================================================================
086000020325     C     Base_eca      BEGSR
086100020213      *
086200020325      * richiama la routine del calcolo come per gli accessori
086300020325     C*  Calcolo Valore
086400020325     c                   eval      Se_Access ='N'
086500020325     C*               *----------------------*
086600020325     c                   exsr      Calc_VAL
086700020325     C*               *----------------------*
086800020514     C*
086900020514     c                   if        §taitt = *all'9' and w01itr<>0
087000020514     c                   eval      §taitt = 0
087100020514     c                   end
087200020325     C*
087300020325      * poi eseguo come la base ma in aggiunta
087400020325     c     Piumen        ifeq      '-'
087500091202     c                   sub(h)    W01ITR        §taitt
087600091202     c                   sub(h)    w01itr        vocice
087700020325    1C                   else
087800091202     c                   add(h)    W01ITR        §taitt
087900091202     c                   add(h)    w01itr        vocice
088000020325    1C                   End
088100020426     C*
088200020426     C* memorizza in schiera per eventuale storno a fronte di un accessorio
088300020426     C* in sostituzione alla tariffa base
088400020426     C                   exsr      Mem_xBase
088500020325     C*
088600020325     C                   ENDSR
088700020325     C*================================================================
088800020325     C* Imposta la Tariffa Stop                                       *
088900020325     C*================================================================
089000020325     C     Stop          BEGSR
089100020325      *
089200020213      *  Cerca quindi la tariffa di dettaglio con il giusto CAP e scaglione
089300020213     C*        CALCOLO solo il Valore della riga
089400020213     c                   eval      Se_Base ='N'
089500020213      *             *---------------------------*
089600020215     C                   exsr      Tar_Base
089700020213      *             *---------------------------*
089800020213      *
089900020213     c                   clear                   w01tst
090000020213      *
090100020213     C     w01imp        IFGT      0
090200091202     C     §TASPP        MULT(h)   w01imp        w01TST
090300020213     C                   ENDIF
090400020213      *
090500020213      * Imposta il campo di ritorno
090600020213     c     Piumen        ifeq      '-'
090700020514     c****               z-sub     w01tst        §taTST
090800020514     c                   sub       w01tst        §taTST
090900020326     c                   sub       w01tst        vocice
091000020213    1C                   else
091100020514     c****               z-add     w01tst        §taTST
091200020514     c                   add       w01tst        §taTST
091300020326     c                   add       w01tst        vocice
091400020213    1C                   End
091500020426     C*
091600020426     C* memorizza in schiera per eventuale storno a fronte di un accessorio
091700020426     C* in sostituzione alla tariffa base che azzera anche lo STOP
091800020426     C                   exsr      Mem_xStop
091900020213      *
092000020213     C                   ENDSR
092100020213     C*================================================================
092200020325     C* Imposta la Tariffa Stop x espresso (la maggiorazione)         *
092300020325     C*================================================================
092400020325     C     Stop_est      BEGSR
092500020325      *
092600020325      * richiama la routine del calcolo come per gli accessori
092700020325     C*  Calcolo Valore
092800020325     c                   eval      Se_Access ='N'
092900020325     C*               *----------------------*
093000020325     c                   exsr      Calc_VAL
093100020325     C*               *----------------------*
093200020325      *
093300020325      * poi eseguo come lo stop ma in aggiunta
093400020325     c                   clear                   w01tst
093500020325      *
093600020325     C     w01itr        IFGT      0
093700091202     C     §TASPP        MULT(h)   w01itr        w01TST
093800020325     C                   ENDIF
093900020325      *
094000020325      * Imposta il campo di ritorno
094100020325     c     Piumen        ifeq      '-'
094200020325     c                   sub       w01tst        §taTST
094300020326     c                   sub       w01tst        vocice
094400020325    1C                   else
094500020325     c                   add       w01tst        §taTST
094600020326     c                   add       w01tst        vocice
094700020325    1C                   End
094800020426     C*
094900020426     C* memorizza in schiera per eventuale storno a fronte di un accessorio
095000020426     C* in sostituzione alla tariffa base che azzera anche lo STOP
095100020426     C                   exsr      Mem_xStop
095200020325      *
095300020325     C                   ENDSR
095400030109     C*================================================================
095500030109     C* Imposta la Tariffa Base x dest.disagiato (la maggiorazione)   *
095600030109     C*================================================================
095700030109     C     Base_Disag    BEGSR
095800030109      *
095900030109      * richiama la routine del calcolo come per gli accessori
096000030109     C*  Calcolo Valore
096100030109     c                   eval      Se_Access ='N'
096200030109     C*               *----------------------*
096300030109     c                   exsr      Calc_VAL
096400030109     C*               *----------------------*
096500030109     C*
096600030109     c                   if        §taitt = *all'9' and w01itr<>0
096700030109     c                   eval      §taitt = 0
096800030109     c                   end
096900030109     C*
097000030109      * poi eseguo come la base ma in aggiunta
097100030109     c     Piumen        ifeq      '-'
097200091202     c                   sub(h)    W01ITR        §taitt
097300091202     c                   sub(h)    w01itr        vocice
097400030109    1C                   else
097500091202     c                   add(h)    W01ITR        §taitt
097600091202     c                   add(h)    w01itr        vocice
097700030109    1C                   End
097800030109     C*
097900030109     C* memorizza in schiera per eventuale storno a fronte di un accessorio
098000030109     C* in sostituzione alla tariffa base
098100030109     C                   exsr      Mem_xBase
098200030109     C*
098300030109     C                   ENDSR
098400021223     C*================================================================
098500021223     C* Imposta la Tar. Stop x destinatari disagiati (la maggiorazione)
098600021223     C*================================================================
098700030109     C     Stop_disag    BEGSR
098800021223      *
098900021223      * richiama la routine del calcolo come per gli accessori
099000021223     C*  Calcolo Valore
099100021223     c                   eval      Se_Access ='N'
099200021223     C*               *----------------------*
099300021223     c                   exsr      Calc_VAL
099400021223     C*               *----------------------*
099500021223      *
099600021223      * poi eseguo come lo stop ma in aggiunta
099700021223     c                   clear                   w01tst
099800021223      *
099900021223     C     w01itr        IFGT      0
100000091202     C     §TASPP        MULT(h)   w01itr        w01TST
100100021223     C                   ENDIF
100200021223      *
100300021223      * Imposta il campo di ritorno
100400021223     c     Piumen        ifeq      '-'
100500021223     c                   sub       w01tst        §taTST
100600021223     c                   sub       w01tst        vocice
100700021223    1C                   else
100800021223     c                   add       w01tst        §taTST
100900021223     c                   add       w01tst        vocice
101000021223    1C                   End
101100021223     C*
101200021223     C* memorizza in schiera per eventuale storno a fronte di un accessorio
101300021223     C* in sostituzione alla tariffa base che azzera anche lo STOP
101400021223     C                   exsr      Mem_xStop
101500021223      *
101600021223     C                   ENDSR
101700020325     C*================================================================
101800020213     C* Imposta la Tariffa Picking/Etichettatura                      *
101900020213     C*================================================================
102000020213     C     Pck_Etic      BEGSR
102100020213      *
102200020213      *  Cerca quindi la tariffa di dettaglio con il giusto CAP e scaglione
102300020213     C*        CALCOLO solo il Valore della riga
102400020213     c                   eval      Se_Base ='N'
102500020213      *             *---------------------------*
102600020215     C                   exsr      Tar_Base
102700020213      *             *---------------------------*
102800020506     C********           clear                   w01tpe
102900020213      *
103000020213    1C     w01imp        IFGT      0
103100020213     C*
103200020213     C* FGTTAT = "G" --> A GIORNATA - IMPORTO FISSO
103300020213     C* FGTTAT = "S" --> A SERVIZIO - IMPORTO FISSO
103400020213    2C     Fpttpg        IFEQ      'F'
103500020506     C*
103600020506     C* se a giornata imposto in campi specifici da passare
103700020506     C*  alla conferma
103800020506     c     §tatsr        ifeq      'G'
103900020506     C*
104000021216     c                   if        fptCTD = 'ETI'
104100091202     C                   Z-ADD(h)  w01imp        w01tge
104200020506     c                   end
104300020506     C*
104400020506     c                   if        fptCTD = 'PCK'
104500091202     C                   Z-ADD(h)  w01imp        w01tgp
104600020506     c                   end
104700020506     C*
104800020506     c                   else
104900091202     C                   ADD(h)    w01imp        w01tpe
105000020506     c                   end
105100020506     C*
105200020213   X2C                   ELSE
105300020213     C*
105400020213     C* FGTTAT = "C" --> A COLLO
105500020213     C* Etichettatura se ritiro
105600020213     C     §TATSR        IFEQ      'R'
105700020213     C*
105800091202     C     §TACPE        MULT(h)   w01imp        w01tpe
105900020213     C*
106000020213     C                   ELSE
106100020213     C* Piking se consegna
106200020213     C* Solo sul totale colli delle pagate
106300020213     C     Fpttpp        IFEQ      *BLANKS
106400091202     C     §TANCL        MULT(h)   w01imp        w01tpe
106500020213     C                   ELSE
106600020213     C* Sul totale colli di tutte le spedizioni
106700020213     C     Fpttpp        IFEQ      '1'
106800091202     C     §TACPE        MULT(h)   w01imp        w01tpe
106900020213     C                   END
107000020213     C                   END
107100020213     C                   END
107200020213     C*
107300020213    2C                   ENDIF
107400020213    1C                   ENDIF
107500020213      *
107600020506      * Attenzione: se a giornata occorre dividere i valori
107700020506     c     §tatsr        ifeq      'G'
107800020506      *
107900020506     c                   if        fptCTD = 'PCK'
108000020506      * imposta il campo di ritorno
108100020506     c     Piumen        ifeq      '-'
108200020506     c                   z-sub     w01tgp        §tatgp
108300020506     c                   sub       w01tgp        vocice
108400020506    1C                   else
108500020506     c                   z-add     w01tgp        §tatgp
108600020506     c                   add       w01tgp        vocice
108700020506    1C                   End
108800020506     c                   add       §tatgp        §tatpe
108900020506    1C                   End
109000020506      *
109100021216     c                   if        fptCTD = 'ETI'
109200020506      * imposta il campo di ritorno
109300020506     c     Piumen        ifeq      '-'
109400020506     c                   z-sub     w01tge        §tatge
109500020506     c                   sub       w01tge        vocice
109600020506    1C                   else
109700020506     c                   z-add     w01tge        §tatge
109800020506     c                   add       w01tge        vocice
109900020506    1C                   End
110000020506     c                   add       §tatge        §tatpe
110100020506    1C                   End
110200020506      *
110300020506     c                   else
110400020213      * imposta il campo di ritorno
110500020213     c     Piumen        ifeq      '-'
110600020514     c****               z-sub     w01tpe        §tatpe
110700020514     c                   sub       w01tpe        §tatpe
110800020326     c                   sub       w01tpe        vocice
110900020213    1C                   else
111000020514     c****               z-add     w01tpe        §tatpe
111100020514     c                   add       w01tpe        §tatpe
111200020326     c                   add       w01tpe        vocice
111300020213    1C                   End
111400020506     c                   endif
111500020213      *
111600020213     C                   ENDSR
111700020213     C*================================================================
111800020213     C* Imposta la Tariffa Bonus                                      *
111900020213     C*================================================================
112000020213     C     Bonus         BEGSR
112100020213      *
112200020213      *   controlla se presente in testata il bonus
112300020213     C                   clear                   dsbonus
112400020213     C                   movel     fptflr        dsbonus
112500020213      *------
112600020213     C                   CLEAR                   WTBN
112700020213     C                   CLEAR                   W01TOT
112800020213     C                   CLEAR                   W01NEG
112900020213      *
113000020213     C* DETERMINO TOTALE SPEDIZIONI SU CUI CALCOLARE IL BONUS
113100020213     C* IN BASE AL TIPO PAGAMENTO BONUS
113200020213     C                   SELECT
113300020213      *
113400020213     C* Tipo pagamento = blank => solo sulle spediz. pagate
113500020321     C     §FPTTPB       WHENEQ    *BLANK
113600020213     C     §TASET        SUB       §TASNP        W01TOT
113700020213     C                   Z-ADD     §TASNP        W01NEG
113800020213      *
113900020213     C* Tipo pagamento = '1'   => solo sulle spediz. consegnate
114000020321     C     §FPTTPB       WHENEQ    '1'
114100020213     C     §TASET        SUB       §TASNT        W01TOT
114200020213     C                   Z-ADD     §TASNT        W01NEG
114300020213      *
114400020213     C* Tipo pagamento = '2'   => pagate e/o consegnate
114500020321     C     §FPTTPB       WHENEQ    '2'
114600020213     C     §TASET        SUB       §TASNE        W01TOT
114700020213     C                   Z-ADD     §TASNE        W01NEG
114800020213      *
114900020213     C                   ENDSL
115000020213     C*
115100020213     C* SU UN CERTO  N U M E R O   D I   S E R V I Z I
115200020321    2C     §FPTSAB       IFGT      0
115300020213     C*
115400020213     C* - XXX SPEDIZIONI DAL TOTALE
115500020321    3C     §FPTPMD       IFEQ      '-'
115600020321    4C     W01NEG        IFLE      §FPTSAB
115700020213     C                   ADD       1             WTBN              1 0
115800020213    4C                   ENDIF
115900020213     C*
116000020213   X3C                   ELSE
116100020213     C* + DI XXX SPEDIZIONI (XXX COMPRESO)
116200020321    4C     W01TOT        IFGE      §FPTSAB
116300020213     C                   ADD       1             WTBN              1 0
116400020213    4C                   ENDIF
116500020213    3C                   ENDIF
116600020213     C*
116700020213     C                   ELSE
116800020213     C* SE NON C'E' IL CONTROLLO E' SUPERATO
116900020213     C                   ADD       1             WTBN
117000020213    2C                   ENDIF
117100020213     C* %
117200020213     C* ALMENO  UNA CERTA  P E R C E N T U A L E
117300020321    2C     §FPTPAB       IFGT      0
117400020321     C     §TASET        MULT      §FPTPAB       W01RST
117500020213     C                   DIV(H)    100           W01RST
117600020213    3C     W01TOT        IFGE      W01RST
117700020213     C                   ADD       1             WTBN
117800020213    3C                   ENDIF
117900020213     C*
118000020213     C                   ELSE
118100020213     C* SE NON C'E' IL CONTROLLO E' SUPERATO
118200020213     C                   ADD       1             WTBN
118300020213    2C                   ENDIF
118400020213     C*-------------------------------
118500020213     C* SE ENTRAMBI I CONTROLLI SUPERATI ASSEGNO IL BONUS
118600020213    2C     WTBN          IFEQ      2
118700020213     C*
118800020213     c     Piumen        ifeq      '-'
118900091202     C                   Z-sub(h)  FptATA        §TATBN
119000091202     c                   sub(h)    fptata        vocice
119100020213     c                   else
119200091202     C                   Z-add(h)  FptATA        §TATBN
119300091202     c                   add(h)    fptata        vocice
119400020213     c                   end
119500020213     C*
119600020213    2C                   ENDIF
119700020213     C*-------------------------------
119800020213      *
119900020213     C                   ENDSR
120000020214     C*================================================================
120100030527     C*  x Solo Recupero C/A
120200020214     C*================================================================
120300030527     C     Recup_Cas     BegSR
120400030527     C*
120500030527     C     fptctd        ifeq      'FW '
120600030527     C                   move      'S'           esegui
120700030527     C                   END
120800030527      *
120900030527     C                   ENDSR
121000030527     C*================================================================
121100030527     C*  x Solo Incasso sugli  Accessori
121200030527     C*================================================================
121300030527     C     Solo_Incasso  begSR
121400030527     C*
121500020320     C* INCASSO C/ASSEGNI
121600020320     C     §TACAS        ifGT      0
121700020320     C     fptctd        andeq     'C  '
121800020320     C                   Z-ADD     §TACAS        WTACAS
121900020320     C                   EXSR      CALIMP
122000020320     C                   Z-ADD     YECIMO        W0100            13 3
122100020320     C                   move      'S'           esegui
122200020320     C                   END
122300020320      *
122400020320     C* INCASSO ASSEGNATI
122500020320     C     §TAIFP        IFGT      0
122600020320     C     fptctd        andeq     'B  '
122700020320     C                   Z-ADD     §TAIFP        W0100
122800020320     C                   move      'S'           esegui
122900020320     C                   END
123000020320      *
123100020320     C                   ENDSR
123200020320     C*================================================================
123300020320     C*  Imposta la Tariffa degli Accessori
123400020320     C*================================================================
123500020320     C     Accessori     begSR
123600020214     C*
123700020321     C     Giac_LAvv     Ifeq      'S'
123800020418     C     fptctd        andeq     §tafgc
123900020214     C                   move      'S'           esegui
124000020321     C                   end
124100020418      *
124200020214     C* INCASSO C/ASSEGNI
124300020214     C     §TACAS        ifGT      0
124400020214     C     fptctd        andeq     'C  '
124500020214     C                   Z-ADD     §TACAS        WTACAS
124600020214     C                   EXSR      CALIMP
124700020215     C                   Z-ADD     YECIMO        W0100            13 3
124800020214     C                   move      'S'           esegui
124900020214     C                   END
125000020214     C*
125100020214     C* C/A PRONTAMENTE RECUPERATI
125200020214     C     §TACAS        IFGT      0
125300020214     C     §TACA1        andgt     0
125400020214     C     fptctd        andeq     'N  '
125500020214     C                   Z-ADD     §TACA1        WTACAS
125600020214     C                   EXSR      CALIMP
125700020215     C                   Z-ADD     YECIMO        W0100
125800020214     C                   move      'S'           esegui
125900020214     C                   ENDIF
126000020214      *
126100020214     C* INCASSO ASSEGNATI
126200020214     C     §TAIFP        IFGT      0
126300020214     C     fptctd        andeq     'B  '
126400020214     C                   Z-ADD     §TAIFP        W0100
126500020214     C                   move      'S'           esegui
126600020214     C                   END
126700020214     C***
126800020214     C* E-ESPRESSO  (TARIFFA PARTICOLARE DEL TIPO SERVIZIO)
126900091009     C* H-H10:30    (TARIFFA PARTICOLARE DEL TIPO SERVIZIO)
127000020214     C                   Z-ADD     1             C
127100020214     C     §TATSP        LOOKUP    TSP(C)                                 32
127200020214     C     *IN32         IFEQ      *ON
127300020214     C     Giac_LAvv     andeq     'N'
127400020214     C                   clear                   codctd
127500020214     C                   MOVEL     TPT(C)        codctd
127600091009      *  Attenzione poichè nella 5E la H 10:30 è espressa con (h) minuscolo poichè
127700091009      *   le varie utilizzano anche lettere minuscole.
127800091009     c                   if        TPT(C) ='h'
127900091009     C                   MOVEL(p)  'H'           codctd
128000091009     c                   end
128100020214     C     fptctd        ifeq      codctd
128200091009      *
128300091009     C     fptctd        oreq      'E  '
128400091009     C     §taTSP        andeq     'H'
128500091009     C     Si_TarH1030   andeq     'NO'
128600091009      *
128700020214     C                   move      'S'           esegui
128800091009      *
128900020214     c                   end
129000020214     C                   ENDIF
129100020214     C***
129200020214     C* CONSEGNE PARTICOLARI
129300020214     C***
129400020214     C     §TATC1        IFNE      ' '
129500020214     C     §TATC1        ANDNE     'L'
129600020214     C     Giac_LAvv     andeq     'N'
129700020214     C                   clear                   codctd
129800020214     C                   MOVEL     §TATC1        codctd
129900020322      * se supermercati  i codici possono essere ("S  " o "S 2")
130000020214     C     fptctd        ifeq      codctd
130100020321     c     fptctd        oreq      'S 2'
130200020321     C     codctd        andeq     'S  '
130300020214     C                   move      'S'           esegui
130400020214     c                   end
130500020214     C                   ENDIF
130600020214     C*
130700020214     C     §TATC2        IFNE      ' '
130800020214     C     §TATC2        ANDNE     'L'
130900020214     C     Giac_LAvv     andeq     'N'
131000020214     C                   clear                   codctd
131100020214     C                   MOVEL     §TATC2        codctd
131200020322      * se supermercati  i codici possono essere ("S  " o "S 2")
131300020214     C     fptctd        ifeq      codctd
131400020321     c     fptctd        oreq      'S 2'
131500020321     C     codctd        andeq     'S  '
131600020214     C                   move      'S'           esegui
131700020214     c                   end
131800020214     C                   ENDIF
131900020214     C* ISOLA
132000020214     C     §TAFIN        IFEQ      'I'
132100020214     C     Giac_LAvv     andeq     'N'
132200020214     C                   clear                   codctd
132300020214     C                   MOVEL     §TAFIN        codctd
132400020214     C     fptctd        ifeq      codctd
132500020214     C                   move      'S'           esegui
132600020214     c                   end
132700020214     C                   ENDIF
132800020214     C***
132900020214     C* LOCALITA' DISAGIATE
133000020214     C     §TAFIN        IFEQ      'D'
133100020214     C     Giac_LAvv     andeq     'N'
133200020321     C**** fptctd        andeq     'K  '
133300020321     C     fptctd        andeq     'LOD'
133400020214     C                   move      'S'           esegui
133500020214     C                   ENDIF
133600020214     C***
133700020214     C* CENTRI STORICI
133800020214     C     §TAFIN        IFEQ      'S'
133900020214     C     Giac_LAvv     andeq     'N'
134000020214     C     fptctd        andeq     'Q  '
134100020214     C                   move      'S'           esegui
134200020214     C                   ENDIF
134300020214     C*** ----------------------------------------- ****
134400020214     C* RICONSEGNA GIACENZA       (NON ESISTE +)
134500020214     C***
134600020214     C*****§TAFGC        IFEQ      'S'
134700020214     C*****fptctd        andeq     'G  '
134800020214     C*******            move      'S'           esegui
134900020214     C*******            ENDIF
135000020214     C*
135100020214     C* COMPETENZE CONTO ECONOMICO  (SEMPRE APPLICATA)
135200020214     C     fptctd        ifeq      'O  '
135300020214     C                   move      'S'           esegui
135400020214     C                   ENDIF
135500020322      *
135600020214     C* COMPETENZE AGGIUNTIVE 2
135700020214     C     fptctd        ifeq      'Z  '
135800020214     C                   move      'S'           esegui
135900020214     C                   ENDIF
136000020214     C***
136100020214     C* COMPETENZE AGGIUNTIVE 3
136200090928     C*   tariffa di maggiorazione a stop di consegna over 5 Kg.
136300020214     C     fptctd        ifeq      'X  '
136400090915     C     fptctd        oreq      'XT '
136500090928      *
136600090928      * viene attivata solo se le spedizioni inerenti lo stop
136700090928      *  superano i 5 Kg.
136800090928     C     Peso_Sped     ifgt      5
136900020214     C                   move      'S'           esegui
137000090928     c                   end
137100090928      *
137200020214     C                   ENDIF
137300020214     C***
137400020214     C* PRIVATI (PARTICOLARITA' VARIE)
137500020214     C     §TAGVA        IFEQ      'P '
137600020214     C     Giac_LAvv     andeq     'N'
137700020214     C     fptctd        andeq     'V  '
137800020214     C                   move      'S'           esegui
137900020214     C                   END
138000020322      *
138100020322      * ---------------
138200020322      * Nuove Tariffe
138300020322      * ---------------
138400020322      * Tariffa Scarico di Ritiro
138500020322     C     fptctd        ifeq      'SCR'
138600021211     C     §tafcs        andeq     'S'
138700020322     C                   move      'S'           esegui
138800020322     C                   ENDIF
138900020322      *
139000020322      * Tariffa Carico di Consegna
139100020322     C     fptctd        ifeq      'CAC'
139200021211     C     §tafcs        andeq     'S'
139300020322     C                   move      'S'           esegui
139400020322     C                   ENDIF
139500020325      * ------------
139600020325      *
139700020214     C                   ENDSR
139800020213     C*================================================================
139900090928     C* CALCOLO il Valore da moltiplicare in base all'unità mis.di Pagamento
140000020213     C*================================================================
140100090928     C     MoltiplicatoreBEGSR
140200020215     C*
140300090928     C*  In questa routine, in base alla tipologia di Pagamento, si deve calcolare
140400090928     C*  la quantità (in unità di misura espressa dal tipo pagamento) che dovrà
140500090928     C*  essere moltiplicata x ogni tariffa da applicare all'autista.
140600090928     C*
140700020215     C*  prende dalla tab. "TR"  il > Minimo <
140800020215     C                   MOVEL     'TR'          COD
140900020215     C                   MOVEL     *BLANKS       KEY
141000020215     C                   MOVEL     fpttpg        KEY
141100020215     C     KTAB          CHAIN     TABEL                              30
141200020215     C   30              Z-ADD     1             §TRATA
141300020215     C  N30              MOVEL     TBLUNI        DSTR
141400090928     C*
141500090928     C*  In tutti i casi comunque si calcola in una variabile a parte il
141600090928     C*  Peso tassabile per ragionamenti puramente legati al peso.
141700090928     C*
141800090928     C* SE NO RAPPORTO O VOLUME A ZERO --> PESO TASSABILE=PESO REALE
141900090928    2C     fptRPV        IFeq      0
142000090928     C     §TATVL        OReq      0
142100090928     C                   Z-ADD     §TAKFA        Peso_Sped
142200090928   X2C                   ELSE
142300090928     C*
142400090928     C     §TATVL        MULT(H)   fptRPV        VolXrpv          13 3
142500090928     C     VolXrpv       MULT      100           VolXrpv
142600090928     C*
142700090928     C* PRENDO IL PIU' GRANDE TRA IL TASSABILE DA VOLUME E IL REALE
142800090928    3C     VolXrpv       IFLT      §TAKFA
142900090928     C                   Z-ADD     §TAKFA        Peso_Sped
143000090928     C                   ELSE
143100090928     C                   Z-ADD     VolXrpv       Peso_Sped
143200090928    3C                   ENDIF
143300090928    2C                   ENDIF
143400090928     C*
143500020215     C*
143600020215     C* CTR A KILO E Q.LE --> RAPPORTO PESO /VOL E ARROTONDAMENTI
143700020215     C* CTR A COLLO       -->                      ARROTONDAMENTI
143800020215     C* CTR A SPEDIZIONE  -->
143900020213     C**
144000020213     C* KILO O Q.LE  PER TARIFFE NORMALI
144100020213     C*    SOLO KILO PER TARIFFE PARTICOLARI
144200020213    1C                   SELECT
144300020213     C**
144400020215    1C     fpttpg        WHENeq    '0'
144500020215     C     fpttpg        OReq      '3'
144600020213     C*
144700020213     C* SE NO RAPPORTO O VOLUME A ZERO --> PESO TASSABILE=PESO REALE
144800020215    2C     fptRPV        IFeq      0
144900020215     C     §TATVL        OReq      0
145000020213     C                   Z-ADD     §TAKFA        W01PTA
145100020213   X2C                   ELSE
145200020213     C*
145300020215     C     §TATVL        MULT(H)   fptRPV        VolXrpv          13 3
145400020215     C     VolXrpv       MULT      100           VolXrpv
145500020213     C*
145600020213     C* PRENDO IL PIU' GRANDE TRA IL TASSABILE DA VOLUME E IL REALE
145700020215    3C     VolXrpv       IFLT      §TAKFA
145800020213     C                   Z-ADD     §TAKFA        W01PTA
145900020213     C                   ELSE
146000020215     C                   Z-ADD     VolXrpv       W01PTA
146100020213    3C                   ENDIF
146200020213    2C                   ENDIF
146300020213     C* COLLI
146400020215   x1C     fpttpg        WHENeq    '1'
146500020213     C                   Z-ADD     §TANCL        W01PTA
146600020213     C* SPEDIZIONI
146700020215   x1C     fpttpg        WHENeq    '2'
146800020213     C                   Z-ADD     §TASET        W01PTA
146900020213     C* Bonus
147000020215     C* Fisso (Valore Fisso)
147100020215     C* Valore (% su Valore) x le TARIFFE PARTICOLARI
147200020215     C*   imposta un valore fittizio per seguenti calcoli
147300020215   x1C     fpttpg        WHENeq    'B'
147400020215   x1C     fpttpg        OReq      'F'
147500020215   x1C     fpttpg        OReq      'V'
147600020215     c                   z-add     1             W01PTA
147700020213      *
147800020213     C                   OTHER
147900020213     C**
148000020213    1C                   ENDSL
148100020215     C* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
148200020215     C* Quindi arrotonda il peso Tassabile
148300020215     C*   SOLO SE previsto arrotondamento è sulla tab.>TR<
148400020215     C*
148500020215     C* e APPLICO arrotondamento SOLTANTO SE il Valore  è >=  al MINIMO
148600020215    1C     W01PTA        IFGE      §TRATA
148700020215     C     §TRarr        andEQ     'S'
148800020215     C*                          =======
148900020215     C* ARROTONDAMENTO OLTRE
149000020215    3C     W01PTA        IFGT      fptARL
149100020215     C                   Z-ADD     fptARO        W01ARR
149200020215   X3C                   ELSE
149300020215     C* ARROTONDAMENTO FINO A
149400020215     C                   Z-ADD     fptARF        W01ARR
149500020215    3C                   ENDIF
149600020215     C*
149700020215     C* APPLICO ARROTONDAMENTO SE C'E' E SE DIVIDENDO MI DA UN RESTO
149800020215    3C     W01ARR        IFGT      0
149900020215     C     W01PTA        DIV       W01ARR        W01RIS            7 0
150000020215     C                   MVR                     W01RST           10 3
150100020215    4C     W01RST        IFGT      0
150200020215     C     W01RIS        MULT      W01ARR        W01PTA
150300020215     C                   ADD       W01ARR        W01PTA
150400020215    4C                   ENDIF
150500020215    3C                   ENDIF
150600020215     C*
150700020215    1C                   ENDIF
150800020215     C* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
150900020213     C*
151000020213     C                   ENDSR
151100020213     C* ==============================================================
151200020213     C* Si posiziona sulla Tariffa con il CAP                        *
151300020213     C* ==============================================================
151400020215     C     Tar_Base      BEGSR
151500020213      *
151600020213     C* Inizializza campo dell'importo Tariffa
151700020213     C                   clear                   w01imp           18 3
151800091202     C********           clear                   w01imp           18 5    x prossimo step
151900020213     C*
152000020214     C*  Importo Fisso in testata
152100020214     C                   If        FptTPG = 'F' or  FptTPG = 'B'
152200020214     C*
152300091202     c                   z-add(h)  FptATA        w01imp
152400020214     C*
152500020214     c                   else
152600020214     C*
152700020322     C*  Cerca la Tariffa sugli scaglioni
152800020322     C*               *----------------------*
152900020322     c                   exsr      Dett_Tar
153000020322     C*               *----------------------*
153100020213     C*
153200020214    1C                   End
153300020322     C*
153400020322     C                   ENDSR
153500020213     C*===============================================================
153600020322     C* Dettaglio Tariffa  a scaglioni                               *
153700020213     C*===============================================================
153800020322     C     Dett_Tar      BEGSR
153900020213     C*
154000020322     C*  Importo su Dettaglio tariffario
154100020322     C*  Cerca lo scaglione
154200020322     c                   eval      Se_Access ='N'
154300020322     C*               *----------------------*
154400020322     c                   exsr      Riga_FPD
154500020322     C*               *----------------------*
154600020322     C*
154700020322      * -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
154800020322      * Ma.....Attenzione: se non si è trovata la tariffa e
154900020322     C*    solo se si sta cercando la TARIFFA BASE ......
155000020322     C* questa routine serve anche per cercare i valori del
155100020322     C* pick/etic. e di stop e per questi non deve dare il
155200020322     C* manca tariffa poichè non è obbligatoria.
155300020322      * -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
155400020322     c     Trovata_tar   ifeq      'N'
155500020322      *
155600020322      *  deve dare il manca tariffa solo se è la base
155700020322    1C     Se_Base       ifEQ      'S'
155800020322     C                   EXSR      Manca_TAR
155900020322    1C                   End
156000020322      *
156100020322      *  comunque salta tutto
156200020322    1C                   Goto      EndTariffa
156300020322    1C                   End
156400020322     C*
156500020322      * -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
156600020322     C*
156700020322    1C     Se_Base       ifEQ      'N'
156800020322     C*               -----------
156900020322     C* Valore preso dallo Scaglione
157000091202     C                   Z-ADD(h)  FpDITR        w01imp
157100020322     C*               -----------
157200020322    1C                   else
157300020322     C*               -----------
157400020322     C* PESO TASSABILE < = AL  MINIMO TASSABILE
157500020322    2C     W01PTA        IFLE      W01ATA
157600020322     C*
157700020322     C* Valore preso dallo Scaglione
157800091202     C                   Z-ADD(h)  FpDITR        w01imp
157900020322     C*
158000020322   X2C                   ELSE
158100020322     C*
158200020322     C* SE TARIFFA A Q.LE DIVIDO PER 100
158300020322     C     fpttpg        IFEQ      '0'
158400020322     C                   DIV(H)    100           W01PTA
158500020322     C                   ENDIF
158600020322     C*
158700020322     C* SCAGLIONE > DEL MINIMO TASSABILE
158800020322     C*  MOLTIPLICO TARIFFA PER IL PESO TASSABILE
158900020322     C*  PRENDO L'IMPORTO  >  TRA QUESTO E IL MINIMO
159000020322     C     W01PTA        MULT(H)   FpDITR        w01imp
159100020322     C*
159200020322    2C                   ENDIF
159300020322     C*               -----------
159400020322    1C                   End
159500020322     C*
159600020322     C* sempre esegue il test del minimo e del massimo
159700020322     C* PRENDO L'IMPORTO  >  TRA LA TARIFFA E IL MINIMO
159800020322    3C     w01imp        IFLT      FpDMIN
159900020322     C                   Z-ADD(H)  FpDMIN        w01imp
160000020322    3C                   ENDIF
160100020322     C*
160200020322     C* PRENDO L'IMPORTO MAX SE w01imp > DEL MAX (SE IMMESSO)
160300020322     C     FpDMAX        IFGT      0
160400020322     C     w01imp        ANDGT     FpDMAX
160500020322     C                   Z-ADD(H)  FpDMAX        w01imp
160600020322     C                   END
160700020322     C*
160800020322     C     EndTariffa    ENDSR
160900020322     C*===============================================================
161000020322     C* Prende la riga del FIFPD                                     *
161100020322     C*===============================================================
161200020322     C     Riga_FPD      BEGSR
161300020322     C*
161400020213     C* APPLICAZIONE TARIFFA FINITA: SE NON C'E' PRENDO IL MINIMO
161500020213     C*  DA TABELLA TR
161600020313     c                   if        Se_Access ='S'
161700020313     C                   Z-ADD     §TRATA        W01ATA
161800020313     c                   else
161900020213    1C     W01ATA        IFEQ      0
162000020213     C                   Z-ADD     §TRATA        W01ATA
162100020213    1C                   ENDIF
162200020313     c                   end
162300020213     C*
162400020213      * imposta ind.31 a ON come se non avesse trovato il record
162500020213      *  solo nella LEGFPD se trova il record lo spegne.
162600020213     C                   SETON                                        31
162700020606     C** e inizializza il flag
162800020606     c                   move      'N'           Trovata_tar
162900020213     C**
163000020213     C* CONTROLLO SE IL DETTAGLIO CONTIENE RIGHE SENZA CAP
163100020213     C                   CLEAR                   W01CAP
163200020213     C     KFPD          SETLL     fifpd000                               30
163300020213    1C     *IN30         IFEQ      *ON
163400020213     C                   EXSR      LEGFPD
163500020213    1C                   ENDIF
163600020213     C**
163700020213     C* CAP INTERO DI 5 CIFRE
163800020213    1C     *IN30         IFEQ      *OFF
163900020606     C                   MOVEL     §TACAP        W01CAP
164000020606     C**
164100020606     C* Prova con il CAP intero
164200020606     C     KFPD          setll     fifpd000                               30
164300020606    1C     *IN30         ifeq      *ON
164400020606     C                   exsr      LEGFPD
164500020606    1C                   Else
164600020606     C**
164700020606     C                   MOVE      §TACAP        WNUM2             2 0
164800020213    2C     WNUM2         IFGT      0
164900020213     C     KFPD          SETLL     fifpd000                               30
165000020213    3C     *IN30         IFEQ      *ON
165100020213     C                   EXSR      LEGFPD
165200020213    3C                   ENDIF
165300020213    2C                   ENDIF
165400020606     c                   end
165500020606      *
165600020213    1C                   ENDIF
165700020409      *
165800020213     C* CAP CON ULTIME 2 CIFRE A "00"
165900020213    1C     *IN30         IFEQ      *OFF
166000020409     C     §TAFIN        IFNE      'C'
166100020213     C                   MOVE      '000'         WDSCAP
166200020213     C                   ELSE
166300020213     C                   MOVE      '100'         WDSCAP
166400020213     C                   ENDIF
166500020213     C     KFPD          SETLL     fifpd000                               30
166600020213    2C     *IN30         IFEQ      *ON
166700020213     C                   EXSR      LEGFPD
166800020213    2C                   ENDIF
166900020213    1C                   ENDIF
167000020213     C**
167100020213     C* CAP CON "IT" NELLE PRIME  2 CIFRE
167200020213    1C     *IN30         IFEQ      *OFF
167300020213     C                   MOVEL     'IT'          W01CAP
167400020213     C     KFPD          SETLL     fifpd000                               30
167500020213    2C     *IN30         IFEQ      *ON
167600020213     C                   EXSR      LEGFPD
167700020213    2C                   ENDIF
167800020213    1C                   ENDIF
167900020213     C*
168000020213     C                   ENDSR
168100020213     C*===============================================================
168200020213     C* LEGGO DETTAGLIO fifpd000                                     *
168300020213     C*===============================================================
168400020213     C     LEGFPD        BEGSR
168500020213     C*
168600020213     C* TROVATO ALMENO UNA RIGA DI DETTAGLIO CON IL CAP:
168700020213     C* (CAP SPECIFICO -O CAP CON 00 -O CAP IT)
168800020213     C* SE NON TROVO LO SCAGLIONE ALL'INTERNO
168900020213     C* E' TARIFFA A 0 SENZA CERCARE NEL CAP PIU' GENERICO SUCCESSIVO
169000020213     C*
169100020213     C* SE WNUM1 = ' ' SIGNIFICA CHE CI SONO SOLO RECORD ANNULLATI
169200020213     C*                DEVO CERCARE NUOVAMENTE
169300020213     C*
169400020213     C                   CLEAR                   WNUM1
169500020213     C     KFPD          READE     fifpd000                               31
169600020213     C*
169700020213     C* SE ANNULLATO COME SE AVESSE SCAGLIONE A ZERO
169800020213     C     FPDATB        IFNE      ' '
169900020213     C                   Z-ADD     0             FPDSGL
170000020213     C                   ELSE
170100020213     C                   MOVEL     1             WNUM1             1 0
170200020213     C                   ENDIF
170300020213     C*
170400030603     C* A causa di un vecchio modo di inserire gli scaglioni delle tariffe
170500030603     C*  per indicare l'inifinito si usava il 9999 o 99999 o 999999
170600030603     C*   potendo, in tal modo, non soddisfare il test successivo per una
170700030603     C*    quantità elevata soprattutto per quanto riguarda SDI.
170800030603     C* Per eliminare il problema: reimposto il campo.
170900030603     c     Fpdsgl        ifeq      9999
171000030603     c     Fpdsgl        oreq      99999
171100030603     c     Fpdsgl        oreq      999999
171200030603     c                   move      *all'9'       Fpdsgl
171300030603     c                   end
171400030603     C*
171500020213     C     *IN31         DOWEQ     *OFF
171600020213     C     FPDSGL        ANDLT     W01PTA
171700020213     C*
171800020213     C     KFPD          READE     fifpd000                               31
171900020213     C  N31FPDATB        IFNE      ' '
172000020213     C                   Z-ADD     0             FPDSGL
172100020213     C                   ELSE
172200020213     C                   MOVEL     1             WNUM1
172300020213     C                   ENDIF
172400020213     C                   ENDDO
172500020213     C*
172600020213     C* 31 ON - NON TROVATO SCAGLIONE
172700020322     c                   move      'S'           Trovata_tar       1
172800020322     c   31              move      'N'           Trovata_tar
172900020213     C*
173000020213     C* CERCO NUOVAMENTE
173100020213     C     WNUM1         IFEQ      0
173200020213     C                   SETOFF                                       30
173300020213     C                   ENDIF
173400020213     C*
173500020213     C                   ENDSR
173600020214     C*================================================================
173700020214     C* CALCOLO IMPORTO CONTRASSEGNO IN BASE AL CAMBIO                *
173800020214     C*================================================================
173900020214     C     CALIMP        BEGSR
174000020214     C*---------
174100020214     C* UTILIZZA LA YEURCO PER CONVERITRE L'IMPORTO
174200020214     C                   CLEAR                   YEURDS
174300020214     C                   MOVEL     §TAVCA        YECDVI
174400020214     C                   Z-ADD     WTACAS        YECIMI
174500020214     C                   MOVEL     §GEDCN        YECDVO
174600020214     C                   MOVE      '3'           YECDCO
174700020214     C                   MOVEL     'H'           YECTAR
174800020214     C                   CALL      'YEURCO'
174900020214     C                   PARM                    YEURDS
175000020215     C*
175100020214     C                   ENDSR
175200020214     C*===============================================================
175300020325     C* CALCOLO il valore dal dettaglio TARIFFE PARTICOLARI          *
175400020214     C*===============================================================
175500020325     C     Calc_VAL      BEGSR
175600020325     C*
175700020325     C                   CLEAR                   W01ITR
175800020325     C*
175900020325     C*  Importo Fisso in testata
176000020325     C                   If        FptTPG = 'F' or  FptTPG = 'B'
176100020325     C*
176200091202     c                   z-add(h)  FptATA        w01itr
176300020325     C*
176400020325     c                   else
176500020325     C*
176600020325     C*  Cerca lo scaglione
176700020325     C*               *----------------------*
176800020325     c                   exsr      Riga_FPD
176900020325     C*               *----------------------*
177000020325     C*
177100020325     C* TROVATA RIGA DI DETTAGLIO
177200020325     c     Trovata_tar   Ifeq      'S'
177300020325     C*
177400020325     C* SE LA TARIFFA NON E' A VALORE E
177500020325     C* SCAGLIONE < = AL  MINIMO TASSABILE
177600020325    2C     fpttpg        IFNE      'V'
177700020325     C     W01PTA        ANDLE     W01ATA
177800020325     C*
177900091202     C                   Z-ADD(h)  FPDITR        W01ITR           18 3
178000091202     C*********          Z-ADD     FPDITR        W01ITR           18 5    x prossimo step
178100020325     C*
178200020325   X2C                   ELSE
178300020325     C*
178400020325     C* SCAGLIONE > DEL MINIMO TASSABILE
178500020325     C*  MOLTIPLICO TARIFFA PER IL PESO TASSABILE
178600020325     C*  PRENDO L'IMPORTO  >  TRA QUESTO E IL MINIMO
178700041108     C*****fpttpg        IFne      'V'
178800020325     C     W01PTA        MULT(H)   FPDITR        W01ITR
178900041108     c************       endif
179000020325      *
179100020325     C* SE TARIFFA A VALORE DIVIDO PER 100 PERCHE' E' UNA PERCENTUALE
179200020325     C     fpttpg        IFEQ      'V'
179300020325     C     W01itr        MULT(H)   w0100         W01ITR
179400020325     C                   DIV(H)    100           W01ITR
179500020325     C                   ENDIF
179600020325      *
179700020325    2C                   ENDIF
179800020325     C*
179900020325     C* PRENDO L'IMPORTO  >  TRA LA TARIFFA E IL MINIMO(SE C'E')
180000020325     C     w01ITR        IFLT      FPDMIN
180100091202     C                   Z-ADD(H)  FPDMIN        W01ITR
180200020325    3C                   ENDIF
180300020325     C*
180400020325     C* SE L'IMPORTO SUPERA IL MAX PRENDO IL MAX (SE IMMESSO)
180500020325    2C     FPDMAX        IFGT      0
180600020325     C     W01ITR        ANDGT     FPDMAX
180700020325     C                   Z-ADD(H)  FPDMAX        W01ITR
180800020325    2C                   ENDIF
180900020325     C*
181000020325    1C                   ENDIF
181100020325     C*
181200020325    1C                   End
181300020325     C*
181400020325     C                   ENDSR
181500020325     C*===============================================================
181600020325     C* CALCOLO IMPORTO DA DETTAGLIO TARIFFE PARTICOLARI             *
181700020325     C*===============================================================
181800020325     C     Calc_Access   BEGSR
181900020325     C*
182000020325     C*  Calcolo Valore
182100020325     c                   eval      Se_Access ='S'
182200020325     C*               *----------------------*
182300020325     c                   exsr      Calc_VAL
182400020325     C*               *----------------------*
182500020325     C*
182600020322     C*.......................................................................
182700020514     C*  Se richiesta la SOSTITUZIONE della Tariffa Base
182800020514      *------------
182900020514     C**** FPDAIN        IFNE      'S'
183000020214     C*
183100020322     C*  L'importo di solito viene impostato nel suo cumulatore (§taITA)
183200020322     C*   oppure può essere dirottato sulla base (§taITT) tutto è pilotato
183300020322     C*    dalla tabella 8A tramite il flag "TBA" che può essere blk o "B"-> Base.
183400020426      *------------
183500020514    1c                   if        flbase <>'B'
183600020322      * se accessori
183700020214     c     Piumen        ifeq      '-'
183800020214     C                   sub(H)    w01itr        §taita
183900091202     c                   sub(h)    w01itr        vocice
184000020426     C                   else
184100020214     C                   ADD(H)    W01ITR        §TAITA
184200091202     c                   add(h)    w01itr        vocice
184300020426     C                   End
184400020514     C
184500020514    1C                   else
184600020322     C* se Base
184700020322     C*    vecchio retaggio del programma chiamante *all'9'
184800020322     C*     poichè devo eseguire una somma algebrica, azzero il campo.
184900020514     c                   if        §taitt = *all'9'
185000020514     c                   eval      §taitt = 0
185100020514     c                   end
185200020514     C*
185300020514     c     Piumen        ifeq      '-'
185400020514     C                   sub(H)    w01itr        §taitt
185500091202     c                   sub(h)    w01itr        vocice
185600020514     C                   else
185700020514     C                   ADD(H)    W01ITR        §TAITT
185800091202     c                   add(h)    w01itr        vocice
185900020514     C                   End
186000020514     C*
186100020514    1C                   End
186200020514     C***********
186300020514     C***********        ELSE
186400020514     C***********
186500020514     C***se*invece richiesta la sostituzione della Base
186600020514     c*****Piumen        ifeq      '-'
186700020514     C***********        sub(H)    w01itr        w01sia
186800091202     c***********        sub(h)    w01itr        vocice
186900020514    1C***********        else
187000020514     C***********        ADD(H)    W01ITR        w01sia
187100091202     c***********        add(h)    w01itr        vocice
187200020514    1C***********        End
187300020514     C***********
187400020514     c*****§TAPAG        IFEQ      'S'
187500020514     C***********        Z-ADD(H)  W01SIA        §TAITT
187600020514     C***********        move      'S'           Sost_ITT
187700020514     C***********
187800020514     C**PER*SOSTITUZIONE DA TAR. LASCIATO AVVISO/GIACENZA AZZERO ANCHE IMP.STOP CONSEGNA
187900020514     C*****Giac_LAvv     ifeq      'S'
188000020514     C***********        Z-ADD     *ZEROS        §TATST
188100020514     C***********        move      'S'           Sost_TST
188200020514    1C***********        End
188300020514     C***********
188400020514     C***********        ENDIF
188500020514     C***********
188600020514     C***********        ENDIF
188700020214     C*
188800020214     C                   ENDSR
188900020215     C*================================================================
189000020215     C*  Somma algebrica pilotata da Tab.8A
189100020215     C*================================================================
189200020322     C     Tab_8A        begSR
189300020215     C*
189400020215     C* Comunque viene sempre impostata la somma in positivo
189500020215     c                   move      '+'           Piumen            1
189600020322     c                   move      *blank        UtiOK             1
189700020322     c                   move      *blank        FlBase            1
189800020326     C*  Allo stesso momento azzera l'importo del C/Econ. in testa a tutto
189900020403     c                   move      *blank        VocFce            1
190000020403     c                   move      *blank        VocFc1            1
190100020403     c                   move      *blank        VocFc2            1
190200020403     c                   move      *blank        VocCce
190300020327     c                   move      *blank        VocCc1
190400020327     c                   move      *blank        VocCc2
190500020327     c                   move      *blank        Vocusc            1
190600020327     c                   move      *blank        Vocusr            1
190700020326     c                   move      *zeros        VocICE
190800020215     C*
190900020215     C                   Z-ADD     1             C
191000020215     C     fptCTD        LOOKUP    FTP(C)                                 33
191100020215     C     *IN33         IFEQ      *ON
191200020215     C*
191300020322     C* per il calcolo corretto degli accessori
191400020322     c                   movel     UTI(C)        UtiOK
191500020322     c                   movel     TBA(C)        FlBase
191600020326     C*  consegna
191700020326     C                   if        §tatsr = 'C'
191800020403     C                   if        CE1(C) <> *zeros and CE1(C) <> *blank
191900020326     C                   move      CE1(C)        VocCCE
192000020403     C                   move      fc1(C)        VocfCE
192100020326     c                   else
192200020326     C                   move      CE3(C)        VocCCE
192300020403     C                   move      fc3(C)        VocfCE
192400020326     c                   end
192500020327     c                   end
192600020326     C*  ritiri
192700020327     C                   if        §tatsr = 'R'
192800020403     C                   if        CE2(C) <> *zeros and CE2(C) <> *blank
192900020326     C                   move      CE2(C)        VocCCE
193000020403     C                   move      fc2(C)        VocfCE
193100020326     c                   else
193200020326     C                   move      CE4(C)        VocCCE
193300020403     C                   move      fc4(C)        VocfCE
193400020326     c                   end
193500020326     c                   end
193600020327     C*  giornata
193700020327     C                   if        §tatsr = 'G'
193800020327     C                   move      usc(C)        Vocusc
193900020327     C                   move      usr(C)        Vocusr
194000020327     C                   move      CE1(C)        VocCC1
194100020327     C                   move      CE2(C)        VocCC2
194200020403     C                   move      fc1(C)        Vocfc1
194300020403     C                   move      fc2(C)        Vocfc2
194400020327     c                   end
194500020327     C*
194600020215     C*  se l'importo dovrà essere negativo è indicato sulla tab.8A
194700020215     C     SPM(C)        ifeq      '-'
194800020215     c                   move      '-'           Piumen
194900020215     c                   end
195000020215     C*
195100020215    1C                   ENDIF
195200020215     C*
195300020215     C                   ENDSR
195400020215     C*****************************************************************
195500020215     C* *INZSR
195600020215     C*****************************************************************
195700020215     C     *INZSR        BEGSR
195800020215     C*
195900020215     C     KTAB          KLIST
196000020215     C                   KFLD                    CODUT             1 0
196100020215     C                   KFLD                    COD               2
196200020215     C                   KFLD                    KEY               8
196300020215     C*
196400020215     C     KTAB2         KLIST
196500020215     C                   KFLD                    CODUT
196600020215     C                   KFLD                    COD
196700020215     C*
196800020215     C     KTBE          KLIST
196900020215     C                   KFLD                    TBECOD            3
197000020215     C                   KFLD                    TBEKE1           15
197100020215      *
197200020215     C     KFGT          KLIST
197300020215     C                   KFLD                    W01PDR
197400020215     C                   KFLD                    §taSML
197500020215     C                   KFLD                    W01TSR
197600020215     C                   KFLD                    W01CTR
197700090507      *
197800090507     C     KTGT          KLIST
197900090507     C                   KFLD                    fgtPDR
198000090507     C                   KFLD                    fgtSML
198100090507     C                   KFLD                    fgtPRG
198200020215      *
198300020215     C     KFGT2         KLIST
198400020215     C                   KFLD                    W01PDR
198500020215     C                   KFLD                    §taSML
198600020215     C                   KFLD                    W01TSR
198700020215     C                   KFLD                    W01CTR
198800020215     C                   KFLD                    W01PRG
198900020215      *
199000020215     C     KFPT00        KLIST
199100020215     C                   KFLD                    W01PDR
199200020215     C                   KFLD                    §taSML
199300020215     C                   KFLD                    W01TSR
199400020215     C                   KFLD                    W01CTR
199500020215     C                   KFLD                    w01PRG
199600020215     C                   KFLD                    w01ctd
199700020215      * ---
199800020215     C     kfpt02        KLIST
199900020215     C                   KFLD                    k02PDR
200000020215     C                   KFLD                    k02SML
200100020215     C                   KFLD                    k02TSR
200200020215     C                   KFLD                    k02CTR
200300020215     C                   KFLD                    k02PRG
200400020215     C                   KFLD                    k02TSM
200500020215      *
200600020215     C     KFPD          KLIST
200700020215     C                   KFLD                    fptPDR
200800020215     C                   KFLD                    fptSML
200900020215     C                   KFLD                    fptTSR
201000020215     C                   KFLD                    fptCTR
201100020215     C                   KFLD                    fptPRG
201200020215     C                   KFLD                    fptctd
201300020215     C                   KFLD                    w01CAP
201400020215      *
201500020326     C     Kfce          KLIST
201600020326     C                   KFLD                    fceFGS
201700020326     C                   KFLD                    fcePDR
201800020326     C                   KFLD                    fceTSR
201900020326     C                   KFLD                    fceNDC
202000020326     C                   KFLD                    fceDDC
202100020326     C                   KFLD                    fceAAS
202200020326     C                   KFLD                    fceLNP
202300020326     C                   KFLD                    fceNRS
202400020326     C                   KFLD                    fceNSP
202500020326     C                   KFLD                    fceCCE
202600020327      *
202700020327     C     Kfcw          KLIST
202800020327     C                   KFLD                    fcwFGS
202900020327     C                   KFLD                    fcwPDR
203000020327     C                   KFLD                    fcwTSR
203100020327     C                   KFLD                    fcwNDC
203200020327     C                   KFLD                    fcwDDC
203300020327     C                   KFLD                    fcwAAS
203400020327     C                   KFLD                    fcwLNP
203500020327     C                   KFLD                    fcwNRS
203600020327     C                   KFLD                    fcwNSP
203700020327     C                   KFLD                    fcwCCE
203800020328      *
203900030320     C     Kbol          KLIST
204000030320     C                   KFLD                    ftwAAS
204100030320     C                   KFLD                    ftwLNP
204200030320     C                   KFLD                    ftwNRS
204300030320     C                   KFLD                    ftwNSP
204400030320      *
204500020328     C     Kftdw         KLIST
204600020328     C                   KFLD                    ftwFGS
204700020328     C                   KFLD                    ftwPDR
204800020328     C                   KFLD                    ftwTSR
204900020328     C                   KFLD                    ftwNDC
205000020328     C                   KFLD                    ftwDDC
205100020328     C                   KFLD                    ftwSTP
205200020523      *
205300030108     C     kfpt01        KLIST
205400030108     C                   KFLD                    k01PDR
205500030108     C                   KFLD                    k01SML
205600030108     C                   KFLD                    k01TSR
205700030108     C                   KFLD                    k01CTR
205800030108     C                   KFLD                    k01PRG
205900030108     C                   KFLD                    k01CTD
206000030108      *
206100020523     C*===============================================================
206200020523     C* Apertura Files                                               *
206300020523     C*===============================================================
206400020523     C* Chiamato dalla Conferma
206500020523     c                   if        §tapgm = 'CONFERM'
206600020523     c                             and  §tasml = ' '
206700020523     c                   open      FIFCEW1L
206800020523     c                   end
206900020326      *
207000020215     C*===============================================================
207100020215     C* CARICO TABELLE                                               *
207200020215     C*===============================================================
207300020215     C                   Z-ADD     1             CODUT
207400020326     C                   Z-ADD     0             C                 3 0
207500020215     C***
207600020215     C* CARICO TABELLA DEI TIPI SERVIZIO
207700020215     C***
207800020215     C                   Z-ADD     1             C
207900020215     C                   MOVEL     '1Z'          COD
208000020215     C     KTAB2         SETLL     TABEL
208100020215     C     KTAB2         READE     TABEL                                  30
208200020215     C*
208300020215     C     *IN30         DOWEQ     *OFF
208400020215     C     TBLFLG        IFEQ      ' '
208500020215     C                   MOVEL     TBLUNI        DS1Z
208600020215     C     §1ZUPD        IFEQ      'S'
208700020215     C                   MOVEL     TBLKEY        TSR(C)
208800020215     C                   MOVEL     §1ZCTR        CTS(C)
208900020215     C                   ADD       1             C
209000020215     C                   ENDIF
209100020215     C                   ENDIF
209200020215     C     KTAB2         READE     TABEL                                  30
209300020215     C                   ENDDO
209400020215     C***
209500020215     C* CARICO TABELLA DEI TIPI SERVIZIO CON TARIFFA PARTICOLARE
209600020215     C***
209700020215     C                   Z-ADD     1             C
209800020215     C                   MOVEL     '5E'          COD
209900020215     C     KTAB2         SETLL     TABEL
210000020215     C     KTAB2         READE     TABEL                                  30
210100020215     C*
210200020215     C     *IN30         DOWEQ     *OFF
210300020215     C     TBLFLG        IFEQ      ' '
210400020215     C                   MOVEL     TBLUNI        DS5E
210500020215     C     §5EFTC        IFNE      ' '
210600020215     C                   MOVEL     TBLKEY        TSP(C)
210700020215     C                   MOVEL     §5EFTC        TPT(C)
210800020215     C                   ADD       1             C
210900020215     C                   ENDIF
211000020215     C                   ENDIF
211100020215     C     KTAB2         READE     TABEL                                  30
211200020215     C                   ENDDO
211300020215     C***
211400020215     C* CARICO TARIFFE PARTICOLARI PADRONCINI
211500020215     C***
211600020215     C                   Z-ADD     1             C
211700020215     C                   MOVEL     '8A'          COD
211800020215     C     KTAB2         SETLL     TABEL
211900020215     C     KTAB2         READE     TABEL                                  30
212000020215     C*
212100020215     C     *IN30         DOWEQ     *OFF
212200020215     C     TBLFLG        IFEQ      ' '
212300020215     C                   MOVEL     TBLUNI        DS8A
212400020215     C                   MOVEL     TBLKEY        FTP(C)
212500020215     C                   MOVEL     §8AUSC        USC(C)
212600020215     C                   MOVEL     §8AUSR        USR(C)
212700020215     C                   MOVEL     §8ATBA        TBA(C)
212800020215     C                   MOVEL     §8ASPM        SPM(C)
212900020402     C                   MOVE      §8ACE1        CE1(C)
213000020402     C                   MOVE      §8ACE2        CE2(C)
213100020402     C                   MOVE      §8ACE3        CE3(C)
213200020402     C                   MOVE      §8ACE4        CE4(C)
213300020402     C                   MOVE      §8Afc1        fc1(C)
213400020402     C                   MOVE      §8Afc2        fc2(C)
213500020402     C                   MOVE      §8Afc3        fc3(C)
213600020402     C                   MOVE      §8Afc4        fc4(C)
213700020215     C                   ADD       1             C
213800020215     C                   ENDIF
213900020215     C     KTAB2         READE     TABEL                                  30
214000020215     C                   ENDDO
214100020215      *
214200020215     C* CARICO DATI TABELLA moneta CORRENTE/CONTO gestionale
214300020215     C                   MOVEL     'GED'         TBECOD
214400020215     C                   MOVEL     *BLANKS       TBEKE1
214500020215     C                   MOVEL     '1'           TBEKE1
214600020215     C     KTBE          CHAIN     TNTBE01L                           22
214700020215     C  N22              MOVEL     TBEUNI        DGED
214800020215     C*
214900090914     C* Carica tabelle Tariffe Particolari su Cliente
215000090914     C*  Memorizzandosi, una volta per tutte, tutte le tariffe inserite
215100090914      *----
215200090914     C                   Z-ADD     0             CP                5 0
215300090914     C                   movel(P)  'CTP'         tbeCOD
215400090914     c                   clear                   KCliTrovato
215500090914     c                   clear                   KCliPar
215600090914     c                   clear                   KCliParTSR
215700090914     c                   clear                   KTarStdNew
215800090914     c                   clear                   KTarStd
215900090914     c                   clear                   KTarNew
216000090914     c                   clear                   KTarObb
216100090914     c                   clear                   KTarUgu
216200090914     c                   clear                   KSTDDaExcl
216300090915     c                   clear                   KTsrNewCli
216400090914      *----
216500090914     C     tbeCOD        setll     tntbe01l
216600090914     C     tbeCOD        reade     tntbe01l
216700090914      *
216800090914     c                   dow       not %eof(tntbe01l)
216900090914     C                   add       1             CP
217000090914     C                   MOVEL     TBeKE1        KCliPar(CP)
217100090914     C                   MOVEL     TBeKE1        KCliParTSR(CP)
217200090914     C                   MOVEL     TBeKE2        primoByteTSR      1
217300090914      * il tipo servizio alla destra del codice cliente
217400090914     C                   MOVE      primoByteTSR  KCliParTSR(CP)
217500090914     C                   MOVEL     TBeKE2        KTarStd(CP)
217600090914     C                   MOVEL     TBeKE2        KTarStdNew(CP)
217700090914     C                   movel     tbeuni        dCTP
217800090914     C                   MOVE      §CTPCTD       KTarStdNew(CP)
217900090914     C                   MOVE      §CTPCTD       KTarnew(CP)
218000090914     C                   MOVEL     primoByteTSR  KTarnew(CP)
218100090914     C                   MOVEL     §CTPOBB       KTarobb(CP)
218200090914     C                   MOVEL     §CTPUGU       KTarugu(CP)
218300090915     c                   movel     KTarnew(CP)   KTsrNewCli(CP)
218400090915     c                   move      KCliPar(CP)   KTsrNewCli(CP)
218500090914     C     tbeCOD        reade     tntbe01l
218600090914     c                   enddo
218700090914      *----
218800090914     C*
218900020215     C* DEFINIZIONE CAMPI
219000020215     C     *LIKE         DEFINE    FGTPRG        W01PRG
219100020215     C     *LIKE         DEFINE    FGTPRG        k02PRG
219200020215     C     *LIKE         DEFINE    FGTATA        W01ATA
219300020215     C     *LIKE         DEFINE    FGTARL        W01ARR
219400020215     C     *LIKE         DEFINE    FGTPDR        W01PDR
219500020215     C     *LIKE         DEFINE    FGTPDR        k02PDR
219600020215     C     *LIKE         DEFINE    FGTCTR        W01CTR
219700020215     C     *LIKE         DEFINE    FGTCTR        k02CTR
219800020215     C     *LIKE         DEFINE    FGTTSR        W01TSR
219900020215     C     *LIKE         DEFINE    FGTTSR        k02TSR
220000020215     C     *like         define    fptata        w01mnt
220100020215     C     *like         define    fptata        w01tmg
220200020215     C     *like         define    fptata        w01tig
220300020215     C     *like         define    fptctd        w01ctd
220400020215     C     *like         define    fptctd        codctd
220500020215     C     *LIKE         DEFINE    FPDSGL        W01PTA
220600090928     C     *LIKE         DEFINE    FPDSGL        Peso_Sped
220700020215     C     *LIKE         DEFINE    §TAITA        W01SIA
220800020215     C     *LIKE         DEFINE    §TASET        W01TOT
220900020215     C     *LIKE         DEFINE    §TASET        W01NEG
221000020215     C     *LIKE         DEFINE    §TACAS        WTACAS
221100020215     C     *LIKE         DEFINE    §tatpe        w01tpe
221200020506     C     *LIKE         DEFINE    §tatpe        w01tge
221300020506     C     *LIKE         DEFINE    §tatpe        w01tgp
221400020215     C     *LIKE         DEFINE    §tatst        w01tst
221500020215     C     *LIKE         DEFINE    fptsml        k02sml
221600020215     C     *LIKE         DEFINE    fpttsm        k02tsm
221700020326     C     *LIKE         DEFINE    fcecce        voccce
221800020327     C     *LIKE         DEFINE    fcecce        voccc1
221900020327     C     *LIKE         DEFINE    fcecce        voccc2
222000020326     C     *LIKE         DEFINE    fceice        vocice
222100020328     C     *LIKE         DEFINE    fceice        parice
222200020328     C     *LIKE         DEFINE    fceice        restoice
222300030108     C     *like         define    fptPDR        k01PDR
222400030108     C     *like         define    fptSML        k01SML
222500030108     C     *like         define    fptTSR        k01TSR
222600030108     C     *like         define    fptCTR        k01CTR
222700030108     C     *like         define    fptPRG        k01PRG
222800030108     C     *like         define    fptCTD        k01CTD
222900020215      *
223000020215     C                   ENDSR
223100020326     C*================================================================
223200020426     C* memorizza sulle schiere gli importi da attribuire alla Base   *
223300020326     C*================================================================
223400020426     C     Mem_xBase     BEGSR
223500020426      *
223600020426      * cerca se presente altrimenti cerca il primo libero
223700020426     c                   z-add     1             vc                3 0
223800020426     c     voccce        lookup    vitt(vc)                               91
223900020426     c  N91*blank        lookup    vitt(vc)                               91
224000020426      * imposta in schiera
224100020426     c   91              add       vocice        iitt(vc)
224200020426     c   91              move      voccce        vitt(vc)
224300020426      *
224400020426     C                   ENDSR
224500020426     C*================================================================
224600020426     C* memorizza sulle schiere gli importi da attribuire allo Stop   *
224700020426     C*================================================================
224800020426     C     Mem_xStop     BEGSR
224900020426      *
225000020426      * cerca se presente altrimenti cerca il primo libero
225100020426     c                   z-add     1             vc                3 0
225200020426     c     voccce        lookup    vtst(vc)                               91
225300020426     c  N91*blank        lookup    vtst(vc)                               91
225400020426      * imposta in schiera
225500020426     c   91              add       vocice        itst(vc)
225600020426     c   91              move      voccce        vtst(vc)
225700020426      *
225800020426     C                   ENDSR
225900020426     C*================================================================
226000020426     C* Deve riadattare gli importi precedentemente scritti relativi  *
226100020426     C*  alle voci di C/E della Base della Tariffa                    *
226200020426     C*================================================================
226300020426     C     Sost_FIFCE    BEGSR
226400020426     C*
226500020426     C*  Se ci sono dei valori della base da sostituire
226600020426     c                   do        99            vc
226700020426     C*
226800020426     c                   if        vitt(vc) = *blank
226900020426     c                   leave
227000020426     c                   end
227100020426     C*
227200020426     C* Chiamato dalla valorizzazione
227300020426     c                   if        §tapgm = 'CALCVAL'
227400020426      *
227500020426     c                   move      vitt(vc)      sost_voc          3
227600020426     c                   z-add     iitt(vc)      sost_imp         10 3
227700020426      *
227800020426     c                   exsr      AGG_Sost
227900020426      *
228000020426     C* Chiamato dalla conferma
228100020426     c                   else
228200020426     c                   end
228300020426      *
228400020426     c                   if        vc = 99
228500020426     c                   leave
228600020426     c                   end
228700020426     c                   enddo
228800020426      * -------------
228900020426      *  x lo Stop
229000020426      *    Se è stato azzerato anche lo STOP
229100020426     C     Sost_TST      ifeq      'S'
229200020426      *
229300020426     c                   do        99            vc
229400020426     c                   if        vtst(vc) = *blank
229500020426     c                   leave
229600020426     c                   end
229700020426     C*
229800020426     C* Chiamato dalla valorizzazione
229900020426     c                   if        §tapgm = 'CALCVAL'
230000020426      *
230100020426     c                   move      vtst(vc)      sost_voc          3
230200020426     c                   z-add     itst(vc)      sost_imp         10 3
230300020426      *
230400020426     c                   exsr      AGG_Sost
230500020426      *
230600020426     C* Chiamato dalla conferma
230700020426     c                   else
230800020426     c                   end
230900020426      *
231000020426     c                   if        vc = 99
231100020426     c                   leave
231200020426     c                   end
231300020426     c                   enddo
231400020426      *
231500020426     c                   end
231600020426      * -------------
231700020426     C                   ENDSR
231800020426     C*================================================================
231900020426     C* Aggiorna File per il Conto Economico in sostituzione          *
232000020426     C*================================================================
232100020426     C     AGG_Sost      BEGSR
232200020426      *
232300020426     C                   z-add     §tafgs        fceFGS
232400020426     c                   if        fceFGS=0
232500020426     C                   movel     §tapdr        fceFGS
232600020426     c                   end
232700020426     C                   z-add     §tapdr        fcePDR
232800020426     C                   move      §tatsr        fceTSR
232900020426     C                   z-add     §tandc        fceNDC
233000020426     C                   z-add     §taddc        fceDDC
233100020426     C                   z-add     §taaas        fceAAS
233200020426     C                   z-add     §talnp        fceLNP
233300020426     C                   z-add     §tanrs        fceNRS
233400030320     C                   move      §tacbo        fcecbo
233500020426     c                   if        §tatsr='R' and
233600020426     c                             §tansp=§taksc
233700020426     C                   move      *zero         fceNSP
233800020426     c                   else
233900020426     C                   z-add     §tansp        fceNSP
234000020426     c                   end
234100020426     C*
234200020426     c                   move      sost_voc      fcecce
234300020426     C*
234400020426     C     Kfce          chain     fifce01l
234500020426     c                   if        %found(fifce01l)
234600020426     C*  sottrae il valore sostituito dall'accessorio
234700020426     C                   sub       sost_Imp      fceice
234800020426     C*
234900020426     c                   if        fceice <> 0
235000020426     C                   update    fifce000
235100020426     c                   else
235200020426     C*  se il risultato è = 0 allora cancella il record come se
235300020426     C*   non fosse mai stato scritto.
235400020426     C                   delete    fifce000
235500020426     c                   end
235600020426     c                   end
235700030108      *
235800030108     C                   ENDSR
235900030108     C*================================================================
236000030108     C* Imposta i dati per il Conto Economico                         *
236100030108     C*================================================================
236200030108     C     xContoEcon    BEGSR
236300030108      *
236400030108     C* solo per conteggi reali e non simulati
236500030108     C* e solo se c'è un valore valido
236600030108     C*  sia in positivo che in negativo
236700030108     c     VOCice        ifnE      0
236800030108      *
236900030108      * oppure fare attenzione alla scrittura per il Picking
237000030108     c     VOCfce        oreq      'S'
237100030108     c     fptCTD        andne     'PCK'
237200030108     c     fptCTD        andne     'ETI'
237300030108     c     fptCTD        andne     'ETC'
237400030108      *
237500030108     c     VOCfce        oreq      'S'
237600030108     c     fptCTD        andeq     'PCK'
237700030108     c     §taPPE        andeq     'S'
237800030108     c     §tatsr        andne     'G'
237900030108      *
238000030108     c     VOCfce        oreq      'S'
238100030108     c     fptCTD        andeq     'PCK'
238200030108     c     §tapgp        andeq     'S'
238300030108     c     §tatsr        andeq     'G'
238400030108      *
238500030108     c     VOCfce        oreq      'S'
238600030108     c     fptCTD        andeq     'ETI'
238700030108     c     §taPPE        andeq     'S'
238800030108     c     §tatsr        andne     'G'
238900030108      *
239000030108     c     VOCfce        oreq      'S'
239100030108     c     fptCTD        andeq     'ETC'
239200030108     c     §taPPE        andeq     'S'
239300030108     c     §tatsr        andne     'G'
239400030108      *
239500030108     c     VOCfce        oreq      'S'
239600030108     c     fptCTD        andeq     'ETI'
239700030108     c     §tapge        andeq     'S'
239800030108     c     §tatsr        andeq     'G'
239900030108      *
240000030108      * deve scrivere le voci per il picking solo se richiesto
240100030108      * altrimenti nonostante la forzatura sulla 8A non deve scrivere
240200030108      *              *-------------------------*
240300030108     c                   exsr      AGG_FIFCE
240400030108      *              *-------------------------*
240500030108      *
240600030108     c                   end
240700020426      *
240800020426     C                   ENDSR
240900020426     C*================================================================
241000020426     C* Aggiorna File per il Conto Economico                          *
241100020426     C*================================================================
241200020426     C     AGG_FIFCE     BEGSR
241300020326     C*
241400020326     C* Chiamato dalla valorizzazione
241500020426     c                   if        §tapgm = 'CALCVAL'
241600020326     C*
241700020328     C* se il tipo somma è a stop
241800020328     C*  deve ripartire la voce di c/economico su tutte le bolle
241900020328     C*   appartenenti allo STOP.
242000020328     c     §tatsm        ifeq      'T'
242100020328     C*                *--------------------*
242200020328     c                   exsr      VOC_Stop
242300020328     C*                *--------------------*
242400020328     c                   else
242500020328     C*
242600020326     C                   z-add     §tafgs        fceFGS
242700020326     c                   if        fceFGS=0
242800020326     C                   movel     §tapdr        fceFGS
242900020326     c                   end
243000020326     C                   z-add     §tapdr        fcePDR
243100020326     C                   move      §tatsr        fceTSR
243200020326     C                   z-add     §tandc        fceNDC
243300020326     C                   z-add     §taddc        fceDDC
243400020326     C                   z-add     §taaas        fceAAS
243500020326     C                   z-add     §talnp        fceLNP
243600020326     C                   z-add     §tanrs        fceNRS
243700030320     C                   move      §tacbo        fcecbo
243800020326     c                   if        §tatsr='R' and
243900020326     c                             §tansp=§taksc
244000020326     C                   move      *zero         fceNSP
244100020326     c                   else
244200020326     C                   z-add     §tansp        fceNSP
244300020326     c                   end
244400020326     c                   move      voccce        fcecce
244500020326     C*
244600020326     C     Kfce          chain     fifce01l
244700020326     c                   if        %found(fifce01l)
244800020326     C*  somma algebrica del valore
244900020326     C                   add       vocice        fceice
245000020326     C                   update    fifce000
245100020326     C*
245200020326     c                   else
245300020326     C                   clear                   fifce000
245400020326     C                   z-add     §tafgs        fceFGS
245500020326     c                   if        fceFGS=0
245600020326     C                   movel     §tapdr        fceFGS
245700020326     c                   end
245800020326     C                   z-add     §tapdr        fcePDR
245900020326     C                   move      §tatsr        fceTSR
246000020326     C                   z-add     §tandc        fceNDC
246100020326     C                   z-add     §taddc        fceDDC
246200020326     C                   z-add     §taaas        fceAAS
246300020326     C                   z-add     §talnp        fceLNP
246400020326     C                   z-add     §tanrs        fceNRS
246500030320     C                   move      §tacbo        fcecbo
246600020326     c                   if        §tatsr='R' and
246700020326     c                             §tansp=§taksc
246800020326     C                   move      *zero         fceNSP
246900020326     c                   else
247000020326     C                   z-add     §tansp        fceNSP
247100020326     c                   end
247200020326     c                   move      voccce        fcecce
247300020326     C*  somma algebrica del valore
247400020326     C                   add       vocice        fceice
247500020326     C                   write     fifce000
247600020326     c                   end
247700020326     C*
247800020328     c                   end
247900020328     C*
248000020328     c                   else
248100020327     C*  ------------------------>
248200020327     C*  chiamato dalla Conferma   solo per il calcolo della Base
248300020327     C*  ------------------------>
248400020327     C*   deve creare un file di work poichè la conferma conteggi imposta
248500020327     C*  i dati a video che in seguito vengono confermati.
248600020327     C*   A causa di ciò il programma di calcolo deve preparare le voci in
248700020327     C*  anticipo e poi sommarle alle righe già impostate dalla valorizzazione.
248800020327     C*  ------------------------>
248900020327     C*  ma non deve scrivere gli importi e i records della giornata
249000020327     C*  ------------------------>
249100020327      *
249200020327     C                   z-add     §tafgs        fcWFGS
249300020327     c                   if        fcWFGS=0
249400020327     C                   movel     §tapdr        fcWFGS
249500020327     c                   end
249600020327     C                   z-add     §tapdr        fcWPDR
249700020328     C                   move      §tatsr        fcWTSR
249800020327     C                   z-add     0             fcWNDC
249900020617     C                   if        §tatsr ='G'
250000020617     c                   z-add     999           fcWNDC
250100020617     c                   end
250200020327     C                   z-add     §taddc        fcWDDC
250300020327     C                   z-add     §taaas        fcWAAS
250400020327     C                   z-add     §talnp        fcWLNP
250500020327     C                   z-add     §tanrs        fcWNRS
250600020327     c                   if        §tatsr='R' and
250700020327     c                             §tansp=§taksc
250800020327     C                   move      *zero         fcWNSP
250900020327     c                   else
251000020327     C                   z-add     §tansp        fcWNSP
251100020327     c                   end
251200020617     c                   move      *blank        codt8a
251300020617      *
251400020617      *  imposta se a prestazione il codice della tariffa nel campo T8A
251500020617     c                   if        §tactr=999
251600020617     c                   movel     §tatsr        fld4              4
251700020617     c                   move      §tactr        fld4
251800020617     c                   movel     fld4          codt8a            8
251900020617     c                   move      fptCTD        codt8a
252000020617     c                   end
252100020617      *
252200020328      * consegne/ritiri
252300020327     c     §TAtsr        ifne      'G'
252400020617      *
252500020327     c                   move      voccce        fcWcce
252600020617      *
252700020327     c                   else
252800020328      *  se a giornata
252900020328      * se solo consegne
253000020327     c     vocusc        ifeq      'S'
253100020327     c     vocusr        andeq     ' '
253200020327     c                   move      voccc1        voccce
253300020327     c                   move      voccc1        fcWcce
253400020328     C                   move      'C'           fcWTSR
253500020327     c                   end
253600020328      * se solo ritiri
253700020327     c     vocusr        ifeq      'S'
253800020327     c     vocusc        andeq     ' '
253900020327     c                   move      voccc2        voccce
254000020327     c                   move      voccc2        fcWcce
254100020328     C                   move      'R'           fcWTSR
254200020327     c                   end
254300020327      * da ripartire su entrambe consegne e ritiri
254400020328      *  in base ai pesi ??????
254500020327     c     vocusc        ifeq      'S'
254600020327     c     vocusr        andeq     'S'
254700020327      *
254800020327      *
254900020327     c                   end
255000020617      *
255100020503      * Attenzione:
255200020503      * per il Picking/Etichettatura a GIORNATA
255300020503      * per le vecchie tariffe convertite deve riproporzionare
255400020503      *  l'importo impostato per la voce tenendo conto che deve
255500020503      *   essere ripartito fra Ritiri e Consegne
255600020503      *   ...per le nuove tariffe invece non deve più effettuare la
255700020503      *    ripartizione fra Ritiri e Consegne.
255800021216     C                   if        fptCTD = 'PCK' or fptCTD = 'ETI'
255900020503      *
256000020503     c     §takgc        add       §takgr        tot_kgcr          9 1
256100020506      *********
256200020506      * asteriscato dopo avere sdoppiato ETI e PCK per le nuove tariffe a giornata
256300020506      *********
256400020506     C**** fcWTSR        ifeq      'C'
256500020506     c**** tot_kgcr      andne     0
256600020506     c*********          eval      vocice = vocice * §takgc /tot_kgcr
256700020506     c*********          end
256800020506     C**** fcWTSR        ifeq      'R'
256900020506     c**** tot_kgcr      andne     0
257000020506     c*********          eval      vocice = vocice * §takgr /tot_kgcr
257100020506     c*********          end
257200020506      *********
257300020503     c                   endif
257400020328      *
257500020327     c                   end
257600020327      *
257700020327     C     Kfcw          chain     fifceW1l
257800020327      *
257900020327     c                   if        %found(fifceW1l)
258000020327      *
258100020327     C*  somma algebrica del valore
258200020329     C                   add       vocice        fcwice
258300020617      *
258400020617     c                   if        fcWt8a<>codT8a and                            prestazione
258500020619     c                             §tactr=999                                    e giornata
258600020619     c                             or §tactr<>999                               o non prestaz.
258700020617      *
258800020327     C                   update    fifcW000
258900030415     c                   feod      fifcew1L
259000020619     c                   else
259100020619     c                   unlock    fifcew1l
259200020619     c                   end
259300020327     C*
259400020327     c                   else
259500020327     C*
259600020327     C                   clear                   fifcW000
259700020327     C                   z-add     §tafgs        fcWFGS
259800020327     c                   if        fcWFGS=0
259900020327     C                   movel     §tapdr        fcWFGS
260000020327     c                   end
260100020327     C                   z-add     §tapdr        fcWPDR
260200020328      *
260300020328     c     §tatsr        ifne      'G'
260400020327     C                   move      §tatsr        fcWTSR
260500020328     c                   else
260600020328      * solo a giornata
260700020328     c                   select
260800020328      * se solo consegne
260900020328     c     vocusc        wheneq    'S'
261000020328     c     vocusr        andeq     ' '
261100020328     C                   move      'C'           fcWTSR
261200020328      * se solo ritiri
261300020328     c     vocusr        wheneq    'S'
261400020328     c     vocusc        andeq     ' '
261500020328     C                   move      'R'           fcWTSR
261600020328      *
261700020328     c                   other
261800020328     C                   move      §tatsr        fcWTSR
261900020328     c                   endsl
262000020328     c                   endif
262100020328      *
262200020327     C                   z-add     0             fcWNDC
262300020617     C                   if        §tatsr ='G'
262400020617     c                   z-add     999           fcWNDC
262500020617     c                   end
262600020327     C                   z-add     §taddc        fcWDDC
262700020327     C                   z-add     §taaas        fcWAAS
262800020327     C                   z-add     §talnp        fcWLNP
262900020327     C                   z-add     §tanrs        fcWNRS
263000020327     c                   if        §tatsr='R' and
263100020327     c                             §tansp=§taksc
263200020327     C                   move      *zero         fcWNSP
263300020327     c                   else
263400020327     C                   z-add     §tansp        fcWNSP
263500020327     c                   end
263600020327     c                   move      voccce        fcWcce
263700020327     C*  somma algebrica del valore
263800020402     C                   z-add     §takgc        fcwkgc
263900020402     C                   z-add     §takgr        fcwkgr
264000020327     C                   add       vocice        fcWice
264100020617     c                   movel     codt8a        fcWt8a
264200020327     C                   write     fifcW000
264300030415     c                   feod      fifceW1L
264400020327     c                   end
264500020327     C*
264600020326     c                   end
264700020326     C*
264800020326     C                   EndSR
264900020214     C*===============================================================
265000020328     C* Ripartisce il valore Voce C/E x stop sulle bolle dello stop
265100020328     C*===============================================================
265200020328     C     VOC_Stop      BegSR
265300020328     C*
265400020328     C                   z-add     §tafgs        ftwFGS
265500020328     c                   if        §taFGS=0
265600020328     C                   movel     §tapdr        ftwFGS
265700020328     c                   end
265800020328     C                   z-add     §tapdr        ftwPDR
265900020328     C                   move      §tatsr        ftwTSR
266000020328     C                   z-add     §tandc        ftwNDC
266100020328     C                   z-add     §taddc        ftwDDC
266200020328     C                   z-add     §tastp        ftwSTP
266300020328     C                   z-add     vocice        restoICE
266400020328     C*
266500020328     C* ciclo su tutte le bolle appartenenti allo STOP
266600020328     C     Kftdw         setll     fiftdw1l
266700020328     C     Kftdw         reade     fiftdw1l
266800020328     c                   dow       not %eof(fiftdw1l)
266900020328     C*
267000020328     C* Crea il valore da ripartire:
267100020328     C*  proporziona la PARte relativa al peso della spedizione e si
267200020328     C*   memorizza il resto
267300020403     c                   eval      parice = 0
267400020403     c                   if        §takfa > 0
267500020329     c                   eval      parice = vocice * ftwpkl / §takfa
267600020403     c                   end
267700020328     c                   eval      restoice = restoice - parice
267800020328     C*
267900020328     C                   z-add     ftwfgs        fceFGS
268000020328     c                   if        fceFGS=0
268100020328     C                   movel     ftwpdr        fceFGS
268200020328     c                   end
268300020328     C                   z-add     ftwpdr        fcePDR
268400020328     C                   move      ftwtsr        fceTSR
268500020328     C                   z-add     ftwndc        fceNDC
268600020328     C                   z-add     ftwddc        fceDDC
268700020328     C                   z-add     ftwaas        fceAAS
268800020328     C                   z-add     ftwlnp        fceLNP
268900020328     C                   z-add     ftwnrs        fceNRS
269000020328     C                   z-add     ftwnsp        fceNSP
269100030320     c                   if        ftwtsr = 'R'
269200030320     c     kbol          chain     fnblp01l
269300030320     c                   move      blpcbo        fcecbo
269400030320     c                   else
269500030320     c     kbol          chain     fnarb01l
269600030320     c                   move      arbcbo        fcecbo
269700030320     c                   end
269800020328     c                   move      voccce        fcecce
269900020328     C*
270000020328     C     Kfce          chain     fifce01l
270100020328     c                   if        %found(fifce01l)
270200020328     C*  somma algebrica del valore
270300020328     C                   add       parice        fceice
270400020328     C                   update    fifce000
270500020328     C*
270600020328     c                   else
270700020328     C*
270800020328     C                   clear                   fifce000
270900020328     C                   z-add     ftwfgs        fceFGS
271000020328     c                   if        fceFGS=0
271100020328     C                   movel     ftwpdr        fceFGS
271200020328     c                   end
271300020328     C                   z-add     ftwpdr        fcePDR
271400020328     C                   move      ftwtsr        fceTSR
271500020328     C                   z-add     ftwndc        fceNDC
271600020328     C                   z-add     ftwddc        fceDDC
271700020328     C                   z-add     ftwaas        fceAAS
271800020328     C                   z-add     ftwlnp        fceLNP
271900020328     C                   z-add     ftwnrs        fceNRS
272000020328     C                   z-add     ftwnsp        fceNSP
272100020328     c                   move      voccce        fcecce
272200020328     C*  somma algebrica del valore
272300020328     C                   add       parice        fceice
272400030320     c                   if        ftwtsr = 'R'
272500030320     c     kbol          chain     fnblp01l
272600030320     c                   move      blpcbo        fcecbo
272700030320     c                   else
272800030320     c     kbol          chain     fnarb01l
272900030320     c                   move      arbcbo        fcecbo
273000030320     c                   end
273100020328     C                   write     fifce000
273200020328     c                   end
273300020328     C*
273400020328     C     Kftdw         reade     fiftdw1l
273500020328     c                   enddo
273600020328     C*
273700020328     C*  se c'è un resto ancora da attribuire lo ripartiamo sull'ultima
273800020328     C*  bolla letta.
273900020423     c                   if        restoICE <> 0
274000020328     C*
274100020328     C     Kftdw         setgt     fiftdw1l
274200020328     C     Kftdw         readpe    fiftdw1l
274300020328     c                   if        not %eof(fiftdw1l)
274400020328     C*
274500020328     C                   z-add     ftwfgs        fceFGS
274600020328     c                   if        fceFGS=0
274700020328     C                   movel     ftwpdr        fceFGS
274800020328     c                   end
274900020328     C                   z-add     ftwpdr        fcePDR
275000020328     C                   move      ftwtsr        fceTSR
275100020328     C                   z-add     ftwndc        fceNDC
275200020328     C                   z-add     ftwddc        fceDDC
275300020328     C                   z-add     ftwaas        fceAAS
275400020328     C                   z-add     ftwlnp        fceLNP
275500020328     C                   z-add     ftwnrs        fceNRS
275600020328     C                   z-add     ftwnsp        fceNSP
275700020328     c                   move      voccce        fcecce
275800020328     C*
275900020328     C     Kfce          chain     fifce01l
276000020328     c                   if        %found(fifce01l)
276100020328     C*  somma algebrica del valore
276200020328     C                   add       restoice      fceice
276300020328     C                   update    fifce000
276400020328     c                   end
276500020328     C*
276600020328     c                   end
276700020328     C*
276800020328     c                   end
276900020328     C*
277000030108     C                   EndSR
277100030108     C*===============================================================
277200030108     C* ECCEZIONI :
277300030108     C*===============================================================
277400030108     C* Ritiri Annullati fanno Eccezione:
277500030108     C*===============================================================
277600030108     C     Ritiri_Ann    BegSR
277700030108     C*
277800030108     C* Poichè vengono calcolati al momento in cui il programma è chiamato
277900030108     C* a prestazione ossia con tariffa R999 e hanno una tariffa definita
278000030108     C* a dettaglio anche se poi in fondo è un valore fisso moltiplicato
278100030108     C* per il numero dei ritiri.
278200030108     C*
278300030108     C*---------------------------------------------------------------*
278400030108     C                   clear                   ficnb3
278500030108     C                   movel     §tapdr        §t3pdr
278600030108     C                   movel     §tasml        §t3sml
278700030108     C                   movel     'V'           §t3fvl
278800030108     C                   movel     §tatsr        §t3tsr
278900030108     C                   z-add     §tandc        §t3ndc
279000030108     C                   z-add     §taddc        §t3ddc
279100030108     C                   movel     §tadiv        §t3div
279200030108     C                   movel     'S'           §t3ran
279300030108     C                   CALL      'FICNB3R1'
279400030108     C                   PARM                    ficnb3
279500030331      * se non c'è una tariffa
279600030331     C                   if        §t3CTR = '???'
279700030331     c                   goto      fine_ritann
279800030331     c                   endif
279900030108      ****
280000030108      * reperisce il progressivo giusto per prendere la tariffa corretta
280100030108     C                   movel     §t3PDR        W01PDR
280200030108     C                   movel     §t3CTR        W01CTR
280300030108     C                   movel     §t3TSR        W01TSR
280400030108      *               *-------------------------*
280500030108     C                   exsr      Ric_Tariffa
280600030108      *               *-------------------------*
280700030108     C     Tar_valida    ifeq      'N'
280800030108     c                   goto      fine_ritann
280900030108     C                   endIF
281000030108      *
281100030108     C* SE C'E' RIFERIMENTO ALTRA TARIFFA USO QUESTO PER IL DETTAGLIO
281200030108    1C     FGTRAP        IFGT      0
281300030108     C                   Z-ADD     FGTRAP        W01PDR
281400030108     C                   Z-ADD     FGTRCT        W01CTR
281500030108      *               *-------------------------*
281600030108     C                   EXSR      Ric_Tariffa
281700030108      *               *-------------------------*
281800030108     C     Tar_valida    ifeq      'N'
281900030108     c                   goto      fine_ritann
282000030108    1C                   end
282100030108    1C                   ENDIF
282200030108     C*
282300030108     C*---------------------------------------------------------------*
282400030108      * Chiave di lettura
282500030108     C                   eval      k01PDR  =   fgtPDR
282600030108     C                   eval      k01SML  =   fgtSML
282700030108     C                   eval      k01TSR  =   fgtTSR
282800030108     C                   eval      k01CTR  =   fgtCTR
282900030108     C                   eval      k01PRG  =   fgtPRG
283000030108     C                   move      'RAN'         k01CTD
283100030108      *
283200030108      * Leggendo per livello di Tipo Somma
283300030108     C     kfpt01        chain     fifpt01l
283400030108      *
283500030108     C                   if        %found(fifpt01L) and F01atb=*blank
283600030108     c                   move      f01CTD        fptctd
283700030404     C*
283800030404     C*  Aggancia la Tabella per prendere la Voce del Conto.
283900030404     c                   Exsr      Tab_8A
284000030108     C*
284100030108     c     f01ata        mult      §tatbs        w01RAN           18 3
284200030108      *
284300030108     c     Piumen        ifeq      '-'
284400030108     C                   z-sub     w01RAN        §TARAN
284500030108     c                   sub       w01RAN        vocice
284600030108     c                   else
284700030108     C                   z-add     w01RAN        §TARAN
284800030108     c                   add       w01RAN        vocice
284900030108     c                   end
285000030108      *
285100030108      *  Non deve aggiornare se in simulazione
285200030108      *   Prepara i dati per il FIFCE per comparazione al Conto Economico
285300030108     c                   if        §tasml=' '
285400030108     c                   Exsr      xContoEcon
285500030108     c                   end
285600040506     C*
285700040506     c                   else
285800040506     C*
285900040506     C* se non trovata tariffa su tariffa di riferimento
286000040506     C                   eval      Tar_valida = 'N'
286100030108     C*
286200030108     c                   end
286300030108     C*
286400030108     C     fine_ritann   EndSR
286500020328     C*===============================================================
286600030529     C* Ci sono Tariffe Particolari ?! da sapere prima di tassare ?
286700030529     C*===============================================================
286800030529     C     Tariffe_Part  BegSR
286900030529     C*
287000030529     C*  Pulisce i flags Particolari da impostare
287100030529     C*    x tariffa FW
287200030529     c                   clear                   Bolla_FW          1
287300030821     c                   clear                   Bolla_2R          1
287400091009     c                   move      'NO'          Si_TarH1030       2
287500030529     C* ------
287600030529      * Chiave di lettura generale prima a Spedizione e poi a Stop
287700030529     C                   eval        k02PDR  =   fgtPDR
287800030529     C                   eval        k02SML  =   fgtSML
287900030529     C                   eval        k02TSR  =   fgtTSR
288000030529     C                   eval        k02CTR  =   fgtCTR
288100030529     C                   eval        k02PRG  =   fgtPRG
288200030529      *
288300030529     c                   do        2             volte             3 0
288400030529      * imposta il Tipo Somma
288500030529     c                   if        volte = 1
288600030529     C                   eval        k02TSM  =   'S'
288700030529     c                   end
288800030529     c                   if        volte = 2
288900030529     C                   eval        k02TSM  =   'T'
289000030529     c                   end
289100030529      *
289200030529     C     kfpt02        setll     fifpt002
289300030529     C     kfpt02        READE     fifpt002
289400030529      *
289500030529     C                   Dow       not %Eof(Fifpt02l)
289600030529     C* ------>
289700091009      *  se NON presente tariffa H10:30 se lo memorizza poichè, le spedizioni in ESPRESSO
289800091009      *   dovranno allora essere valorizzate con la vecchia "E" Espresso = Priority.
289900091009     c                   if        fptctd='H  ' and §taTSP='H' and fptatb=' '
290000091009     c                   eval      Si_TarH1030 = 'SI'
290100091009     c                   end
290200091009      *
290300030529     C* Se è una bolla Particolare. x Recupero C/A
290400030529     C*  questo tipo di bolla non deve valorizzarsi con tutte le tariffe
290500030529     C* ma solo con un fisso particolare.
290600030529     c                   if        fptctd = 'FW ' and §taCBO ='FW' and
290700030529     c                             fptatb = ' '
290800030821     C* sempre a spedizione e a stop solo se non ha altre spedizioni nello stesso Stop
290900030821     c*******            if        §taTSM ='S' or
291000030821     c*******                      §taTSM ='T' and (§tancl = 1 or §tacpe = 1)
291100030821     c                   if        §tanss = 1 or
291200030821     c                             §taNSS = §taFW
291300030529     c                   move      'S'           Bolla_FW
291400030529     C                   End
291500030529     C                   End
291600030529     C* ------>
291700030821     C* Se è una bolla Particolare. x Ritiro Contestuale alla Consegna
291800030821     C*  questo tipo di bolla non deve valorizzarsi con tutte le tariffe
291900030821     C* ma solo con un fisso particolare.
292000030821     c                   if        fptctd = 'RCC' and §taCBO ='2R' and
292100030821     c                             fptatb = ' '
292200030821     C* sempre a spedizione e a stop solo se non ha altre spedizioni nello stesso Stop
292300030821     c                   if        §taNSS = 1 or
292400030821     c                             §taNSS = §taRCC
292500030821     c                   move      'S'           Bolla_2R
292600030821     C                   End
292700030821     C                   End
292800030821     C* ------>
292900030529      *
293000030529     C     kfpt02        READE     fifpt002
293100030529     C                   EndDO
293200030529      *
293300030529     C                   EndDO
293400030529     C* ------
293500090914      * Carica Tariffe Particolari
293600090914      *   Se il programma è chiamato in modalità "S" a Spedizione ossia il TSM ="S"
293700090914      *   deve essere passato il codice dell'autista con tariffe particolari
293800090914      *   con il quale andare a controllare sulla tabella CTP se ci sono delle tariffe
293900090914      *   da sostituire.
294000090914      *   Se il programma è chiamato in modalità "T" a STOP ossia il TSM ="T" può essere
294100090914      *   passato il codice dell'autista con tariffe particolari o 9999999 se all'interno
294200090914      *   dello stop ci sono spedizioni di clienti con Tariffe Particolari per i
294300090914      *   quali andare a controllare sulla tabella CTP se ci sono tariffe da sostituire.
294400090914      *
294500090914      *   Nel caso con 9999999 non è possibile identificare i clienti con quali tariffe
294600090914      *   quindi si paga in maniera standard.
294700090914      *
294800090914     c                   eval      Tar_da_sostituire = ' '
294900090914     c                   eval      x_il_cliente   = 0
295000090914      *
295100090914      *  deve essere però specifica sul cliente x essere applicata
295200090915     c                   if        (§TACLIPAR > 0 and §TACLIPAR < 9999999)
295300090915     c                               or §TATUTTE  ='S'
295400090915      *
295500090915     **  quindi ci sono tariffe particolari sul cliente da prendere in considerazione
295600090914     c                   eval      Tar_da_sostituire = 'S'
295700090914     c                   eval      x_il_cliente      = §TACLIPAR
295800090914      *
295900090914     C                   End
296000090914      *
296100030529     C                   EndSR
296200030529     C*===============================================================
296300090914      *
296400090914     C*===============================================================
296500090914     C     Ignora_TariffaBegSR
296600090915      *
296700090915     c                   clear                   Tariffa_Letta     4
296800090915     c                   eval      Tariffa_Letta = fptTSR + fptCTD
296900090915      * >>>>>>>>>>>
297000090915     c                   IF         Tar_da_sostituire = *blank
297100090915      * >>>>>>>>>>>
297200090915      *  Deve comunque controllare se la tariffa letta è una tariffa di quelle
297300090915      *   da utilizzare SOLO in sostituzione
297400090915      * se c'è in tabella allora è una di quelle da saltare
297500090915     c     Tariffa_Letta lookup    KtarNEW                                33
297600090915     c                   if        *in33
297700090915     c                   eval      tariffa_da_saltare = 'S'
297800090915     c                   end
297900090915      *
298000090915      * >>>>>>>>>>>
298100090915     c                   ElseIF     Tar_da_sostituire = 'S'
298200090915      * >>>>>>>>>>>
298300090915      *  cicla su tutte le tariffe
298400090915      * deve applicara le tariffe in sostituzione x l'autista
298500090914     c                   z-add     1             CP
298600090915     c                   doW         CP <= %elem(KCliPar)
298700090915      *
298800090915      *   esce quando ha finito gli elementi validi
298900090915     c                   if          KCliPar(CP) = 0
299000090915     c                   leave
299100090915     c                   end
299200090915      * --------
299300090915      *  se è quelle da considerare del cliente per la sostituzione
299400090915     c                   iF          x_il_cliente = KCliPar(CP)
299500090915      *
299600090915      *  controlla se è la tariffa particolare
299700090915     c                   if        KtarNew(CP) = Tariffa_Letta
299800090915      * allora NON deve poi utilizzare la relativa tariffa Standard collegata.
299900090914     c                   eval      KSTDDaExcl(CP) = KtarSTD(CP)
300000090914     c                   end
300100090915      *
300200090914      *  controlla se la tariffa Standard è già stata sostituita nel calcolo
300300090915     c                   if        KtarSTD(CP) = Tariffa_Letta  and
300400090914     c                             KSTDDaExcl(CP) = KtarSTD(CP)
300500090915      *  e quindi NON deve essere utilizzata.
300600090914     c                   eval      tariffa_da_saltare = 'S'
300700090915     c                   end
300800090915      * --------
300900090915     c                   else
301000090915      * --------
301100090915      *  Dopo aver controllato fra le tariffe specifiche del cliente
301200090915      *  occorre verificare che la tariffa letta non sia fra quelle necessarie
301300090915      *  per la sostituzione per altri clienti
301400090915     c                   if        KtarNew(CP) = Tariffa_Letta
301500090915     c                   movel     Tariffa_Letta Tar_Letta_CLI    11
301600090915     c                   move      x_il_cliente  Tar_Letta_CLI    11
301700090915     c     Tar_Letta_CLI lookup    KTsrNewCli                             33
301800090915      *  se non è stato trovato nello specifico sul cliente allora è una
301900090915      *   tariffa sostitutiva di un altro cliente ed in quanto tale non deve
302000090915      *   essere presa in considerazione su questo cliente.
302100090915     c  N33              eval      tariffa_da_saltare = 'S'
302200090915     c                   end
302300090915      *
302400090915     c                   end
302500090915      * --------
302600090914      *   incrementa il contatore di schiera da leggere
302700090915     c                   eval        CP = CP + 1
302800090914     c                   endDo
302900090915      * >>>>>>>>>>>
303000090914     C                   END
303100090915      * >>>>>>>>>>>
303200090914      *
303300090914     C                   EndSR
303400090914     C*===============================================================
