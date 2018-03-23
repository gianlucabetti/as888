000100081222     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000101120724     H BNDDIR('TIBS') EXTBININT(*YES)
000200081218     H* ficn01R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400081218     H*               GESTIONE SUPERTESTA TARIFFE PADRONCINI         *
000500000000     H*--------------------------------------------------------------*
000600081218     Fficn01D   CF   E             WORKSTN
000700030121     F                                     SFILE(CN10S02:NRR)
000800081218     Ffitgt01L  iF   E           K DISK
000900021203     Ffiapd01L  IF   E           K DISK
001200930923     D*
001300940920     D* PARAMETRO PASSATO PER ACCEDERE ALLE TARIFFE SIMULAZIONE
001400940920     D*   DIRETTAMENTE DA MENU'
001500131008     d tibs42ds      e ds
001501131008     d uteautds      e ds
001600081219     d ficn01ds      e ds
001700081219     d ficn02ds      e ds
001800090112     d ficn06ds      e ds
002300921102     D WLBDAT          DS
002400940906     D  G02DAT                 1      8  0
002500940906     D  G02INV                 9     16  0
002600940906     D  G02ERR                17     17
002700940906     D  G02TGI                18     22  0
002800940504     D                 DS
002900940906     D  AA                     1      4  0
003000940906     D  MM                     5      6  0
003100940906     D  GG                     7      8  0
003200940906     D  DATA                   1      8  0
003300081219     D*
003400000000     D KPJBA         E DS
003500090323     c* stampa prova allegato
003600090323     dparam            ds
003700090323     D lancio                         1
003800090323     d codpadr                        7s 0
003900090323     D codpadr2                       7s 0
004000090323     d alladat                        8s 0
004100090504     d pardtcerta                     8s 0
004200090504     d parmNRC                        7s 0
004300090504     d parmFIL                        3s 0
004400090504     D OUTQOVR                       10
004500090504     D errore                         1
004600091021     D simulata                       1
004700090504
004800930914     D*
004900900320     D CNCR80        E DS
005000021203     D fnlv24ds      E DS
005100900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005200931115     D  TCU                  398    697
005300931115     D                                     DIM(50)                              TAB FLAG CNT
005400931115     D  KCU                  698    847P 0
005500931115     D                                     DIM(50)                              TAB CAPO CONTO
005600931115     D                                     PACKEVEN
005700931115     D  DTU                  848    895P 0
005800931115     D                                     DIM(12)                              DATE PARAM
005900931115     D                                     PACKEVEN
006000090427     d savnrr          s                   like(nrr)
006100090427     d dtaiso          s               d
006200090323     d dtaeur          s               d   datfmt(*eur)
006201120724ab   ** Prototipi.
006202120724ab    **
006203120724ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
006204120724ab    /DEFINE DFTACTGRP_NO
006205120724ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
006206120724ab    /UNDEFINE DFTACTGRP_NO
006207120724      *
006208120724ab   D TibsSof_esito...
006209120724ab   D                 S             10I 0 IMPORT
006210120724ab   D idSocieta...
006211120724EDPDCD                 S              3A
006212120724ab   **
006213120724EDPDCd dtVALIDITA      s               D
006214120724EDPDCd VALdatFINE      s               D
006215120724EDPDCd VALdatINIZ      s               D
006300930811     C*****************************************************************
006400930928     C* ESEGUO OPERAZIONI INIZIALI
006500930928     C                   EXSR      OPERAZ
006600930903     C*
006700930914     C* ALF1 = 'F' ===> VADO A FINE PGM
006800930811     C     ALF1          DOWNE     'F'
006900930824     C*
007000081219     C* ALF1 = '1' ===> scelta codice autista
007100930811     C     ALF1          IFEQ      '1'
007200930811     C                   EXSR      VIDEO1
007300930811     C                   ENDIF
007400930811     C*
007500081219     C* ALF1 = '2' ===> GESTIONE VIDEATA SUBFILE supertestata
007600930811     C     ALF1          IFEQ      '2'
007700930811     C                   EXSR      VIDEO2
007800930811     C                   ENDIF
007900020125     C*
008000930811     C                   ENDDO
008100940930     C*
008200930820     C                   SETON                                        LR
008300930928     C*
008400930811     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
008500930811     C     VIDEO1        BEGSR
008600011109     C*---------------------------------------------------------------*
008700090428     c                   clear                   msgv
008800930907     C*
008900081219     C                   do        *hival
009000020124     C                   EXFMT     CN10D01
009100930929     C*
009200930811     C** CMD3  - FINE LAVORO
009300931013     C   KC              MOVEL     'F'           ALF1
009400081219     C   KC              leave
009500931013     C*
009600940928     C** CMD7  - GESTIONE ANAGRAFICA PADRONCINI
009700930907     C     *INKG         IFEQ      *ON
009900131022     C                   Z-ADD     VIDFIL        d24FIL
010000021203     C                   Z-ADD     0             d24pdr
010100030204     C                   MOVEL     'R'           d24FLG
010200081219     C                   MOVEL     'A'           d24tip
010300021203     C                   MOVEL     *BLANKS       d24rsc
010400021203     C                   MOVEL(p)  fnlv24ds      KPJBU
010500940926     C                   CALL      'FNLV24R'
010600930811     C                   PARM                    KPJBA
010700030204     C                   MOVEL     KPJBU         fnlv24ds
010800030204     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
010900030204     C     d24pdr        IFNE      0
011000030204     C                   MOVEL     d24pdr        VIDFIL
011100030204     C                   MOVEL     *ZEROS        VIDPDR
011200030204     C                   MOVE      d24pdr        VIDPDR
011300030204     C                   MOVEL     d24rsc        DESPDR
011400030204     C                   ENDIF
011500930907     C*
011600081219     C                   iter
011700930811     C                   ENDIF
011800930811     C*
011900930907     C** CMD14 - GESTIONE ANAGRAFICA FORNITORI
012000011126     c                   if        *inkn
012100011126     c                   movel(p)  'P'           kpjbu
012200011126     C                   CALL      'TRUL16R'
012300930907     C                   PARM                    KPJBA
012400081219     C                   iter
012500011126     c                   end
012600940919     C*
012700930811     C** CONTROLLI DELLA PRIMA VIDEATA
012800930907     C                   EXSR      CONTR1
012900081219     C   90              iter
013000930811     C*
013100930907     C                   MOVEL     '2'           ALF1
013200081219     c                   leave
013300081219     c                   enddo
013400081219     C                   ENDSR
013500930811     C*
013600930811     C*--- CONTROLLI DELLA PRIMA VIDEATA -----------------------------*
013700930811     C     CONTR1        BEGSR
013800011109     C*---------------------------------------------------------------*
013900081219     C                   SETOFF                                       408390
014000930811     C*
014100940920     C****  CODICE FILIALE  ****
014200950113     C* FILIALE OBBLIGATORIA
014300030529     C     VIDFIL        IFEQ      0
014400950113     C                   SETON                                        8390
014500081219     C                   leavesr
014600950113     C                   ENDIF
014700950113     C*
014800131022     C* contyrollo autorizzazione
015000131008     c                   clear                   tibs42ds
015100131008     c                   movel     vidfil        i42pge
015300131008     c                   call      'TIBS42R'
015400131008     c                   parm                    tibs42ds
015401131008     c                   movel     o42uni        uteautds
015500131022     c                   if        o42err <> ' ' or §AUTMTC <> 'S'
015600090109     c                   seton                                        8390
015700090109     c                   leavesr
015800090109     c                   endif
016700940920     C*
016800930811     C****  CODICE PADRONCINO  ****
016900930908     C     '?'           SCAN      VIDPDR                                 90
017000930811     C*
017100930811     C     *IN90         IFEQ      *ON
017200021203     C                   Z-ADD     VIDFIL        d24FIL
017300021203     C                   Z-ADD     0             d24pdr
017400021203     C                   MOVEL     'R'           d24FLG
017500081219     C                   MOVEL     'A'           d24tip
017600021203     C                   MOVEL     *BLANKS       d24rsc
017700021203     C                   MOVEL(p)  fnlv24ds      KPJBU
017800940926     C                   CALL      'FNLV24R'
017900930811     C                   PARM                    KPJBA
018000021203     C                   MOVEL     KPJBU         fnlv24ds
018100941006     C*
018200941006     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
018300021203     C     d24pdr        IFNE      0
018400021203     C                   MOVEL     d24pdr        VIDFIL
018500930921     C                   MOVEL     *ZEROS        VIDPDR
018600021203     C                   MOVE      d24pdr        VIDPDR
018700021203     C                   MOVEL     d24rsc        DESPDR
018800941006     C                   ENDIF
018900941006     C*
019000081219     C                   leavesr
019100930811     C                   ENDIF
019200930811     C*
019300930921     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
019400930921     C                   TESTN                   VIDPDR               35
019500930921     C  N35              SETON                                        4090
019600081219     C  N35              leavesr
019700931111     C*
019800940915     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
019900940920     C                   MOVEL     VIDFIL        COMPDR
020000940920     C                   MOVE      VIDPDR        COMPDR
020100081219     C                   MOVE      compdr        vidaut
020200090604     c* errore annullato solo per gestione
020300021203     C     kapdc         CHAIN     fiapd000                           32
020400090604     c                   if        not *in32 and not *in10
020500090604     C     APDATB        IFNE      ' '
020600940914     C                   SETON                                        32
020700940914     C                   ENDIF
020800090604     C                   ENDIF
020900940914     C* CODICE PADRONCINO ERRATO
021000940914     C   32              SETON                                        4090
021100081219     C   32              leavesr
021200100204      *
021300100204     c*  se escluso da autofatturazione dare msg di errore
021400100204     c                   if        apdpdd = 'S'
021500100204     C                   SETON                                        4090
021600100204     C                   leavesr
021700100204     c                   end
021800100204      *
021900090706     C                   MOVEL     APDRSf        DESPDR
022000930908     C*
022100081219     C                   ENDSR
022200930811     C*
022300930811     C*--- GESTIONE DELLA SECONDA VIDEATA ----------------------------*
022400930811     C     VIDEO2        BEGSR
022500011109     C*---------------------------------------------------------------*
022600081219     C* P U L I Z I A   S U B F I L E
022700081219     C                   EXSR      PULSF
022800020410     C* CARICAMENTO SUBFILE
022900081218     C                   EXSR      CARSF
023000081219     c                   if        nrr = 0
023100081219     c                   seton                                        87
023200081222     c                   else
023300081222     c                   setoff                                       87
023400081219     c                   end
023500930820     C*
023600020205     c                   do        *hival
023700081219     C                   Z-ADD     1             REC
023800020125     C                   WRITE     CN10Z02
023900020124     C                   EXFMT     CN10C02
024000930820     C*
024100930820     C** CMD12 - RITORNO
024200930914     C     *INKL         IFEQ      *ON
024300930914     C                   SETOFF                                       11
024400930914     C                   MOVEL     '1'           ALF1
024500081219     C                   leave
024600930914     C                   ENDIF
024700930820     C** CMD9  - AGGIUNTA TARIFFA
024800930914     C     *INKI         IFEQ      *ON
024900090828     c                   exfmt     LRF6W01
025000090828     c                   if        *inkl
025100090828     c                   iter
025200090828     c                   end
025300081219     C                   clear                   ficn02ds
025400081219     C                   Z-ADD     COMPDR        cn02pdr                        *PDR VECCHIO
025500081219     C                   MOVEL     cn01SML       cn02sml                        *SML VECCHIO
025600081222     C                   MOVEL     '1'           cn02mod                        *SML VECCHIO
025700081219     C                   MOVEL     VIDdiv        cn02div                        *CTR NUOVO
025800081219     C                   z-add     0             cn02prg                        *CTR NUOVO
025900081219     C                   MOVEL(p)  ficn02ds      KPJBU
026000081219     C                   CALL      'FICN02R'
026100930825     C                   PARM                    KPJBA
026200081219     C                   MOVEL     KPJBU         ficn02ds
026300090423     c                   if        cn02err <> ' '
026400090423     c                   movel     cn02msg       msgv
026500090423     c                   else
026600090423     c                   clear                   msgv
026700090423     c                   end
026800081222     C                   leave
026900930823     C                   ENDIF
027000090429     c                   if        nrr = 0
027100090429     c                   iter
027200090429     c                   end
027300081219     C* CONTROLLO SE E' STATA SCELTA UNA TARIFFA
027400081222     c                   do        *hival        xyz               3 0
027500081222     c     xyz           chain     cn10s02                            56
027600081222     c                   if        *in56
027700081222     c                   leave
027800081222     c                   end
027900930825     C*
028000081219     c                   if        vi2sce <> ' '
028100081219     C                   clear                   ficn02ds
028200081219     C                   Z-ADD     COMPDR        cn02pdr                        *PDR VECCHIO
028300081219     C                   MOVEL     cn01SML       cn02sml                        *SML VECCHIO
028400081219     C                   MOVEL     VI2div        cn02div                        *CTR NUOVO
028500081219     C                   z-add     VI2PRG        cn02PRG
028600081219     C                   MOVEL     vi2sce        cn02mod                        *SML VECCHIO
028700081223     C                   MOVEL     'A'           cn02ana                        *SML VECCHIO
028800081219     c                   select
028900081219     c                   when      vi2sce = '1'
029000081229     C                   MOVEL     cn01mod       cn02mod                        *SML VECCHIO
029100081229     C                   MOVEL(p)  ficn02ds      KPJBU
029200081219     C                   CALL      'FICN10R'
029300081219     C                   PARM                    KPJBA
029400090427     C                   MOVEL     KPJBU         ficn02ds
029500090325     c                   if        cn02err <> ' '
029600090325     c                   movel     cn02msg       msgv
029700090325     c                   else
029800090325     c                   clear                   msgv
029900090325     c                   end
030000090108     c* annullamento solo per edp
030100090108     c                   when      (vi2sce = '2' and not *in10) or
030200090108     c                             vi2sce = '5' or
030300090427     c                             (vi2sce = '4' and *in44 and not *in10)
030400090427     c* faccio annullare solo se non ho progressivi successivi
030500090427     c                   if        vi2sce = '4' and xyz <> savnrr
030600090427     c                   eval      msgv = 'Tariffa non annullabile esistono -
030700090427     c                             progressivi successivi'
030800090427     c                   else
030900081229     C                   MOVEL(p)  ficn02ds      KPJBU
031000081219     C                   CALL      'FICN02R'
031100081219     C                   PARM                    KPJBA
031200090427     C                   MOVEL     KPJBU         ficn02ds
031300090325     c                   if        cn02err <> ' '
031400090325     c                   movel     cn02msg       msgv
031500090325     c                   else
031600090325     c                   clear                   msgv
031700090325     c                   end
031800090427     c                   end
031900090112     c* copia
032000090112     c                   when      (vi2sce = '3' and not *in10)
032100090112     C                   clear                   ficn06ds
032200090112     C                   movel     COMPDR        filold                         *PDR VECCHIO
032300090112     C                   move      COMPDR        pdrold                         *PDR VECCHIO
032400090112     C                   MOVEL     cn01SML       smlold                         *SML VECCHIO
032500090112     C                   z-add     VI2PRG        PRGold
032600090112     C                   MOVEL(p)  ficn06ds      KPJBU
032700090112     C                   CALL      'FICN85C'
032800090112     C                   PARM                    KPJBA
032900100416     c*
033000090324     c* conferma la tariffa no simulazione
033100090324     c                   when      vi2sce = 'C' and not *in10 and not *in19
033101120724      *controllo congruenza società
033103120724     C                   move      vi2DDT        dtaeur
033104120724     C                   move      dtaeur        dtaiso
033105120725ab   C                   exsr      TEST_societa
033106120724ab   c                   if        apdcsf <> idsocieta
033108120724     c                   eval      msgv = 'Tariffa non Convalidabile -
033109120724     c                             società fornitore non congruente -
033110120724     c                             alla società operativa'
033111120724     c                   leavesr
033112120724ab   C                   end
033113120724ab   c                   if        tgtdst > valdatfinew
033115120724     c                   eval      msgv = 'Scadenza tariffa maggiore -
033116120724     c                             di data scadenza della società -
033117120724     c                             operativa'
033118120724     c                   leavesr
033119120724ab   C                   end
033200100416      *
033300100416      * PARAMETRI DI CONTROLLO
033400100416     c                   CLEAR                   IN_Simula         1
033500100416     c     VIDFIL        MULT      10000         IN_Autista        7 0
033600100416     c                   move      VIDPDR        IN_Autista
033700100416     c                   Z-ADD     VI2PRG        IN_Progress       3 0
033800100416     c                   CLEAR                   OUT_TServ         1
033900100416     c                   CLEAR                   OUT_Tariffa       3 0
034000100416     c                   CLEAR                   OUT_CodDetta      3
034100100416     c                   CLEAR                   OUT_DesCodice    25
034200100416     c                   CLEAR                   OUT_Errore       50
034300100416     c*
034400100416     c* Controlla se ci sono problAmi sulle tariffe prima di confermarle
034500100416     C                   CALL      'FICN11R1'
034600100416     c                   parm                    IN_Autista
034700100416     c                   parm                    IN_Simula
034800100416     c                   parm                    IN_Progress
034900100416     c                   parm                    OUT_TServ
035000100416     c                   parm                    OUT_Tariffa
035100100416     c                   parm                    OUT_CodDetta
035200100416     c                   parm                    OUT_DesCodice
035300100416     c                   parm                    OUT_Errore
035400100416      ****
035500100416     c                   IF          OUT_Errore <> *BLANKS
035600100416     c*
035700100416     c                   if        OUT_TServ = 'C'
035800100416     c                   eval      msgv = 'Tar.Consegna: codice '
035900100416     c                             + OUT_CodDetta
036000100416     c                   elseIf    OUT_TServ = 'R'
036100100416     c                   eval      msgv = 'Tar.Ritiro  : codice '
036200100416     c                             + OUT_CodDetta
036300100416     c                   elseIf    OUT_TServ = 'G'
036400100416     c                   eval      msgv = 'Tar.Giornata: codice '
036500100416     c                             + OUT_CodDetta
036600100416     c                   end
036700100416     c*
036800100416     c                   eval      msgv =  %Trim(msgv) + '. ' + OUT_Errore
036900100416     c                   Else
037000100416      *
037100100416      * errore se tariffa già convalidata
037200090508     c                   if        VI2DTS <>0
037300090508     c                   eval      msgv = 'Tariffa già convalidata'
037400090508     c                   else
037500090428     c* non faccio confermare la tariffa se aut escluso da autofatt.
037600090428     c                   if        apdpdd ='S'
037700090428     c                   eval      msgv = 'Tariffa non convalidabile, AUT esclu-
037800090428     c                             so da autofatturazione'
037900090428     c                   else
038000090323     C                   MOVEL(p)  ficn02ds      KPJBU
038100090323     C                   CALL      'FICN07R'
038200090323     C                   PARM                    KPJBA
038300090325     c                   movel     kpjbu         ficn02ds
038400090325     c                   if        cn02err <> ' '
038500090325     c                   movel     cn02msg       msgv
038600090325     c                   else
038700090325     c                   clear                   msgv
038800090325     c                   end
038900090428     c                   end
039000090508     c                   end
039100100416     c                   ENDif
039200090323     c* stampa di prova allegati
039300090323     c                   when      vi2sce = 'S'
039400090504     c                   clear                   param
039500090504     c                   eval      lancio = 'P'
039600090323     c                   eval      codpadr = vidaut
039700090323     c                   eval      codpadr2= vidaut
039800090323     C                   move      vi2DDT        dtaeur
039900090323     C                   move      dtaeur        dtaiso
040000090323     C                   MOVE      dtaiso        alladat
040100091021     c   19              move      'S'           simulata
040200090421     c                   movel(p)  param         kpjbu
040300090504     C                   CALL      'FICN14R1'
040400090323     C                   PARM                    KPJBA
040500081219     c                   endsl
040600081222     c                   clear                   vi2sce
040700081222     c                   update    cn10s02
040800081219     C                   MOVEL     KPJBU         ficn02ds
040900081219     c                   end
041000020205     c                   enddo
041100081222     c                   leave
041200081222     c                   enddo
041300081229     c                   if        cn01mod = ' '
041400081222     c                   commit
041500081222     c                   end
041600930825     C*
041700081219     C                   ENDSR
041800930811     C*
041900930812     C*--- PULIZIA SUBFILE -------------------------------------------*
042000930812     C     PULSF         BEGSR
042100011109     C*---------------------------------------------------------------*
042200930915     C                   SETON                                        87
042300020124     C                   WRITE     CN10C02
042400930915     C                   SETOFF                                       87
042500930812     C                   ENDSR
042600930812     C*
042700930812     C*--- CARICAMENTO SUBFILE ---------------------------------------*
042800930812     C     CARSF         BEGSR
042900011109     C*---------------------------------------------------------------*
043000930812     C                   Z-ADD     0             NRR               4 0
043100930812     C*
043200081219     C     KtGT          setll     fitgt000
043300081219     c                   do        *hival
043400081219     C     KtGT          READE     fitgt000
043500930812     C*
043600081219     C                   if        %eof(fitgt01l)
043700081219     c                   leave
043800081219     c                   end
043900011126      *seleziono solo la divisa immessa a video se c'è
044000011126     c                   if        viddiv <> *blank and
044100081219     c                             tgtdiv <> viddiv
044200081219     c                   iter
044300011126     c                   end
044400930812     C*
044500081219     C     tGTATB        IFEQ      ' '
044600930820     C*
044700930820     C* CAMPO SELEZIONE
044800930819     C                   MOVEL     ' '           VI2SCE
044900930903     C*
045000081219     C                   MOVEL     tGTPRG        VI2PRG
045100081219     C                   MOVEL     tGTdiv        VI2div
045200930819     C*
045300930819     C* GIRO DATA DECORRENZA TARIFFA
045400081219     C                   Z-ADD     TGTDDT        DATA
045500940906     C                   Z-ADD     MM            GGMM              4 0
045600940906     C                   MOVEL     GG            GGMM
045700940906     C                   Z-ADD     AA            VI2DDT
045800940906     C                   MOVEL     GGMM          VI2DDT
045900930819     C* GIRO DATA SCADENZA TARIFFA
046000081219     C                   Z-ADD     TGTDST        DATA
046100940906     C                   Z-ADD     MM            GGMM
046200940906     C                   MOVEL     GG            GGMM
046300940906     C                   Z-ADD     AA            VI2DST
046400940906     C                   MOVEL     GGMM          VI2DST
046500090323     C* GIRO DATA convalida
046600090323     C                   Z-ADD     TGTdts        DATA
046700090320     C                   Z-ADD     MM            GGMM              4 0
046800090320     C                   MOVEL     GG            GGMM
046900090323     C                   Z-ADD     AA            VI2Dts
047000090323     C                   MOVEL     GGMM          VI2Dts
047100090323     C* GIRO DATA stampa sede
047200090323     C                   Z-ADD     TGTdcn        DATA
047300090323     C                   Z-ADD     MM            GGMM              4 0
047400090323     C                   MOVEL     GG            GGMM
047500090323     C                   Z-ADD     AA            VI2Dcn
047600090323     C                   MOVEL     GGMM          VI2Dcn
047700090323     C* GIRO DATA ricezione
047800090323     C                   Z-ADD     TGTdrc        DATA
047900090323     C                   Z-ADD     MM            GGMM              4 0
048000090323     C                   MOVEL     GG            GGMM
048100090323     C                   Z-ADD     AA            VI2Drc
048200090323     C                   MOVEL     GGMM          VI2Drc
048300930819     C* GIRO DATA ULTIMA VARIAZIONE
048400081219     C                   Z-ADD     TGTDUV        DATA
048500940906     C                   Z-ADD     MM            GGMM
048600940906     C                   MOVEL     GG            GGMM
048700940906     C                   Z-ADD     AA            VI2DUV
048800940906     C                   MOVEL     GGMM          VI2DUV
048900930812     C*
049000930812     C                   ADD       1             NRR
049100020124     C                   WRITE     CN10S02
049200930812     C                   ENDIF
049300930812     C                   ENDDO
049400090427     c                   eval      savnrr=nrr
049500930820     C*
049600930812     C                   ENDSR
049700020125     C*--- OPERAZIONI INIZIALI ---------------------------------------*
049800020125     C     OPERAZ        BEGSR
049900020125     C*---------------------------------------------------------------*
050000020125     C* TESTATA PRG
050100020125     C*
050200020125     C* ALF1 = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
050300020125     C                   MOVEL     '1'           ALF1              1
050400081219     C                   MOVEL     'EUR'         viddiv
050500020125     C*
050600020125     C***
050700020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
050800020125     C***
050900020125     C                   TIME                    W0120            14 0
051000020125     C                   MOVE      W0120         WDAT              8 0
051100020125     C*
051200020125     C                   Z-ADD     WDAT          G02DAT
051300020125     C                   MOVEL     *BLANK        G02ERR
051400020125     C                   CALL      'XSRDA8'
051500020125     C                   PARM                    WLBDAT
051600020125     C*
051700020125     C* PULISCO CODICE PADRONCINO E DESCRIZIONE SOLO A INIZIO PGM
051800020125     C*   IN MODO CHE NELLA PRIMA VIDEATA RESTI IMPOSTATO L'ULTIMO
051900020125     C                   MOVEL     *BLANKS       VIDPDR
052000020125     C                   MOVE      '?'           VIDPDR
052100020125     C                   MOVEL     *BLANKS       DESPDR
052200020125     C                   ENDSR
052243120724ab   C**************************************************************************
052244120724ab    * Controlla se nel periodo successivo è prevista un'altra società
052245120724ab   C**************************************************************************
052246120724ab   C     TEST_SOCieta  begSR
052247120725ab   c                   MOVE      dtaiso        dtVALIDITA
052248120724ab    *
052249120724ab    /FREE
052250120724ab
052251120724EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( vidfil
052252120724EDPDC                                             : dtVALIDITA
052253120724EDPDC                                             : 'O'
052254120724EDPDC                                             : valDatIniz
052255120724EDPDC                                             : valDatFine
052256120724ab                                                );
052257120724ab
052258120724EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
052259120724EDPDC    // Gestire l'errore.
052260120724ab     ENDIF;
052261120724ab
052262120724ab    /END-FREE
052263120724     c                   move      valdatfine    valdatfineW       8 0
052264120724ab   C                   ENDSR
052300020125      *--------------------------------------------------
052400020125     c     *inzsr        begsr
052500020125      *--------------------------------------------------
052600020125
052700020125     C     *ENTRY        PLIST
052800020125     C                   PARM                    KPJBA
052900081219     c                   movel     kpjbu         ficn01ds
053000020125     C                   Z-ADD     1             CODUT
053100020125     C                   CALL      'X§PARUT'
053200020125     C                   PARM                    UT§DSE
053300020125     C                   MOVEL     RAGUT         RSUT             20
053400020125     C                   MOVEL     REC80         CNCR80
053500090108     c*annullamento solo per edp
053600101209     c                   if        %subst(knmus: 1: 3) = 'EDP'
053700101209     c                   seton                                        44
053800101209     c                   end
053900100907     c* Si è voluto permettere a TUTTI di cancellare le tariffe.
054000100907     c*   NON + solo a EDP !!!!
054100101209     c*                  seton                                        44
054200081219     C* ACCESSO fitgt
054300081219     C     KTGT          KLIST
054400020125     C                   KFLD                    COMPDR
054500081219     C                   KFLD                    cn01SML
054600021203     C* ACCESSO fiapd
054700021203     C     Kapdc         KLIST
054800081219     C                   KFLD                    apdtip
054900021203     C                   KFLD                    compdr
055000081219     c                   eval      apdtip = 'A'
055100020125     C* DEFINIZIONE CAMPI
055200081219     C     *LIKE         DEFINE    TGTPDR        COMPDR
055300081219     C***
055400081219     C* VEDO SE SONO SIMFEL O REMOTO
055500081219     C***
055600081219     C                   EVAL      VidFil = SimPOU
057600081219     C* 19 ON  - UTILIZZARE TARIFFE     DI SIMULAZIONE
057700081219     C* 19 OFF - UTILIZZARE TARIFFE NON DI SIMULAZIONE
057800081219     C     cn01sml       IFEQ      'S'
057900081219     C                   SETON                                        19
058000081219     C                   ENDIF
058100081219     C*
058200081229     c     cn01mod       comp      ' '                                1010
058300090423     c   10              eval      vidtes = ' Interrogazione tariffe Aut Città '
058400090423     c  n10              eval      vidtes = '    Gestione tariffe AUT Città    '
058500020125     C*
058501120724ab   C*     Inizializza
058502120724ab    /FREE
058503120724ab     TibsSof_Init();
058504120724ab    /END-FREE
058600020125     c                   endsr
