000100081222     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200120724     H BNDDIR('TIBS') EXTBININT(*YES)
000300081218     H* ficn01R *----------------------------------------------------*
000400000000     H*-----------------*                                            *
000500081218     H*               GESTIONE SUPERTESTA TARIFFE PADRONCINI         *
000600000000     H*--------------------------------------------------------------*
000700081218     Fficn01D   CF   E             WORKSTN
000800030121     F                                     SFILE(CN10S02:NRR)
000900081218     Ffitgt01L  iF   E           K DISK
001000021203     Ffiapd01L  IF   E           K DISK
001100930923     D*
001200940920     D* PARAMETRO PASSATO PER ACCEDERE ALLE TARIFFE SIMULAZIONE
001300940920     D*   DIRETTAMENTE DA MENU'
001400131008     d tibs42ds      e ds
001500131008     d uteautds      e ds
001600081219     d ficn01ds      e ds
001700081219     d ficn02ds      e ds
001800090112     d ficn06ds      e ds
001900921102     D WLBDAT          DS
002000940906     D  G02DAT                 1      8  0
002100940906     D  G02INV                 9     16  0
002200940906     D  G02ERR                17     17
002300940906     D  G02TGI                18     22  0
002400940504     D                 DS
002500940906     D  AA                     1      4  0
002600940906     D  MM                     5      6  0
002700940906     D  GG                     7      8  0
002800940906     D  DATA                   1      8  0
002900081219     D*
003000000000     D KPJBA         E DS
003100090323     c* stampa prova allegato
003200090323     dparam            ds
003300090323     D lancio                         1
003400090323     d codpadr                        7s 0
003500090323     D codpadr2                       7s 0
003600090323     d alladat                        8s 0
003700090504     d pardtcerta                     8s 0
003800090504     d parmNRC                        7s 0
003900090504     d parmFIL                        3s 0
004000090504     D OUTQOVR                       10
004100090504     D errore                         1
004200091021     D simulata                       1
004300090504
004400930914     D*
004500900320     D CNCR80        E DS
004600021203     D fnlv24ds      E DS
004700900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800931115     D  TCU                  398    697
004900931115     D                                     DIM(50)                              TAB FLAG CNT
005000931115     D  KCU                  698    847P 0
005100931115     D                                     DIM(50)                              TAB CAPO CONTO
005200931115     D                                     PACKEVEN
005300931115     D  DTU                  848    895P 0
005400931115     D                                     DIM(12)                              DATE PARAM
005500931115     D                                     PACKEVEN
005600160921     D si              s              1
005700090427     d savnrr          s                   like(nrr)
005800090427     d dtaiso          s               d
005900090323     d dtaeur          s               d   datfmt(*eur)
006000120724ab   ** Prototipi.
006100120724ab    **
006200120724ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
006300120724ab    /DEFINE DFTACTGRP_NO
006400120724ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
006500120724ab    /UNDEFINE DFTACTGRP_NO
006600120724      *
006700120724ab   D TibsSof_esito...
006800120724ab   D                 S             10I 0 IMPORT
006900120724ab   D idSocieta...
007000120724EDPDCD                 S              3A
007100120724ab   **
007200120724EDPDCd dtVALIDITA      s               D
007300120724EDPDCd VALdatFINE      s               D
007400120724EDPDCd VALdatINIZ      s               D
007500930811     C*****************************************************************
007600930928     C* ESEGUO OPERAZIONI INIZIALI
007700930928     C                   EXSR      OPERAZ
007800930903     C*
007900930914     C* ALF1 = 'F' ===> VADO A FINE PGM
008000930811     C     ALF1          DOWNE     'F'
008100930824     C*
008200081219     C* ALF1 = '1' ===> scelta codice autista
008300930811     C     ALF1          IFEQ      '1'
008400930811     C                   EXSR      VIDEO1
008500930811     C                   ENDIF
008600930811     C*
008700081219     C* ALF1 = '2' ===> GESTIONE VIDEATA SUBFILE supertestata
008800930811     C     ALF1          IFEQ      '2'
008900930811     C                   EXSR      VIDEO2
009000930811     C                   ENDIF
009100020125     C*
009200930811     C                   ENDDO
009300940930     C*
009400930820     C                   SETON                                        LR
009500930928     C*
009600930811     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
009700930811     C     VIDEO1        BEGSR
009800011109     C*---------------------------------------------------------------*
009900090428     c                   clear                   msgv
010000930907     C*
010100081219     C                   do        *hival
010200020124     C                   EXFMT     CN10D01
010300930929     C*
010400930811     C** CMD3  - FINE LAVORO
010500931013     C   KC              MOVEL     'F'           ALF1
010600081219     C   KC              leave
010700931013     C*
010800940928     C** CMD7  - GESTIONE ANAGRAFICA PADRONCINI
010900930907     C     *INKG         IFEQ      *ON
011000131022     C                   Z-ADD     VIDFIL        d24FIL
011100021203     C                   Z-ADD     0             d24pdr
011200030204     C                   MOVEL     'R'           d24FLG
011300081219     C                   MOVEL     'A'           d24tip
011400021203     C                   MOVEL     *BLANKS       d24rsc
011500021203     C                   MOVEL(p)  fnlv24ds      KPJBU
011600940926     C                   CALL      'FNLV24R'
011700930811     C                   PARM                    KPJBA
011800030204     C                   MOVEL     KPJBU         fnlv24ds
011900030204     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
012000030204     C     d24pdr        IFNE      0
012100030204     C                   MOVEL     d24pdr        VIDFIL
012200030204     C                   MOVEL     *ZEROS        VIDPDR
012300030204     C                   MOVE      d24pdr        VIDPDR
012400030204     C                   MOVEL     d24rsc        DESPDR
012500030204     C                   ENDIF
012600930907     C*
012700081219     C                   iter
012800930811     C                   ENDIF
012900930811     C*
013000930907     C** CMD14 - GESTIONE ANAGRAFICA FORNITORI
013100011126     c                   if        *inkn
013200011126     c                   movel(p)  'P'           kpjbu
013300011126     C                   CALL      'TRUL16R'
013400930907     C                   PARM                    KPJBA
013500081219     C                   iter
013600011126     c                   end
013700940919     C*
013800930811     C** CONTROLLI DELLA PRIMA VIDEATA
013900930907     C                   EXSR      CONTR1
014000081219     C   90              iter
014100930811     C*
014200930907     C                   MOVEL     '2'           ALF1
014300081219     c                   leave
014400081219     c                   enddo
014500081219     C                   ENDSR
014600930811     C*
014700930811     C*--- CONTROLLI DELLA PRIMA VIDEATA -----------------------------*
014800930811     C     CONTR1        BEGSR
014900011109     C*---------------------------------------------------------------*
015000081219     C                   SETOFF                                       408390
015100930811     C*
015200940920     C****  CODICE FILIALE  ****
015300950113     C* FILIALE OBBLIGATORIA
015400030529     C     VIDFIL        IFEQ      0
015500950113     C                   SETON                                        8390
015600081219     C                   leavesr
015700950113     C                   ENDIF
015800950113     C*
015900131022     C* contyrollo autorizzazione
016000131008     c                   clear                   tibs42ds
016100131008     c                   movel     vidfil        i42pge
016200131008     c                   call      'TIBS42R'
016300131008     c                   parm                    tibs42ds
016400131008     c                   movel     o42uni        uteautds
016500131022     c                   if        o42err <> ' ' or §AUTMTC <> 'S'
016600090109     c                   seton                                        8390
016700090109     c                   leavesr
016800090109     c                   endif
016900940920     C*
017000930811     C****  CODICE PADRONCINO  ****
017100930908     C     '?'           SCAN      VIDPDR                                 90
017200930811     C*
017300930811     C     *IN90         IFEQ      *ON
017400021203     C                   Z-ADD     VIDFIL        d24FIL
017500021203     C                   Z-ADD     0             d24pdr
017600021203     C                   MOVEL     'R'           d24FLG
017700081219     C                   MOVEL     'A'           d24tip
017800021203     C                   MOVEL     *BLANKS       d24rsc
017900021203     C                   MOVEL(p)  fnlv24ds      KPJBU
018000940926     C                   CALL      'FNLV24R'
018100930811     C                   PARM                    KPJBA
018200021203     C                   MOVEL     KPJBU         fnlv24ds
018300941006     C*
018400941006     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
018500021203     C     d24pdr        IFNE      0
018600021203     C                   MOVEL     d24pdr        VIDFIL
018700930921     C                   MOVEL     *ZEROS        VIDPDR
018800021203     C                   MOVE      d24pdr        VIDPDR
018900021203     C                   MOVEL     d24rsc        DESPDR
019000941006     C                   ENDIF
019100941006     C*
019200081219     C                   leavesr
019300930811     C                   ENDIF
019400930811     C*
019500930921     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
019600930921     C                   TESTN                   VIDPDR               35
019700930921     C  N35              SETON                                        4090
019800081219     C  N35              leavesr
019900931111     C*
020000940915     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
020100940920     C                   MOVEL     VIDFIL        COMPDR
020200940920     C                   MOVE      VIDPDR        COMPDR
020300081219     C                   MOVE      compdr        vidaut
020400090604     c* errore annullato solo per gestione
020500021203     C     kapdc         CHAIN     fiapd000                           32
020600090604     c                   if        not *in32 and not *in10
020700090604     C     APDATB        IFNE      ' '
020800940914     C                   SETON                                        32
020900940914     C                   ENDIF
021000090604     C                   ENDIF
021100940914     C* CODICE PADRONCINO ERRATO
021200940914     C   32              SETON                                        4090
021300081219     C   32              leavesr
021400100204      *
021500100204     c*  se escluso da autofatturazione dare msg di errore
021600100204     c                   if        apdpdd = 'S'
021700100204     C                   SETON                                        4090
021800100204     C                   leavesr
021900100204     c                   end
022000100204      *
022100090706     C                   MOVEL     APDRSf        DESPDR
022200930908     C*
022300081219     C                   ENDSR
022400930811     C*
022500930811     C*--- GESTIONE DELLA SECONDA VIDEATA ----------------------------*
022600930811     C     VIDEO2        BEGSR
022700011109     C*---------------------------------------------------------------*
022800081219     C* P U L I Z I A   S U B F I L E
022900081219     C                   EXSR      PULSF
023000020410     C* CARICAMENTO SUBFILE
023100081218     C                   EXSR      CARSF
023200081219     c                   if        nrr = 0
023300081219     c                   seton                                        87
023400081222     c                   else
023500081222     c                   setoff                                       87
023600081219     c                   end
023700930820     C*
023800020205     c                   do        *hival
023900081219     C                   Z-ADD     1             REC
024000020125     C                   WRITE     CN10Z02
024100020124     C                   EXFMT     CN10C02
024200930820     C*
024300930820     C** CMD12 - RITORNO
024400930914     C     *INKL         IFEQ      *ON
024500930914     C                   SETOFF                                       11
024600930914     C                   MOVEL     '1'           ALF1
024700081219     C                   leave
024800930914     C                   ENDIF
024900930820     C** CMD9  - AGGIUNTA TARIFFA
025000930914     C     *INKI         IFEQ      *ON
025100090828     c                   exfmt     LRF6W01
025200090828     c                   if        *inkl
025300090828     c                   iter
025400090828     c                   end
025500081219     C                   clear                   ficn02ds
025600081219     C                   Z-ADD     COMPDR        cn02pdr                        *PDR VECCHIO
025700081219     C                   MOVEL     cn01SML       cn02sml                        *SML VECCHIO
025800081222     C                   MOVEL     '1'           cn02mod                        *SML VECCHIO
025900081219     C                   MOVEL     VIDdiv        cn02div                        *CTR NUOVO
026000081219     C                   z-add     0             cn02prg                        *CTR NUOVO
026100081219     C                   MOVEL(p)  ficn02ds      KPJBU
026200081219     C                   CALL      'FICN02R'
026300930825     C                   PARM                    KPJBA
026400081219     C                   MOVEL     KPJBU         ficn02ds
026500090423     c                   if        cn02err <> ' '
026600090423     c                   movel     cn02msg       msgv
026700090423     c                   else
026800090423     c                   clear                   msgv
026900090423     c                   end
027000081222     C                   leave
027100930823     C                   ENDIF
027200090429     c                   if        nrr = 0
027300090429     c                   iter
027400090429     c                   end
027500081219     C* CONTROLLO SE E' STATA SCELTA UNA TARIFFA
027600081222     c                   do        *hival        xyz               3 0
027700081222     c     xyz           chain     cn10s02                            56
027800081222     c                   if        *in56
027900081222     c                   leave
028000081222     c                   end
028100930825     C*
028200081219     c                   if        vi2sce <> ' '
028300081219     C                   clear                   ficn02ds
028400081219     C                   Z-ADD     COMPDR        cn02pdr                        *PDR VECCHIO
028500081219     C                   MOVEL     cn01SML       cn02sml                        *SML VECCHIO
028600081219     C                   MOVEL     VI2div        cn02div                        *CTR NUOVO
028700081219     C                   z-add     VI2PRG        cn02PRG
028800081219     C                   MOVEL     vi2sce        cn02mod                        *SML VECCHIO
028900081223     C                   MOVEL     'A'           cn02ana                        *SML VECCHIO
029000081219     c                   select
029100081219     c                   when      vi2sce = '1'
029200081229     C                   MOVEL     cn01mod       cn02mod                        *SML VECCHIO
029300081229     C                   MOVEL(p)  ficn02ds      KPJBU
029400081219     C                   CALL      'FICN10R'
029500081219     C                   PARM                    KPJBA
029600090427     C                   MOVEL     KPJBU         ficn02ds
029700090325     c                   if        cn02err <> ' '
029800090325     c                   movel     cn02msg       msgv
029900090325     c                   else
030000090325     c                   clear                   msgv
030100090325     c                   end
030200090108     c* annullamento solo per edp
030300090108     c                   when      (vi2sce = '2' and not *in10) or
030400090108     c                             vi2sce = '5' or
030500090427     c                             (vi2sce = '4' and *in44 and not *in10)
030600090427     c* faccio annullare solo se non ho progressivi successivi
030700090427     c                   if        vi2sce = '4' and xyz <> savnrr
030800090427     c                   eval      msgv = 'Tariffa non annullabile esistono -
030900090427     c                             progressivi successivi'
031000090427     c                   else
031100081229     C                   MOVEL(p)  ficn02ds      KPJBU
031200081219     C                   CALL      'FICN02R'
031300081219     C                   PARM                    KPJBA
031400090427     C                   MOVEL     KPJBU         ficn02ds
031500090325     c                   if        cn02err <> ' '
031600090325     c                   movel     cn02msg       msgv
031700090325     c                   else
031800090325     c                   clear                   msgv
031900090325     c                   end
032000090427     c                   end
032100090112     c* copia
032200090112     c                   when      (vi2sce = '3' and not *in10)
032300090112     C                   clear                   ficn06ds
032400090112     C                   movel     COMPDR        filold                         *PDR VECCHIO
032500090112     C                   move      COMPDR        pdrold                         *PDR VECCHIO
032600090112     C                   MOVEL     cn01SML       smlold                         *SML VECCHIO
032700090112     C                   z-add     VI2PRG        PRGold
032800090112     C                   MOVEL(p)  ficn06ds      KPJBU
032900090112     C                   CALL      'FICN85C'
033000090112     C                   PARM                    KPJBA
033100100416     c*
033200090324     c* conferma la tariffa no simulazione
033300090324     c                   when      vi2sce = 'C' and not *in10 and not *in19
033400120724      *controllo congruenza società
033500120724     C                   move      vi2DDT        dtaeur
033600120724     C                   move      dtaeur        dtaiso
033700120725ab   C                   exsr      TEST_societa
033800120724ab   c                   if        apdcsf <> idsocieta
033900120724     c                   eval      msgv = 'Tariffa non Convalidabile -
034000120724     c                             società fornitore non congruente -
034100120724     c                             alla società operativa'
034200120724     c                   leavesr
034300120724ab   C                   end
034400120724ab   c                   if        tgtdst > valdatfinew
034500120724     c                   eval      msgv = 'Scadenza tariffa maggiore -
034600120724     c                             di data scadenza della società -
034700120724     c                             operativa'
034800120724     c                   leavesr
034900120724ab   C                   end
035000160921     c                   clear                   si
035100160922     C/EXEC SQL
035200160922     C+  SELECT '1' into :si
035300160922     C+   FROM fiadt00f WHERE adtatb = ' ' and adtpdr =:tgtpdr and
035400160922     C+   adtcsf = :apdcsf and adtcdf = :tgtcdf and adtddt= :tgtddt
035500160922     C+  FETCH first row only
035600160922     C/END-EXEC
035700160922     c                   if        si = '1'
035800160922     c                   eval      msgv = 'Tariffa non Convalidabile -
035900160922     c                             esiste tariffa analoga in aff/def'
036000160922     c                   leavesr
036100160922     c                   endif
036200100416      *
036300100416      * PARAMETRI DI CONTROLLO
036400100416     c                   CLEAR                   IN_Simula         1
036500100416     c     VIDFIL        MULT      10000         IN_Autista        7 0
036600100416     c                   move      VIDPDR        IN_Autista
036700100416     c                   Z-ADD     VI2PRG        IN_Progress       3 0
036800100416     c                   CLEAR                   OUT_TServ         1
036900100416     c                   CLEAR                   OUT_Tariffa       3 0
037000100416     c                   CLEAR                   OUT_CodDetta      3
037100100416     c                   CLEAR                   OUT_DesCodice    25
037200100416     c                   CLEAR                   OUT_Errore       50
037300100416     c*
037400160922     c* Controlla se ci sono problemi sulle tariffe prima di confermarle
037500100416     C                   CALL      'FICN11R1'
037600100416     c                   parm                    IN_Autista
037700100416     c                   parm                    IN_Simula
037800100416     c                   parm                    IN_Progress
037900100416     c                   parm                    OUT_TServ
038000100416     c                   parm                    OUT_Tariffa
038100100416     c                   parm                    OUT_CodDetta
038200100416     c                   parm                    OUT_DesCodice
038300100416     c                   parm                    OUT_Errore
038400100416      ****
038500100416     c                   IF          OUT_Errore <> *BLANKS
038600100416     c*
038700100416     c                   if        OUT_TServ = 'C'
038800100416     c                   eval      msgv = 'Tar.Consegna: codice '
038900100416     c                             + OUT_CodDetta
039000100416     c                   elseIf    OUT_TServ = 'R'
039100100416     c                   eval      msgv = 'Tar.Ritiro  : codice '
039200100416     c                             + OUT_CodDetta
039300100416     c                   elseIf    OUT_TServ = 'G'
039400100416     c                   eval      msgv = 'Tar.Giornata: codice '
039500100416     c                             + OUT_CodDetta
039600100416     c                   end
039700100416     c*
039800100416     c                   eval      msgv =  %Trim(msgv) + '. ' + OUT_Errore
039900100416     c                   Else
040000100416      *
040100100416      * errore se tariffa già convalidata
040200090508     c                   if        VI2DTS <>0
040300090508     c                   eval      msgv = 'Tariffa già convalidata'
040400090508     c                   else
040500090428     c* non faccio confermare la tariffa se aut escluso da autofatt.
040600090428     c                   if        apdpdd ='S'
040700090428     c                   eval      msgv = 'Tariffa non convalidabile, AUT esclu-
040800090428     c                             so da autofatturazione'
040900090428     c                   else
041000090323     C                   MOVEL(p)  ficn02ds      KPJBU
041100090323     C                   CALL      'FICN07R'
041200090323     C                   PARM                    KPJBA
041300090325     c                   movel     kpjbu         ficn02ds
041400090325     c                   if        cn02err <> ' '
041500090325     c                   movel     cn02msg       msgv
041600090325     c                   else
041700090325     c                   clear                   msgv
041800090325     c                   end
041900090428     c                   end
042000090508     c                   end
042100100416     c                   ENDif
042200090323     c* stampa di prova allegati
042300090323     c                   when      vi2sce = 'S'
042400090504     c                   clear                   param
042500090504     c                   eval      lancio = 'P'
042600090323     c                   eval      codpadr = vidaut
042700090323     c                   eval      codpadr2= vidaut
042800090323     C                   move      vi2DDT        dtaeur
042900090323     C                   move      dtaeur        dtaiso
043000090323     C                   MOVE      dtaiso        alladat
043100091021     c   19              move      'S'           simulata
043200090421     c                   movel(p)  param         kpjbu
043300090504     C                   CALL      'FICN14R1'
043400090323     C                   PARM                    KPJBA
043500081219     c                   endsl
043600081222     c                   clear                   vi2sce
043700081222     c                   update    cn10s02
043800081219     C                   MOVEL     KPJBU         ficn02ds
043900081219     c                   end
044000020205     c                   enddo
044100081222     c                   leave
044200081222     c                   enddo
044300081229     c                   if        cn01mod = ' '
044400081222     c                   commit
044500081222     c                   end
044600930825     C*
044700081219     C                   ENDSR
044800930811     C*
044900930812     C*--- PULIZIA SUBFILE -------------------------------------------*
045000930812     C     PULSF         BEGSR
045100011109     C*---------------------------------------------------------------*
045200930915     C                   SETON                                        87
045300020124     C                   WRITE     CN10C02
045400930915     C                   SETOFF                                       87
045500930812     C                   ENDSR
045600930812     C*
045700930812     C*--- CARICAMENTO SUBFILE ---------------------------------------*
045800930812     C     CARSF         BEGSR
045900011109     C*---------------------------------------------------------------*
046000930812     C                   Z-ADD     0             NRR               4 0
046100930812     C*
046200081219     C     KtGT          setll     fitgt000
046300081219     c                   do        *hival
046400081219     C     KtGT          READE     fitgt000
046500930812     C*
046600081219     C                   if        %eof(fitgt01l)
046700081219     c                   leave
046800081219     c                   end
046900011126      *seleziono solo la divisa immessa a video se c'è
047000011126     c                   if        viddiv <> *blank and
047100081219     c                             tgtdiv <> viddiv
047200081219     c                   iter
047300011126     c                   end
047400930812     C*
047500081219     C     tGTATB        IFEQ      ' '
047600930820     C*
047700930820     C* CAMPO SELEZIONE
047800930819     C                   MOVEL     ' '           VI2SCE
047900930903     C*
048000081219     C                   MOVEL     tGTPRG        VI2PRG
048100081219     C                   MOVEL     tGTdiv        VI2div
048200930819     C*
048300930819     C* GIRO DATA DECORRENZA TARIFFA
048400081219     C                   Z-ADD     TGTDDT        DATA
048500940906     C                   Z-ADD     MM            GGMM              4 0
048600940906     C                   MOVEL     GG            GGMM
048700940906     C                   Z-ADD     AA            VI2DDT
048800940906     C                   MOVEL     GGMM          VI2DDT
048900930819     C* GIRO DATA SCADENZA TARIFFA
049000081219     C                   Z-ADD     TGTDST        DATA
049100940906     C                   Z-ADD     MM            GGMM
049200940906     C                   MOVEL     GG            GGMM
049300940906     C                   Z-ADD     AA            VI2DST
049400940906     C                   MOVEL     GGMM          VI2DST
049500090323     C* GIRO DATA convalida
049600090323     C                   Z-ADD     TGTdts        DATA
049700090320     C                   Z-ADD     MM            GGMM              4 0
049800090320     C                   MOVEL     GG            GGMM
049900090323     C                   Z-ADD     AA            VI2Dts
050000090323     C                   MOVEL     GGMM          VI2Dts
050100090323     C* GIRO DATA stampa sede
050200090323     C                   Z-ADD     TGTdcn        DATA
050300090323     C                   Z-ADD     MM            GGMM              4 0
050400090323     C                   MOVEL     GG            GGMM
050500090323     C                   Z-ADD     AA            VI2Dcn
050600090323     C                   MOVEL     GGMM          VI2Dcn
050700090323     C* GIRO DATA ricezione
050800090323     C                   Z-ADD     TGTdrc        DATA
050900090323     C                   Z-ADD     MM            GGMM              4 0
051000090323     C                   MOVEL     GG            GGMM
051100090323     C                   Z-ADD     AA            VI2Drc
051200090323     C                   MOVEL     GGMM          VI2Drc
051300930819     C* GIRO DATA ULTIMA VARIAZIONE
051400081219     C                   Z-ADD     TGTDUV        DATA
051500940906     C                   Z-ADD     MM            GGMM
051600940906     C                   MOVEL     GG            GGMM
051700940906     C                   Z-ADD     AA            VI2DUV
051800940906     C                   MOVEL     GGMM          VI2DUV
051900930812     C*
052000930812     C                   ADD       1             NRR
052100020124     C                   WRITE     CN10S02
052200930812     C                   ENDIF
052300930812     C                   ENDDO
052400090427     c                   eval      savnrr=nrr
052500930820     C*
052600930812     C                   ENDSR
052700020125     C*--- OPERAZIONI INIZIALI ---------------------------------------*
052800020125     C     OPERAZ        BEGSR
052900020125     C*---------------------------------------------------------------*
053000020125     C* TESTATA PRG
053100020125     C*
053200020125     C* ALF1 = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
053300020125     C                   MOVEL     '1'           ALF1              1
053400081219     C                   MOVEL     'EUR'         viddiv
053500020125     C*
053600020125     C***
053700020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
053800020125     C***
053900020125     C                   TIME                    W0120            14 0
054000020125     C                   MOVE      W0120         WDAT              8 0
054100020125     C*
054200020125     C                   Z-ADD     WDAT          G02DAT
054300020125     C                   MOVEL     *BLANK        G02ERR
054400020125     C                   CALL      'XSRDA8'
054500020125     C                   PARM                    WLBDAT
054600020125     C*
054700020125     C* PULISCO CODICE PADRONCINO E DESCRIZIONE SOLO A INIZIO PGM
054800020125     C*   IN MODO CHE NELLA PRIMA VIDEATA RESTI IMPOSTATO L'ULTIMO
054900020125     C                   MOVEL     *BLANKS       VIDPDR
055000020125     C                   MOVE      '?'           VIDPDR
055100020125     C                   MOVEL     *BLANKS       DESPDR
055200020125     C                   ENDSR
055300120724ab   C**************************************************************************
055400120724ab    * Controlla se nel periodo successivo è prevista un'altra società
055500120724ab   C**************************************************************************
055600120724ab   C     TEST_SOCieta  begSR
055700120725ab   c                   MOVE      dtaiso        dtVALIDITA
055800120724ab    *
055900120724ab    /FREE
056000120724ab
056100120724EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( vidfil
056200120724EDPDC                                             : dtVALIDITA
056300120724EDPDC                                             : 'O'
056400120724EDPDC                                             : valDatIniz
056500120724EDPDC                                             : valDatFine
056600120724ab                                                );
056700120724ab
056800120724EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
056900120724EDPDC    // Gestire l'errore.
057000120724ab     ENDIF;
057100120724ab
057200120724ab    /END-FREE
057300120724     c                   move      valdatfine    valdatfineW       8 0
057400120724ab   C                   ENDSR
057500020125      *--------------------------------------------------
057600020125     c     *inzsr        begsr
057700020125      *--------------------------------------------------
057800020125
057900020125     C     *ENTRY        PLIST
058000020125     C                   PARM                    KPJBA
058100081219     c                   movel     kpjbu         ficn01ds
058200020125     C                   Z-ADD     1             CODUT
058300020125     C                   CALL      'X§PARUT'
058400020125     C                   PARM                    UT§DSE
058500020125     C                   MOVEL     RAGUT         RSUT             20
058600020125     C                   MOVEL     REC80         CNCR80
058700090108     c*annullamento solo per edp
058800101209     c                   if        %subst(knmus: 1: 3) = 'EDP'
058900101209     c                   seton                                        44
059000101209     c                   end
059100100907     c* Si è voluto permettere a TUTTI di cancellare le tariffe.
059200100907     c*   NON + solo a EDP !!!!
059300101209     c*                  seton                                        44
059400081219     C* ACCESSO fitgt
059500081219     C     KTGT          KLIST
059600020125     C                   KFLD                    COMPDR
059700081219     C                   KFLD                    cn01SML
059800021203     C* ACCESSO fiapd
059900021203     C     Kapdc         KLIST
060000081219     C                   KFLD                    apdtip
060100021203     C                   KFLD                    compdr
060200081219     c                   eval      apdtip = 'A'
060300020125     C* DEFINIZIONE CAMPI
060400081219     C     *LIKE         DEFINE    TGTPDR        COMPDR
060500081219     C***
060600081219     C* VEDO SE SONO SIMFEL O REMOTO
060700081219     C***
060800081219     C                   EVAL      VidFil = SimPOU
060900081219     C* 19 ON  - UTILIZZARE TARIFFE     DI SIMULAZIONE
061000081219     C* 19 OFF - UTILIZZARE TARIFFE NON DI SIMULAZIONE
061100081219     C     cn01sml       IFEQ      'S'
061200081219     C                   SETON                                        19
061300081219     C                   ENDIF
061400081219     C*
061500081229     c     cn01mod       comp      ' '                                1010
061600090423     c   10              eval      vidtes = ' Interrogazione tariffe Aut Città '
061700090423     c  n10              eval      vidtes = '    Gestione tariffe AUT Città    '
061800020125     C*
061900120724ab   C*     Inizializza
062000120724ab    /FREE
062100120724ab     TibsSof_Init();
062200120724ab    /END-FREE
062300020125     c                   endsr
