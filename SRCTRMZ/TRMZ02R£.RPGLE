000100011123     H DFTACTGRP(*NO) BNDDIR('PJXBND') ACTGRP('QILE')
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011121     F*----------------------------------------------------*
000400000000     F*          GESTIONE ANAGRAFICA AUTOMEZZI             *
000500000000     F*----------------------------------------------------*
000600000000     FTABEL00F  IF   E           K DISK
000700011123     Fansif01L  IF   E           K DISK
000800011123     Fansog01L  IF   E           K DISK
000900011123     Fanfrn01L  IF   E           K DISK
001000011122     Fanent01l  IF   E           K DISK
001100910215     FAZORG01L  IF   E           K DISK
001200070926     FmzAMh01L  IF   E           K DISK
001300011121     FmzAME01L  UF A E           K DISK
001400011121     FmzAME02L  IF   E           K DISK
001500011122     F                                     RENAME(mzAME000:mzAME1)
001600011121     FmzAME03L  IF   E           K DISK
001700011122     F                                     RENAME(mzAME000:mzAME3)
001800011218     FmzAME04L  IF   E           K DISK
001900011218     F                                     RENAME(mzAME000:mzAME4)
002000031028     FmzAME05L  IF   E           K DISK
002100031028     F                                     RENAME(mzAME000:mzAME5)
002200011121     Ftrmz02D   CF   E             WORKSTN
002300011122     D  dataiso        s               d
002400011122     D  dataeur        s               d   datfmt(*eur)
002500011123     D  filll          s                   like(frnfiliale) inz
002600011122     D*STORICO
002700000000     D PARAM2          DS
002800000000     D  FLAG                   1      1
002900000000     D  NRF                    2      8  0
003000000000     D  TGA                    9     10
003100000000     D  TGN                   11     19
003200011129     D  CAU                   20     21
003300011129     D  CSP                   22     23
003400011129     D  FIL                   24     26
003500011129     D  DAT                   27     34  0
003600020607     D  SOC                   35     37
003700030115     D  cdc                   38     45
003800040211     D  tele                  46     55  0
003900910215     D                 DS
004000910215     D  VIMM                   1      4  0
004100910215     D  MES                    1      2  0
004200910215     D  ANN                    3      4  0
004300000000     D DSTAR           DS
004400000000     D  VDCTA                  1     25
004500000000     D  DESFLA                26     26
004600000000     D  DATFIS                27     28
004700011122     D* Dati di ambiente ottenuti da XSOC
004800011122     D Ana032ds      E DS                  INZ
004900011122    >D xsoc001ds     E DS
005000011123     D ana007ds      e ds                  inz
005100011126     D Ana201DS      E DS
005200160516      *------------------------
005300160516     D tibs02ds      E DS
005400011207     D dsxh          E DS
005500011220     D dsxa          E DS
005600020111     D dsxd          E DS
005700050216     D dsxn          E DS
005800160516     D dmau          E DS
005900160516     D dcau          E DS
006000011122     D* DS Interna per dati di output di XSOC
006100011122     D XSOCDS          DS          1000
006200000000     D KPJBA         E DS
006300000000     D  LIBSYS                92    100
006400000000     D  MSGERR               453    502
006500000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
006600000000     D  TCU                  398    697
006700000000     D                                     DIM(50)                              TAB FLAG CNT
006800000000     D  KCU                  698    847P 0
006900000000     D                                     DIM(50)                              TAB CAPO CONTO
007000000000     D                                     PACKEVEN
007100000000     D  DTU                  848    895P 0
007200000000     D                                     DIM(12)                              DATE PARAM
007300000000     D                                     PACKEVEN
007400031028     d com20           s                   like(ametel) inz
007500000000      * 10 TESTN FILIALE
007600000000      * 11 TESTN CODICE TARIFFA
007700011122      * 22 generico
007800000000      * 40 DUP TASSE GOVERNATIVE
007900011122      * 42 errore cod. fornitore
008000000000      * 43 DUP INCENDIO E FURTO
008100011122      * 44 errore cod. consumi
008200000000      * 45 CODICE UTILIZZO
008300020607      * 60 codice società in dotazione
008400020607      * 61 FILIALE CENTRO DI COSTO
008500000000      * 62 PORTATA O CAVLLI FISCALI
008600000000      * 63 CODICE TARIFFA
008700000000      * 64 PORT.PIENO CARICO
008800000000      * 66 CONTROLLO GOMME
008900000000      * 67     "      "
009000000000      * 68     "      "
009100000000      * 69     "      "
009200000000      * 70 N.TELAIO
009300000000      * 71 IMMISSIONE
009400000000      * 72 TARGA
009500000000      * 73 NUMERO ASS.C/TERZI
009600000000      * 74 COMPAGNIA ASS.RC AUTO
009700000000      * 75 COMPAGNIA ASS.INCENDIO/FURTO
009800000000      * 77 DATA ULT.REVISIONE
009900000000      * 78 CODICE MANUTENZIONE
010000000000      * 79 ANNO
010100000000      * 80 TIPO
010200000000      * 81 MARCA
010300000000      * 82 TARGA
010400000000      * 83 CODICE STATO
010500000000      * 84 COSTANTE AGGIORNAMENTO
010600000000      * 85 CODICE CATEGORIA
010700000000      * 86 CODICE CARROZZERIA
010800000000      * 87 CODICE DITTA
010900000000      * 88 CODICE FILIALE
011000000000      * 89 TASSA DI CIRCOLAZIONE
011100000000      * 92 DATA ASS.RC AUTO
011200000000      * 93 DATA ASS. INCENDIO/FURTO
011300000000      * 95 DATA ASS.  C/TERZI
011400000000      * 96 SCADENZA
011500000000      * 97 TASSA PUBBLICITA'
011600000000      * 99 INDICATORE GENERICO DI ERRORE
011700000000     C*----------------------------------------------------*
011800011122     c                   if        not *inlr
011900011123     C                   EXSR      CLR01
012000011121     C                   do        *hival
012100011122     c* primo video
012200011123     c                   EXSR      CLR00
012300011123     c                   if        *in99
012400011123     c                   setoff                                       99
012500011123     c                   write     ga02d001
012600011123     c                   seton                                        99
012700011123     c                   end
012800000000     C                   EXFMT     GA02D001
012900011121     C   Kc              leave
013000011121     C* CMD06 INSERIMENTO NUOVO CODICE
013100011121     C   Kf              EXSR      NOVCOD
013200011219     c                   if        *inkd
013300011219     c                   z-add     0             naut
013400011219     C                   EXSR      ric01
013500011219     c                   end
013600011121     c* controlli
013700011121     C                   EXSR      ctr01
013800011122     C   Kc              leave
013900011122     c   99              iter
014000011122     c* Altri formati
014100011122     C                   EXSR      fmt02
014200011122     C   Kc              leave
014300011122     c   99              iter
014400011122     c                   enddo
014500000000     C                   SETON                                        LR
014600011122     c                   end
014700011122     c**********************************************************************
014800011122     c* Gestione secondo formato
014900011122     c**********************************************************************
015000011122     C     fmt02         BEGSR
015100011122     c*
015200011123     c                   z-add     0             §rig
015300011123     c                   z-add     0             §col
015400011122     C                   EXSR      LEGGI
015500011122     C  n99              do        *hival
015600011122     c* secondo video
015700011123     c                   EXSR      CLR00
015800011123     c                   if        *in99
015900011123     c                   setoff                                       99
016000011123     c                   write     ga02d002
016100011123     c                   seton                                        99
016200011123     c                   end
016300011221     c                   if        *in20
016400011221     c                   write     ga02d002
016500011221     c                   EXFMT     PROTECT1
016600011221     C                   ELSE
016700011122     C                   EXFMT     GA02D002
016800011221     C                   END
016900011122     C* FINE LAVORO
017000011122     C                   if        *inkc or *inkl
017100011126     C   kl              EXSR      CLR01
017200011122     C                   leave
017300011122     c                   end
017400011123     C* ricerca
017500011123     C                   if        *inkd
017600011123     c                   exsr      ric02
017700011123     C                   iter
017800011123     c                   end
017900011122     C* CONTROLLI 2° VIDEATA
018000011122     C                   EXSR      ctr02
018100011122     C   99              iter
018200011122     C* storico
018300011122     C                   if        *inki
018400011122     c                   exsr      srstor
018500011122     C     FLAG          IFEQ      'A'
018600011129     c                   exsr      clr01
018700011122     C                   leave
018800011122     C                   ELSE
018900011122     C                   iter
019000011122     C                   END
019100011122     C                   END
019200011122     c* TASSE E ASS.NE
019300011220     C                   if        *inkh or *in71
019400011122     c                   exsr      fmt03
019500011220     C                   IF        *INKC OR *INKL
019600011220     C   kl              EXSR      CLR01
019700011220     c                   leave
019800011122     c                   end
019900011220     c                   end
020000011122     C* AGGIORNAMENTO FILE
020100011126     c                   if        *inkF
020200011122     C                   EXSR      AGGIOR
020300011122     C                   leave
020400011122     c                   end
020500011122     c                   enddo
020600011122     c*
020700011122     C                   ENDSR
020800011122     c**********************************************************************
020900011122     C* terzo formato
021000011122     c**********************************************************************
021100011122     C     fmt03         BEGSR
021200011122     c*
021300011123     c                   z-add     0             §rig
021400011123     c                   z-add     0             §col
021500011122     c                   do        *hival
021600011122     c*terzo formato
021700011123     c                   EXSR      CLR00
021800011123     c                   if        *in99
021900011123     c                   setoff                                       99
022000011123     c                   write     ga02d003
022100011123     c                   seton                                        99
022200011123     c                   end
022300011221     c                   if        *in20
022400011221     c                   write     ga02d003
022500011221     c                   EXFMT     PROTECT2
022600011221     C                   ELSE
022700011122     C                   EXFMT     GA02D003
022800011221     C                   END
022900011122     C* FINE LAVORO
023000011126     C                   if        *inkc
023100011122     C                   leave
023200011122     c                   end
023300011123     C* ricerca
023400011123     C                   if        *inkd
023500011123     c                   exsr      ric03
023600011123     C                   iter
023700011123     c                   end
023800011220     C* VIDEO PRECEDENTE
023900011220     C                   if        *inkl
024000011220     C                   EXSR      CLR03
024100011220     C                   leave
024200011220     c                   end
024300011122     C* CONTROLLI 3 VIDEATA
024400011122     C                   EXSR      ctr03
024500011122     c   99              iter
024600011122     C* AGGIORNAMENTO FILE
024700011126     C   kF              leave
024800011122     c                   enddo
024900011122     c*
025000011122     C                   ENDSR
025100011123     C**********************************************************************
025200011123     C** LETTURA X CHIAVE N.RIFERIMENTO
025300011123     C**********************************************************************
025400011123     C     LEGGI         BEGSR
025500011123     C                   SETOFF                                       842171
025600070926     c                   setoff                                       50
025700011123     c                   do
025800011218     C* LETTURA X AUTISTA
025900011218     c                   z-add     0             naut
026000011218     C     VCAU          IFNE      *BLANKS
026100011218     C     VNRF          andeq     0
026200011218     C     vcau          setll     mzAME4
026300011218     c                   do        *hival
026400011218     c     vcau          reade     mzame04l                               21
026500011218     c   21              leave
026600011218     c                   add       1             naut              2 0
026700011218     c                   if        naut > 1
026800011218     c                   eval      §fld = 'VCAU'
026900011218     c                   exsr      ric01
027000011219     c* non è stata fatta alcuna scelta
027100011219     c                   if        cmd = 'L'
027200011219     c                   seton                                        99
027300011219     c                   end
027400011218     c                   leave
027500011218     c                   end
027600011218     c                   enddo
027700011219     c* non trovato automezzo con l'autista scelto
027800011218     c                   if        *in21 and naut = 0
027900011218     C                   SETON                                        5999
028000011218     C                   leave
028100011218     C                   END
028200011219     c                   if        naut <> 0
028300011219     c                   if        not *in99
028400011219     c                   setoff                                       21
028500011219     c                   else
028600011219     c                   leave
028700011219     c                   end
028800011219     c                   end
028900011219     C                   END
029000011123     C* LETTURA X N.RIFERIMENTO
029100011123     C     VNRF          IFNE      0
029200011123     C     KGAAM         CHAIN     mzAME000                           21
029300070926     c                   if        *in21
029400070926     C     KGAAM         CHAIN     mzAMh000                           21
029500070926     c                   if        *in21
029600070926     C                   SETON                                        71
029700070926     c                   else
029800070926     c                   seton                                        5099
029900011123     C                   END
030000070926     C                   END
030100070926     C                   END
030200011123     C* LETTURA X N. DI TARGA
030300011123     C     VTGA          IFNE      *BLANKS
030400011123     C     VTGN          ANDNE     *BLANKS
030500011123     C     KTAR          CHAIN     mzAME1                             21
030600011123     C   21              SETON                                        7299
030700011123     C   21              leave
030800011123     C                   END
030900011123     C* LETTURA X N.TELAIO
031000011123     C     VTLA          IFNE      *BLANKS
031100011123     C     VTLA          CHAIN     mzAME3                             21
031200011123     C   21              SETON                                        7099
031300011123     C   21              leave
031400011123     C                   END
031500011221     C* SE POSSO SOLO VISUALIZZARE NON EMMETTO LA VIDEATA DI IMMISSIONE
031600011221     C                   IF        *IN20 AND *IN71
031700011221     C                   SETON                                        99
031800011221     C                   LEAVE
031900011221     C                   END
032000011221     C*
032100011123     c                   exsr      clr02
032200011123     C                   if        not *in21
032300011123     C                   SETON                                        84
032400011123     C                   Z-ADD     AMENRF        VNRF
032500011218     C                   MOVEL     AMETLA        VTLA
032600011123     C                   MOVEL     AMESTA        VSTA
032700011123     C*RICERCA SULLA TABELLA 'XF' LA DESCRIZIONE
032800011123     C*DELLO STATO AUTOMEZZO
032900011123     c                   if        vsta <> *blanks
033000011126     c                   exsr      srvSTA
033100011123     c                   else
033200011123     C                   MOVEL     *blanks       VDsta
033300011123     C                   END
033400011123     C                   MOVEL     AMETGA        VTGA
033500011123     C                   MOVEL     AMETGN        VTGN
033600011123     C                   MOVEL     AMECAT        VCAT
033700011123     C*RICERCA SULLA TABELLA 'XA' LA DESCRIZIONE
033800011123     C*DELLA CATEGORIA AUTOMEZZO
033900011126     c                   exsr      srvCAT
034000160516     C*                  MOVEL     AMEMAR        VMAR
034100160516     C*                  MOVEL     AMETIP        VTIP
034200160516     C                   MOVEL     AMEmau        VMAu
034300160517     c                   if        vmau <> *blank
034400160517     c                   clear                   tibs02ds
034500160517     c                   move      'C'           t02mod
034600160517     c                   clear                   vmaud
034700160517     c                   exsr      srvmau
034800160517     c                   endif
034900160516     C                   MOVEL     AMEcaut       Vcaut
035000160517     c                   if        vcaut <> *blank
035100160517     c                   clear                   tibs02ds
035200160517     c                   move      'C'           t02mod
035300160517     c                   clear                   vcautd
035400160517     c                   exsr      srvcaut
035500160517     c                   endif
035600011123RE   C                   MOVEL     AMECIL        VCIL
035700011123     C                   MOVEL     AMECAR        VCAR
035800011123     C*RICERCA SULLA TABELLA 'XB' LA DESCRIZIONE
035900011123     C*DEL TIPO CAROZZERIA
036000011123     c                   if        vcar <> *blanks
036100011126     c                   exsr      srvCAR
036200011123     C                   else
036300011123     C                   MOVEL     *blanks       VDcar
036400011123     C                   END
036500011123     C                   Z-ADD     AMEIMM        VIMM
036600011123     C                   Z-ADD     MES           ANTO              2 0
036700011123     C                   Z-ADD     ANN           MES
036800011123     C                   Z-ADD     ANTO          ANN
036900011123     C                   MOVEL     AMEMAN        VMAN
037000011123     C*RICERCA SULLA TABELLA 'XC' LA DESCRIZIONE
037100011123     C*DEL TIPO DI MANUTENZIONE
037200011126     c                   exsr      srvMAN
037300011123     C                   MOVEL     AMEUTI        VUTI
037400011123     C*RICERCA SULLA TABELLA 'XL' LA DESCRIZIONE
037500011123     C*DEL TIPO DI UTILIZZO
037600011123     c                   if        vuti <> *blanks
037700011126     c                   exsr      srvuti
037800011123     C                   else
037900011123     C                   MOVEL     *blanks       VDes
038000011123     C                   END
038100011123     C                   Z-ADD     AMEASS        VASS
038200011123     C                   movel     AMEusi        Vcon
038300011123     C*RICERCA SULLA TABELLA 'XH' LA DESCRIZIONE
038400011123     C*DEL COD. CONSUMI
038500011126     c                   exsr      srvCON
038600011123     C                   MOVEL     AMEALI        VALI
038700011123     C                   Z-ADD     AMETAR        VTAR
038800011123     C                   Z-ADD     AMEPPC        VPPC
038900011123     C                   Z-ADD     AMEPTR        VPTR
039000011123     C                   Z-ADD     AMEPQL        VPQL
039100011123     C                   Z-ADD     AMENGM        VNGM
039200011123     C                   MOVEL     AMETG1        VTG1
039300011123     C                   MOVEL     AMETG2        VTG2
039400011123     C                   MOVEL     AMETG3        VTG3
039500011123     C                   movel     AMEDIT        VDIT
039600011219     C*RICERCA LA DESCRIZIONE
039700011219     C*DELLA DITTA
039800020607     c                   exsr      srvdit
039900020607     C                   movel     AMEsoc        Vsoc
040000020607     C*RICERCA LA DESCRIZIONE
040100020607     C*DELLA DITTA
040200020607     c                   exsr      srvsoc
040300011123     C                   MOVEL     AMEFIL        VFIL
040400011123     C                   MOVEL     AMECDC        VCDC
040500011123     C                   MOVEL     AMEfor        Vfor
040600031029     c                   clear                   telev
040700031029     c                   if        ametel <> *blanks
040800031029     C                   MOVEl     AMEtel        telev
040900031029     c                   end
041000011126     C*RICERCA LA DESCRIZIONE
041100011123     C*DEL fornitore
041200011126     c                   if        vfor <> *blanks
041300011126     C                   EXSR      SRVFOR
041400011126     c                   end
041500011123     C*RICERCA SU AZORG LA DESCRIZIONE
041600011123     C*DELLA FILIALE IN DOTAZIONE
041700011126     C                   EXSR      SRVFIL
041800011123     C*RICERCA SU ANENT LA DESCRIZIONE
041900011123     C*DELLA FILIALE CDC
042000011126     C                   EXSR      SRVCDC
042100011220     c                   if        amedur <> 0
042200011123     C                   MOVE      AMEDUR        dataiso
042300011123     C                   move      dataiso       DATAeur
042400011123     C                   move      DATAeur       VDUR
042500011220     c                   else
042600011220     c                   z-add     0             vdur
042700011220     c                   end
042800011220     c                   if        amedsp <> 0
042900011123     C                   move      AMEDSP        dataiso
043000011123     C                   move      dataiso       dataeur
043100011123     C                   move      DATAeur       VSCAD
043200011220     c                   else
043300011220     c                   z-add     0             vscad
043400011220     c                   end
043500011123     C*RICERCA SULLA TABELLA 'XN' LA DESCRIZIONE
043600011123     C*DELL' AUTISTA AUTOMEZZO
043700011123     C                   MOVEL     AMECAU        VCAU
043800011126     C                   EXSR      SRVCAU
043900011220     c                   if        amedal <> 0
044000011123     C                   move      AMEDAL        DATAIso
044100011123     C                   move      dataiso       dataeur
044200011123     C                   move      DATAeur       dataut
044300011220     c                   else
044400011220     c                   z-add     0             dataut
044500011220     C                   END
044600011123     C*RICERCA SULLA TABELLA 'XR' LA DESCRIZIONE
044700011123     C*DEL TIPO PROPRIETA'
044800011123     C                   MOVEL     AMECSP        VCSP
044900011126     C                   EXSR      SRVCSP
045000011123     C* 3 VIDEATA
045100011123     C*TASSA DI CIRCOLAZIONE
045200011123     c                   if        amedtc <> 0
045300011123     C                   move      AMEDtc        DATAIso
045400011123     C                   move      dataiso       dataeur
045500011123     C                   move      DATAeur       vdtc
045600011123     c                   else
045700011123     c                   z-add     0             vdtc
045800011123     c                   end
045900011123     C                   Z-ADD     AMEIMT        VIMT
046000011123     C*TASSA PUBBLICITA'
046100011123     c                   if        amedtp <> 0
046200011123     C                   move      AMEDtp        DATAIso
046300011123     C                   move      dataiso       dataeur
046400011123     C                   move      DATAeur       vdtp
046500011123     c                   else
046600011123     c                   z-add     0             vdtp
046700011123     c                   end
046800011123     C                   Z-ADD     AMEIMP        VIMP
046900011123     C*SCADENZA ASS.RCA
047000011123     c                   if        amedrc <> 0
047100011123     C                   move      AMEDrc        DATAIso
047200011123     C                   move      dataiso       dataeur
047300011123     C                   move      DATAeur       vdrc
047400011123     c                   else
047500011123     c                   z-add     0             vdrc
047600011123     c                   end
047700011123     C                   MOVEL     AMECRC        VCRC
047800011123     C*RICERCA SULLA TABELLA 'XD' LA DESCRIZIONE
047900011123     C*DELLA COMP.ASS.RCA
048000011123     c                   if        vcrc <> *blanks
048100011126     C                   EXSR      SRVCRC
048200011123     C                   else
048300011123     C                   MOVEL     *blanks       VDcrc
048400011123     C                   END
048500031127     C*descrizione equipaggiamento
048600031127     C                   MOVEL     AMEequ        Vequi
048700031127     c                   if        vequi<> *blanks
048800031127     C                   EXSR      SRVequi
048900031127     C                   else
049000031128     C                   MOVEL     *blanks       VDequ1
049100031128     C                   MOVEL     *blanks       VDequ2
049200031127     C                   END
049300011123     c                   if        amedfi <> 0
049400011123     C                   move      AMEDfi        DATAIso
049500011123     C                   move      dataiso       dataeur
049600011123     C                   move      DATAeur       vdfi
049700011123     c                   else
049800011123     c                   z-add     0             vdfi
049900011123     c                   end
050000011123     C                   MOVEL     AMECFI        VCFI
050100011123     C                   Z-ADD     AMEIMA        VIMA
050200011123     C*RICERCA SULLA TABELLA 'XD' LA DESCRIZIONE
050300011123     C*DELLA COMP.ASS.INCENDIO/FURTO
050400011123     c                   if        vcfi <> *blanks
050500011126     C                   EXSR      SRVCFI
050600011123     C                   else
050700011123     C                   MOVEL     *blanks       VDcfi
050800011123     C                   END
050900011123     C                   Z-ADD     AMEIMF        VIMF
051000011123     C                   MOVEL     AMENAC        VNAC
051100011123     c                   if        amedac <> 0
051200011123     C                   move      AMEDac        DATAIso
051300011123     C                   move      dataiso       dataeur
051400011123     C                   move      DATAeur       vdac
051500011123     c                   else
051600011123     c                   z-add     0             vdac
051700011123     c                   end
051800011123     c                   if        amedac <> 0
051900011123     C                   move      AMEsac        DATAIso
052000011123     C                   move      dataiso       dataeur
052100011123     C                   move      DATAeur       vsac
052200011123     c                   else
052300011123     c                   z-add     0             vsac
052400011123     c                   end
052500011123     C                   Z-ADD     AMEIMK        VIMK
052600011123     C                   MOVEL     AMETTA        VTTA
052700011123     C                   MOVE      AMECTA        VCTA
052800011123     C*RICERCA SULLA TABELLA 'XI' LA DESCRIZIONE
052900011123     C*DELLA CODICE TARIFFA
053000011123     c                   if        vcta <> 0
053100011126     C                   EXSR      SRVCTA
053200011123     C                   else
053300011126     C                   MOVEL     *blanks       dstar
053400011123     C                   END
053500011123     C                   Z-ADD     AMECVF        VCVF
053600011123     C                   MOVEL     AMEPBL        VPBL
053700011123     C                   ENDif
053800011123     C                   enddo
053900011123     c                   if        *in99
054000011123     c                   exsr      clr01
054100011123     C                   END
054200011123     C                   ENDSR
054300011126     c**********************************************************************
054400011126     C* STORICO
054500011126     c**********************************************************************
054600011126     C     srstor        BEGSR
054700011126     c*
054800011122     C     *HIVAL        SETLL     mzAME01L
054900011122     C                   MOVEL     *BLANKS       FLAG
055000011122     C                   Z-ADD     VNRF          NRF
055100011122     C                   MOVEL     VTGA          TGA
055200011122     C                   MOVEL     VTGN          TGN
055300011122     C                   MOVEL     VCAU          CAU
055400011122     C                   MOVEL     VCSP          CSP
055500011122     C                   MOVEL     VFIL          FIL
055600040213     C                   MOVEl     telev         tele
055700031028     C                   MOVE      DATAUT        DAT
055800020607     C                   MOVEL     VSOC          SOC
055900030115     C                   MOVEL     Vcdc          cdc
056000030115     C                   MOVEL(p)  PARAM2        KPJBU
056100011129     C                   CALL      'TRMZ02R5'
056200011122     C                   PARM                    KPJBA
056300011122     C                   MOVEL     KPJBU         PARAM2
056400011122     C                   MOVEL     TGA           VTGA
056500011122     C                   MOVEL     TGN           VTGN
056600011122     C                   MOVEL     CAU           VCAU
056700011122     C                   MOVEL     CSP           VCSP
056800011129     C                   exsr      srvcsp
056900011122     C                   MOVEL     FIL           VFIL
057000011129     C                   exsr      srvfil
057100020607     C                   MOVEL     SOC           VSOC
057200020607     C                   exsr      srvSOC
057300030115     C                   MOVEL     cdc           Vcdc
057400030115     C                   exsr      srvcdc
057500011122     C     FLAG          IFEQ      'T'
057600011122     C                   MOVE      DAT           DATAUT
057700011129     C                   exsr      srvcau
057800011122     C                   END
057900040213     C                   MOVEl     tele          telev
058000011122     c*
058100011122     C                   ENDSR
058200011121     c**********************************************************************
058300011121     C* IMMISSIONE NUOVO CODICE
058400011121     c**********************************************************************
058500000000     C     NOVCOD        BEGSR
058600011122     C     *HIVAL        SETLL     mzAME000
058700011122     C                   READP     mzAME000                               22
058800011122     c                   if        not *in22
058900000000     C                   Z-ADD     AMENRF        VNRF
059000000000     C                   ADD       1             VNRF
059100011122     c                   end
059200000000     C                   ENDSR
059300011123     c**********************************************************************
059400011123     c* controlli 1° formato
059500011123     c**********************************************************************
059600011123     C     ctr01         BEGSR
059700011123     C                   SETOFF                                       99
059800011123     c                   do
059900011123     C     VNRF          IFNE      0
060000011123     C     VTGA          ANDNE     *BLANKS
060100011123     C                   SETON                                        99
060200011123     C                   LEAVE
060300011123     C                   END
060400011123     C     VTGA          IFNE      *BLANKS
060500011123     C     VTLA          ANDNE     *BLANKS
060600011123     C                   SETON                                        99
060700011123     C                   LEAVE
060800011123     C                   END
060900011123     C     VNRF          IFNE      0
061000011123     C     VTLA          ANDNE     *BLANKS
061100011123     C                   SETON                                        99
061200011123     C                   LEAVE
061300011123     C                   END
061400011123     C     VNRF          IFEQ      0
061500011123     C     VTGA          andeq     *BLANKS
061600011123     C     VTLA          andeq     *BLANKS
061700011218     C     Vcau          andeq     *BLANKS
061800011123     C                   SETON                                        99
061900011123     C                   LEAVE
062000011123     C                   END
062100011123     C                   ENDdo
062200011123     C                   ENDSR
062300011123     c**********************************************************************
062400011123     c* ricerca 1° videata
062500011123     c**********************************************************************
062600011123     C     ric01         BEGSR
062700011123     C*
062800011123     C                   SELECT
062900011221     C                   WHEN      §FLD = 'VTGN' OR §FLD = 'VTGA'
063000011221     C                   CALL      'TRMZ02R4'    RICTAR
063100011219     C                   WHEN      §FLD = 'VTLA'
063200011219     C                   CALL      'TRMZ42R'
063300011219     c                   parm                    vtla
063400011219     c                   parm                    cmd
063500011218     C                   WHEN      §FLD = 'VCAU'
063600011218     c                   if        naut > 1
063700011218     c                   move      ' '           ordina
063800011218     c                   else
063900011218     c                   move      '1'           ordina
064000011218     c                   movel     *blanks       vcau
064100011218     c                   end
064200011218     c                   z-add     0             vnrf
064300011218     C                   CALL      'TRMZ41R'
064400011218     c                   parm                    ordina            1
064500011218     c                   parm                    vcau
064600011218     c                   parm                    vnrf
064700011218     c                   parm                    cmd               1
064800011219     c                   if        cmd = 'L'
064900011219     c                   clear                   vnrf
065000011123     C                   END
065100011219     C                   END
065200011123     C* POSIZIONAMENTO CURSORE
065300011123     C                   Z-ADD     §RIGA         §RIG
065400011123     C                   Z-ADD     §COLON        §COL
065500011123     C                   ENDSR
065600011122     c**********************************************************************
065700000000     C** CONTROLLI 2 VIDEATA
065800011122     c**********************************************************************
065900011122     C     ctr02         BEGSR
066000011122     C                   SETOFF                                       99
066100011123     c                   movel     ' '           fld               1
066200011122     c                   do
066300020607     C*CODICE società proprietaria
066400020607     C     VDIT          IFEQ      *blanks
066500020607     C                   SETON                                        8799
066600020607     c                   movel     *blanks       vddit
066700020607     C                   leave
066800020607     C                   ELSE
066900020607     c                   exsr      srvdit
067000020607     C                   END
067100000000     C*CODICE FILIALE
067200011122     C                   MOVEL     *blanks       ORGDES
067300011122     C     VFIL          IFEQ      *BLANKS
067400011122     C                   SETON                                        8899
067500011122     C                   leave
067600011122     C                   else
067700000000     C                   TESTN                   VFIL                 10
067800011122     C                   if        not *in10
067900000000     C                   SETON                                        8899
068000011122     C                   leave
068100011122     C                   else
068200011126     c                   exsr      srvfil
068300011126     C   99              leave
068400000000     C                   END
068500011122     C                   END
068600020607     C*CODICE società in dotazione
068700020607     C     Vsoc          IFEQ      *blanks
068800020607     C                   SETON                                        6099
068900020607     c                   movel     *blanks       vdsoc
069000020607     C                   leave
069100020607     C                   ELSE
069200020607     c                   exsr      srvsoc
069300020607     C                   END
069400000000     C*CODICE CDC
069500011126     c                   exsr      srvcdc
069600011126     C   99              leave
069700000000     C*TARGA
069800000000     C     VTGA          IFEQ      *BLANKS
069900000000     C                   SETON                                        8299
070000011122     C                   leave
070100000000     C                   END
070200000000     C     VTGN          IFEQ      *BLANKS
070300000000     C                   SETON                                        8299
070400011122     C                   leave
070500000000     C                   END
070600011122     C*TELAIO
070700011122     C     VTLA          IFEQ      *BLANKS
070800011122     C                   SETON                                        7099
070900011122     C                   leave
071000011122     C                   END
071100000000     C*MARCA
071200160516     C     Vcaut         IFEQ      *BLANKS
071300000000     C                   SETON                                        8199
071400011122     C                   leave
071500160516     C                   ELSE
071600160516     C                   clear                   tibs02ds
071700160516     c                   move      'C'           t02mod
071800160516     c                   clear                   vcautd
071900160516     c                   exsr      srvcaut
072000160516     C   99              leave
072100000000     C                   END
072200000000     C*TIPO
072300160516     C     Vmau          IFEQ      *BLANKS
072400000000     C                   SETON                                        8099
072500011122     C                   leave
072600160516     C                   ELSE
072700160516     c                   clear                   tibs02ds
072800160516     c                   move      'C'           t02mod
072900160516     c                   clear                   vmaud
073000160516     c                   exsr      srvmau
073100160516     C   99              leave
073200000000     C                   END
073300000000     C*CATEGORIA
073400011126     c                   exsr      srvcat
073500011126     C   99              leave
073600031106     C*Telepass
073700031106     c                   if        not *in84
073800031106     C                   IF        telev = 0 and §xatel = 'S'
073900031106     C                   SETON                                        1499
074000031106     C                   leave
074100031106     c                   end
074200040416     c                   if        telev <> 9999999999 and telev <> 0
074300031106     c                   movel     telev         com20
074400031106     c     com20         setll     mzame05l                               14
074500031106     c                   if        *in14
074600031106     C                   SETON                                        99
074700031106     C                   leave
074800031106     C                   END
074900031106     C                   END
075000031106     C                   END
075100910215     C*MESE/ANNO IMMATRICOLAZIONE
075200910215     C     MES           IFEQ      0
075300910215     C     MES           ORGT      12
075400000000     C                   SETON                                        7999
075500011122     C                   leave
075600000000     C                   END
075700910204     C*CODICE AUTISTA
075800011126     C                   EXSR      SRVCAU
075900011126     C   99              leave
076000910215     C* DATA ASSEGNAZIONE AUTISTA
076100011220     C                   IF        dataut = 0 and §xadau = 'S'
076200910215     C                   SETON                                        5899
076300011122     C                   leave
076400011220     C                   End
076500011220     C                   IF        dataut <> 0
076600011123     C     *EUR          test(d)                 dataut                 58
076700011122     c                   if        *in58
076800011122     C                   SETON                                        99
076900011122     C                   leave
077000910215     C                   END
077100910215     C                   END
077200011122     C*FORNITORE
077300011123     c                   if        vfor <> *blanks
077400011126     c                   exsr      srvfor
077500011123     c                   end
077600000000     C*CARROZZERIA
077700011123     c                   if        vcar <> *blanks
077800011126     C                   EXSR      srvcar
077900011126     C   99              leave
078000011122     C                   END
078100011123     C*alimentazione
078200011123     c                   if        vali = *blanks
078300011123     c                   seton                                        9099
078400011123     c                   leave
078500011123     c                   end
078600011123     C*COD.CONSUMI
078700011126     c                   exsr      srvcon
078800011126     C   99              leave
078900910204     C*CODICE SIT. PROPRIETA'
079000011122     C                   TESTN                   VCsp                 10
079100011122     C                   if        not *in10
079200011122     C                   SETON                                        5699
079300011122     C                   leave
079400011122     C                   END
079500011126     c                   exsr      srvcsp
079600011126     C   99              leave
079700910214     C* DATA SCAD. PROPRIETA'
079800011220     C                   IF        vscad = 0 and §xadsc = 'S'
079900011122     C                   SETON                                        5799
080000011122     C                   leave
080100011220     C                   End
080200011220     C     vscad         IFne      0
080300011123     C     *EUR          test(d)                 vscad                  57
080400011122     c                   if        *in57
080500011122     C                   SETON                                        99
080600011122     C                   leave
080700011122     C                   END
080800011122     C                   END
080900000000     C*TARA + PORT.UTILE = PORTATA PIENO CARICO
081000000000     C     VTAR          ADD       VPQL          COMPPC            3 0
081100000000     C     COMPPC        IFNE      VPPC
081200000000     C                   SETON                                        6499
081300011122     C                   leave
081400000000     C                   END
081500000000     C*TIPO GOMME
081600011123     c                   movel     'XG'          parcod
081700011123     c                   if        vtg1 <> *blanks
081800011122     c                   movel(P)  VTG1          parkey
081900011122     c                   exsr      srtab
082000011122     c                   movel     parkey        vtg1
082100011123     c                   if        *in99
082200011122     C                   SETON                                        66
082300011122     C                   leave
082400011122     C                   END
082500011122     C                   END
082600011123     c                   if        vtg2 <> *blanks
082700011122     c                   movel(P)  VTG2          parkey
082800011122     c                   exsr      srtab
082900011122     c                   movel     parkey        vtg2
083000011123     c                   if        *in99
083100011122     C                   SETON                                        67
083200011122     C                   leave
083300011122     C                   END
083400011122     C                   END
083500011123     c                   if        vtg3 <> *blanks
083600011122     c                   movel(P)  VTG3          parkey
083700011122     c                   exsr      srtab
083800011122     c                   movel     parkey        vtg3
083900011123     c                   if        *in99
084000011122     C                   SETON                                        69
084100011122     C                   leave
084200011122     C                   END
084300011122     C                   END
084400011220     C* DATA ultima REVISIONE
084500011220     C                   IF        vdur = 0 and §xadur = 'S'
084600011122     C                   SETON                                        7799
084700011122     C                   leave
084800011220     C                   End
084900011220     c                   if        vdur <> 0
085000011123     C     *EUR          test(d)                 vdur                   77
085100011122     c                   if        *in77
085200011122     C                   SETON                                        99
085300011122     C                   leave
085400011122     C                   END
085500011122     C                   END
085600000000     C*TIPO MANUTENZIONE
085700011126     c                   exsr      srvman
085800011126     C   99              leave
085900000000     C*COD.UTILIZZO
086000011123     c                   if        vuti <> *blanks
086100011126     c                   exsr      srvUTI
086200011126     C   99              leave
086300011122     C                   END
086400000000     C*COD.STATO
086500011123     c                   if        vsta <> *blanks
086600011126     c                   exsr      srvSTA
086700011126     C   99              leave
086800011122     C                   END
086900070925     c* se la società in dotazione è diversa dalla società dell'autista
087000070925     c* richiamo il pgm di manutenzione anagrafico autista
087100070925     c                   if        vsoc <> §xnsoc and §xnsoc <> ' '
087200070925     c                             and vsoc <> savsoc
087300070925     c                   movel(p)  'XN'          kpjbu
087400070925     c                   move      vcau          kpjbu
087500070925     c                   call      'TRMZ40R'
087600070925     c                   parm                    kpjba
087700070925     c                   movel     vsoc          savsoc            3
087800070925     c                   end
087900011122     C                   enddo
088000011122     c*
088100000000     C                   ENDSR
088200011126     c**********************************************************************
088300011126     C** ricerca  2 VIDEATA
088400011126     c**********************************************************************
088500011126     C     ric02         BEGSR
088600011123     c                   movel     '1'           fld
088700011123     c                   select
088800020607     C*CODICE società proprietaria
088900011123     C                   when      §fld = 'VDIT'
089000011123     C                   reset                   ana007ds
089100011123     C                   eval      opz007 = '01'
089200011123     C                   eval      kpjbu = ana007ds
089300011123     C                   call      'ANA007R'
089400011123     C                   parm                    kpjba
089500011123     C                   eval      ana007ds = kpjbu
089600011123     C                   if        err007 <> '1' and soc007 <> *blanks
089700011123     C                   eval      vdit = soc007
089800011219     c                   exsr      srvdit
089900011123     C                   endif
090000020607     C*CODICE FILIALE
090100020607     C                   when      §fld = 'VFIL'
090200020607     C                   MOVEL     *BLANKS       §COD1
090300020607     C                   MOVE      *BLANKS       §DES1
090400020607     C                   CALL      'TNSD19R'
090500020607     C                   PARM                    §COD1             3
090600020607     C                   PARM                    §TIP              1
090700020607     C                   PARM                    §DES1            25
090800020607     c                   if        §cod1 <> *blanks
090900020607     C                   MOVEL     §COD1         VFIL                           *CODICE
091000020607     C                   MOVEL     §DES1         VDFIL                          *DESCRIZIONE
091100020607     c                   end
091200020607     C*CODICE società in dotazione
091300020607     C                   when      §fld = 'VSOC'
091400020607     C                   reset                   ana007ds
091500020607     C                   eval      opz007 = '01'
091600020607     C                   eval      kpjbu = ana007ds
091700020607     C                   call      'ANA007R'
091800020607     C                   parm                    kpjba
091900020607     C                   eval      ana007ds = kpjbu
092000020607     C                   if        err007 <> '1' and soc007 <> *blanks
092100020607     C                   eval      vsoc = soc007
092200020607     c                   exsr      srvsoc
092300020607     C                   endif
092400011123     C*CODICE CDC
092500011123     C                   when      §fld = 'VCDC'
092600011123    >C                   RESET                   ANA032DS
092700011123    >C                   MOVEL     '01'          OPZ032
092800011123     C                   MOVE      'S'           FSC032
092900011123     C                   MOVE      'CDC'         TPE032
093000011123    >C                   MOVEL     ANA032Ds      KPJBU
093100011123    >C                   CALL      'ANA032R'
093200011123     C                   PARM                    KPJBA
093300011123    >C                   MOVEL     KPJBU         ANA032DS
093400011212     c                   if        ent032 <> *blanks
093500011123     C                   MOVEL     ENT032        VCDC
093600011212     c                   end
093700160516      *marca
093800160516     c                   when      §fld = 'VCAUT'
093900160516     c                   clear                   tibs02ds
094000160516     c                   move      'R'           t02mod
094100160516     c                   exsr      srvcaut
094200160516      *modello
094300160516     c                   when      §fld = 'VMAU'
094400160516     c                   clear                   tibs02ds
094500160516     c                   move      'R'           t02mod
094600160516     c                   exsr      srvmau
094700160516      *
094800011123     C*CATEGORIA
094900011123     C                   when      §fld = 'VCAT'
095000011126     c                   exsr      srvcat
095100011123     C*CODICE AUTISTA
095200011123     C                   when      §fld = 'VCAU'
095300011126     c                   exsr      srvcau
095400011123     C*FORNITORE
095500011123     C                   when      §fld = 'VFOR'
095600011126     c                   movel     vfor          vfors             8
095700011126     C                   RESET                   Ana201DS
095800011126     c                   movel     socxsc        soc201
095900011126     c                   movel(p)  'F'           kcc201
096000011126     C                   EVAL      Kpjbu = Ana201DS
096100011126     C                   CALL      'ANA201R'
096200011126     C                   PARM                    Kpjba
096300011126     C                   EVAL      Ana201DS = Kpjbu
096400011126     C                   IF        Err201 = '0' and ksc201 <> *blanks
096500011126     c                   movel     ksc201        vfor
096600011126     c                   EXSR      SRVFOR
096700011126     c                   else
096800011126     c                   movel     vfors         vfor
096900011126     c                   end
097000011123     C*CARROZZERIA
097100011123     C                   when      §fld = 'VCAR'
097200011126     c                   exsr      srvcar
097300011123     C*COD.CONSUMI
097400011123     C                   when      §fld = 'VCON'
097500011126     c                   exsr      srvcon
097600011123     C*CODICE SIT. PROPRIETA'
097700011123     C                   when      §fld = 'VCSP'
097800011126     c                   exsr      srvcsp
097900011123     C*TIPO GOMME
098000011123     C                   when      §fld = 'VTG1'
098100011123     c                   movel     'XG'          parcod
098200011126     c                   movel     vtg1          parkey
098300011123     c                   exsr      srtab
098400011123     c                   movel     parkey        vtg1
098500011123     C                   when      §fld = 'VTG2'
098600011123     c                   movel     'XG'          parcod
098700011126     c                   movel     vtg2          parkey
098800011123     c                   exsr      srtab
098900011123     c                   movel     parkey        vtg2
099000011123     C                   when      §fld = 'VTG3'
099100011123     c                   movel     'XG'          parcod
099200011126     c                   movel     vtg3          parkey
099300011123     c                   exsr      srtab
099400011123     c                   movel     parkey        vtg3
099500011123     C*TIPO MANUTENZIONE
099600011123     C                   when      §fld = 'VMAN'
099700011126     c                   exsr      srvman
099800011123     C*COD.UTILIZZO
099900011123     C                   when      §fld = 'VUTI'
100000011126     c                   exsr      srvuti
100100011123     C*COD.STATO
100200011123     C                   when      §fld = 'VSTA'
100300011126     c                   exsr      srvsta
100400011123     C                   endsl
100500011123     C* POSIZIONAMENTO CURSORE
100600011123     C                   Z-ADD     §RIGA         §RIG
100700011123     C                   Z-ADD     §COLON        §COL
100800011123     c*
100900011123     C                   ENDSR
101000011122     C**********************************************************************
101100000000     C** CONTROLLI 3 VIDEATA
101200011122     C**********************************************************************
101300011122     C     ctr03         BEGSR
101400011123     c                   movel     ' '           fld
101500011122     c                   setoff                                       99
101600011122     c                   do
101700000000     C* TASSA CIRCOLAZIONE
101800011220     C                   IF        vdtc = 0 and §xadtc = 'S'
101900011122     C                   SETON                                        8999
102000011122     C                   leave
102100011220     C                   End
102200011220     C                   IF        vdtc <> 0
102300011126     C     *EUR          test(d)                 vdtc                   89
102400011122     c                   if        *in89
102500011122     C                   SETON                                        99
102600011122     C                   leave
102700011122     C                   END
102800011122     C                   END
102900011123     c* periodicità tassa circolazione
103000011123     c                   if        vpbl = *blanks
103100011123     c                   seton                                        9199
103200011123     c                   leave
103300011123     c                   end
103400000000     C* TASSA PUBBLICITA'
103500011126     C     vdtP          IFne      0
103600011126     C     *EUR          test(d)                 vdtp                   97
103700011122     c                   if        *in97
103800011122     C                   SETON                                        99
103900011122     C                   leave
104000011122     C                   END
104100011122     C                   END
104200031127     c* equipaggiamento
104300031127     c                   if        vequi <> ' '
104400031127     c                   exsr      srvequi
104500031127     c                   end
104600000000     C* DATA E COMPAGNIA ASS. RC AUTO
104700011126     C                   EXSR      SRVCRC
104800011126     C   99              leave
104900011122     C     vdrc          IFne      0
105000011126     C     *EUR          test(d)                 vdrc                   92
105100011122     c                   if        *in92
105200011122     C                   SETON                                        99
105300011122     C                   leave
105400011122     C                   END
105500011122     C                   END
105600000000     C* DATA E COMPAGNIA ASS. INCENDIO/FURTO
105700011123     c                   if        vcfi <> *blanks
105800011126     C                   EXSR      SRVCFI
105900011126     C   99              leave
106000020128     C                   else
106100020128     C                   MOVEL     *blanks       VDcfi
106200020128     C                   END
106300000000     C   43              Z-ADD     VDRC          VDFI
106400020128     C                   IF        vdfi = 0 and vcfi <> *blanks and
106500020128     c                             §xdass <> 'N'
106600011122     C                   SETON                                        9399
106700011122     C                   leave
106800020128     C                   end
106900020128     C                   IF        vdfi <> 0
107000020128     c                   if        vcfi = *blanks
107100020128     C                   SETON                                        7599
107200020128     C                   leave
107300020128     C                   end
107400011123     C     *EUR          test(d)                 vdfi                   93
107500011122     c                   if        *in93
107600011122     C                   SETON                                        99
107700011122     C                   leave
107800011122     C                   END
107900000000     C                   END
108000000000     C* ASS.C/TERZI
108100011122     C     VNAC          IFne      *BLANKS
108200011122     C     VDAC          orne      0
108300011122     C     VSAC          orne      0
108400000000      *
108500000000     C     VNAC          IFEQ      *BLANKS
108600000000     C                   SETON                                        7399
108700011122     C                   leave
108800000000     C                   END
108900000000     C     VDAC          IFEQ      0
109000000000     C                   SETON                                        9599
109100011122     C                   leave
109200000000     C                   ELSE
109300011123     C     *EUR          test(d)                 vdac                   95
109400011122     c                   if        *in95
109500011122     C                   SETON                                        99
109600011122     C                   leave
109700011122     C                   END
109800000000     C                   END
109900000000     C     VSAC          IFEQ      0
110000000000     C                   SETON                                        9699
110100011122     C                   leave
110200000000     C                   ELSE
110300011123     C     *EUR          test(d)                 vsac                   96
110400011122     c                   if        *in96
110500011122     C                   SETON                                        99
110600011122     C                   leave
110700011122     C                   END
110800000000     C                   END
110900011122     C                   end
111000011123     c* tipo tariffa
111100011123     c                   if        vtta = *blanks
111200011123     c                   seton                                        9899
111300011123     c                   leave
111400011123     c                   end
111500000000     C*CODICE TARIFFA
111600011126     C                   EXSR      SRVCTA
111700011126     C   99              leave
111800000000     C     DATFIS        IFEQ      'QL'
111900000000     C     VPQL          ANDEQ     0
112000000000     C                   SETON                                        6299
112100011122     C                   leave
112200000000     C                   END
112300000000     C     DATFIS        IFEQ      'CV'
112400000000     C     VCVF          ANDEQ     0
112500000000     C                   SETON                                        6299
112600011122     C                   leave
112700000000     C                   END
112800011122     C                   enddo
112900011122     c*
113000011122     c                   endsr
113100011123     C**********************************************************************
113200011123     C** RICERCA 3 VIDEATA
113300011123     C**********************************************************************
113400011123     C     RIC03         BEGSR
113500011123     c                   movel     '1'           fld
113600011123     c                   SELECT
113700011123     C* COMPAGNIA ASS. RC AUTO
113800011123     C                   WHEN      §fld = 'VCRC'
113900011126     c                   exsr      srvcrc
114000011123     C* COMPAGNIA ASS. INCENDIO/FURTO
114100011123     C                   WHEN      §fld = 'VCFI'
114200031127     C* equipaggiamento
114300031127     C                   WHEN      §fld = 'VEQUI'
114400031127     c                   exsr      srvequi
114500011123     C*CODICE TARIFFA
114600011123     C                   WHEN      §fld = 'VCTA'
114700011126     c                   exsr      srvcta
114800011123     C                   endSL
114900011123     C* POSIZIONAMENTO CURSORE
115000011123     C                   Z-ADD     §RIGA         §RIG
115100011123     C                   Z-ADD     §COLON        §COL
115200011123     c*
115300011123     c                   endsr
115400011126     c**********************************************************************
115500011126     C** fornitore
115600011126     c**********************************************************************
115700011126     C     srvfor        BEGSR
115800011126     c*
115900011126     c     KFOR          CHAIN     ANFRN01L                           42
116000011126     C                   IF        NOT *IN42
116100011126     c     FRNSOGG       CHAIN     ANSOG01L                           42
116200011126     C                   END
116300011126     C                   IF        NOT *IN42
116400011126     C                   MOVEL(P)  SOGDES        VDFOR
116500011126     C                   ELSE
116600011126     C                   CLEAR                   VDFOR
116700011126     c                   end
116800011126     c*
116900011126     C                   ENDSR
117000020607     c**********************************************************************
117100020607     C** società proprietaria
117200020607     c**********************************************************************
117300020607     C     srvdit        BEGSR
117400020607     c*
117500020607     C     vdit          CHAIN     ansif01L
117600020607     C                   if        not %found(ansif01l)
117700020607     C                   SETON                                        8799
117800020607     c                   movel     *blanks       vddit
117900020607     c                   else
118000020607     c                   movel     sifdesbrev    vddit
118100020607     C                   END
118200020607     c*
118300020607     C                   ENDSR
118400011126     c**********************************************************************
118500011126     C** FILIALE
118600011126     c**********************************************************************
118700011126     C     srvfil        BEGSR
118800011126     c*
118900011126     C                   MOVE      VFIL          FILV              3 0
119000011126     C     FILV          CHAIN     AZORG
119100011126     C                   if        not %found(azorg01l)
119200011126     C                   SETON                                        8899
119300011126     c                   else
119400011126     C                   MOVEL     ORGDES        VDFIL
119500011126     C                   END
119600011126     c*
119700011126     C                   ENDSR
119800020607     c**********************************************************************
119900020607     C** società in dotazione
120000020607     c**********************************************************************
120100020607     C     srvsoc        BEGSR
120200020607     c*
120300020607     C     vsoc          CHAIN     ansif01L
120400020607     C                   if        not %found(ansif01l)
120500020607     C                   SETON                                        6099
120600020607     c                   movel     *blanks       vdsoc
120700020607     c                   else
120800020607     c                   movel     sifdesbrev    vdsoc
120900020607     C                   END
121000020607     c*
121100020607     C                   ENDSR
121200011126     c**********************************************************************
121300011126     C** centro di costo
121400011126     c**********************************************************************
121500011126     C     srvcdc        BEGSR
121600011126     c*
121700011126     C                   movel     *blanks       VDCDC
121800011126     C                   movel     VCDC          ententita
121900011126     C     kent          chain     anent01l
122000011126     C                   IF        NOT %FOUND(ANENT01L)
122100011126     C                   SETON                                        6199
122200011126     C                   ELSE
122300011126     C                   move      entdesbrev    VDCDC
122400011126     C                   END
122500011126     c*
122600011126     C                   ENDSR
122700011126     c**********************************************************************
122800011126     C** categoria
122900011126     c**********************************************************************
123000011126     C     srvcat        BEGSR
123100011126     c*
123200011126     c                   movel     'XA'          parcod            2
123300011126     c                   movel(P)  VCAT          parkey
123400011126     c                   exsr      srtab
123500011126     c                   movel     parkey        vcat
123600011126     c                   if        *in99
123700011220     C                   MOVEL     *blanks       dsxa
123800011220     C                   MOVEL     *blanks       VDCAT
123900011126     C                   SETON                                        85
124000011126     C                   else
124100011220     C                   MOVEL     TBLUNI        dsxa
124200011220     C                   MOVEL     §xades        VDCAT
124300011126     C                   END
124400011126     c*
124500011126     C                   ENDSR
124600160516     c**********************************************************************
124700160516     C** controllo marca
124800160516     c**********************************************************************
124900160516     C     srvcaut       BEGSR
125000160516     c*
125100160516     c                   if        vcaut <> *blank and t02mod <> 'R'
125200160516     c                   movel     vcaut         t02ke1
125300160516     c                   endif
125400160516     c                   movel     knsif         t02sif
125500160516     c                   movel     'CAU'         t02cod
125600160516     c                   call      'TIBS02R'
125700160516     c                   parm                    KPJBA
125800160516     c                   parm                    TIBS02DS
125900160516     c                   if        t02err =  *blanks
126000160516     c                   movel     t02uni        dcau
126100160516     c                   movel     t02ke1        vcaut
126200160516     c                   movel     §caudes       vcautd
126300160516     c                   else
126400160516     c                   seton                                        8199
126500160516     c                   endif
126600160516     C                   ENDSR
126700160516     c**********************************************************************
126800160516     C** controllo modello
126900160516     c**********************************************************************
127000160516     C     srvmau        BEGSR
127100160516     c*
127200160516     c                   if        vmau <> *blank and t02mod <> 'R'
127300160516     c                   movel     vmau          t02ke1
127400160516     c                   endif
127500160516     c                   movel     knsif         t02sif
127600160516     c                   movel     'MAU'         t02cod
127700160516     c                   call      'TIBS02R'
127800160516     c                   parm                    KPJBA
127900160516     c                   parm                    TIBS02DS
128000160516     c                   if        t02err =  *blanks
128100160516     c                   movel     t02uni        dmau
128200160516     c                   movel     t02ke1        vmau
128300160516     c                   movel     §maudes       vmaud
128400160516     c                   else
128500160516     c                   seton                                        8099
128600160516     c                   endif
128700160516     c*
128800160516     C                   ENDSR
128900011126     c**********************************************************************
129000011126     C** autista
129100011126     c**********************************************************************
129200011126     C     srvcau        BEGSR
129300011126     c                   movel     'XN'          parcod
129400011126     c                   movel(P)  VCAU          parkey
129500011126     c                   exsr      srtab
129600011126     c                   movel     parkey        vcau
129700011126     c                   if        *in99
129800011126     C                   MOVEL     *blanks       VDAU
129900070925     c                   clear                   dsxn
130000011126     C                   SETON                                        55
130100011126     C                   else
130200050216     C                   MOVEL     TBLUNI        dsxn
130300050216     C                   MOVEL     §XNdes        VDAU
130400050216     C                   MOVEL     §XNFUN        Vfun
130500011126     C                   END
130600011126     c*
130700011126     C                   ENDSR
130800011126     c**********************************************************************
130900011126     C** carrozzeria
131000011126     c**********************************************************************
131100011126     C     srvcar        BEGSR
131200011126     c*
131300011126     c                   movel(P)  VCAR          parkey
131400011126     c                   movel     'XB'          parcod
131500011126     c                   exsr      srtab
131600011126     c                   movel     parkey        vcar
131700011126     c                   if        *in99
131800011126     C                   MOVEL     *blanks       VDCAR
131900011126     C                   SETON                                        86
132000011126     C                   else
132100011126     C                   MOVEL     TBLUNI        VDCAR
132200011126     C                   END
132300011126     c*
132400011126     C                   ENDSR
132500011126     c**********************************************************************
132600011126     C** consumi
132700011126     c**********************************************************************
132800011126     C     srvcon        BEGSR
132900011126     c*
133000011207     C                   clear                   dsxh
133100011126     c                   movel     'XH'          parcod
133200011126     c                   movel(P)  VCON          parkey
133300011126     c                   exsr      srtab
133400011126     c                   movel     parkey        vcon
133500011126     c                   if        *in99
133600011126     C                   SETON                                        44
133700011126     C                   else
133800011207     C                   MOVEL     TBLUNI        dsxh
133900011126     C                   END
134000011207     c                   movel     §xhdes        vdcon
134100011207     c                   z-add     §xhmed        vmed
134200011126     c*
134300011126     C                   ENDSR
134400011126     c**********************************************************************
134500011126     C** tipo proprietà
134600011126     c**********************************************************************
134700011126     C     srvcsp        BEGSR
134800011126     c*
134900011126     c                   movel     'XR'          parcod
135000011126     c                   movel(P)  VCSP          parkey
135100011126     c                   exsr      srtab
135200011126     c                   movel     parkey        vcsp
135300011126     c                   if        *in99
135400011126     C                   MOVEL     *blanks       VDSP
135500011126     C                   SETON                                        56
135600011126     C                   else
135700011126     C                   MOVEL     TBLUNI        VDSP
135800011126     C                   END
135900011126     c*
136000011126     C                   ENDSR
136100011126     c**********************************************************************
136200011126     C** tipo manutenzione
136300011126     c**********************************************************************
136400011126     C     srvman        BEGSR
136500011126     c*
136600011126     c                   movel     'XC'          parcod
136700011126     c                   movel(P)  VMAN          parkey
136800011126     c                   exsr      srtab
136900011126     c                   movel     parkey        vman
137000011126     c                   if        *in99
137100011126     C                   MOVEL     *blanks       VDMAN
137200011126     C                   SETON                                        78
137300011126     C                   else
137400011126     C                   MOVEL     TBLUNI        VDMAN
137500011126     C                   END
137600011126     c*
137700011126     C                   ENDSR
137800011126     c**********************************************************************
137900011126     C** utilizzo
138000011126     c**********************************************************************
138100011126     C     srvuti        BEGSR
138200011126     c*
138300011126     c                   movel     'XL'          parcod
138400011126     c                   movel(P)  VUTI          parkey
138500011126     c                   exsr      srtab
138600011126     c                   movel     parkey        vuti
138700011126     c                   if        *in99
138800011126     C                   MOVEL     *blanks       VDES
138900011126     C                   SETON                                        45
139000011126     C                   else
139100011126     C                   MOVEL     TBLUNI        VDES
139200011126     C                   END
139300011126     c*
139400011126     C                   ENDSR
139500011126     c**********************************************************************
139600011126     C** stato
139700011126     c**********************************************************************
139800011126     C     srvsta        BEGSR
139900011126     c*
140000011126     c                   movel     'XF'          parcod
140100011126     c                   movel(P)  vsta          parkey
140200011126     c                   exsr      srtab
140300011126     c                   movel     parkey        vsta
140400011126     c                   if        *in99
140500011126     C                   MOVEL     *blanks       VDsta
140600011126     C                   SETON                                        83
140700011126     C                   else
140800011126     C                   MOVEL     TBLUNI        VDsta
140900011126     C                   END
141000011126     c*
141100011126     C                   ENDSR
141200011126     c**********************************************************************
141300011126     C** ass.ne RCA auto
141400011126     c**********************************************************************
141500011126     C     srvcrc        BEGSR
141600011126     c*
141700011126     c                   movel     'XD'          parcod
141800011126     c                   movel(P)  vcrc          parkey
141900011126     c                   exsr      srtab
142000011126     c                   movel     parkey        vcrc
142100011126     c                   if        *in99
142200011126     C                   MOVEL     *blanks       VDcrc
142300011126     C                   SETON                                        74
142400011126     C                   else
142500011126     C                   MOVEL     TBLUNI        VDcrc
142600011126     C                   END
142700011126     c*
142800011126     C                   ENDSR
142900031127     c**********************************************************************
143000031127     C** equipaggiamento
143100031127     c**********************************************************************
143200031127     C     srvequi       BEGSR
143300031127     c*
143400031127     c                   movel     'XT'          parcod
143500031127     c                   exsr      srtabequi
143600031127     c                   movel     parkey        vequi
143700031128     C                   MOVEL     *blanks       VDequ1
143800031128     C                   MOVEL     *blanks       VDequ2
143900031127     c                   if        *in99
144000031127     C                   SETON                                        76
144100031127     C                   else
144200031128     C                   MOVEL     des1          VDequ1
144300031128     C                   MOVEL     des2          VDequ2
144400031127     C                   END
144500031127     c*
144600031127     C                   ENDSR
144700011126     c**********************************************************************
144800011126     C** ass.ne inc/furto
144900011126     c**********************************************************************
145000011126     C     srvcfi        BEGSR
145100011126     c*
145200011126     c                   movel     'XD'          parcod
145300011126     c                   movel(P)  vcfi          parkey
145400011126     c                   exsr      srtab
145500011126     c                   movel     parkey        vcfi
145600011126     c                   if        *in99
145700020111     c                   clear                   dsxd
145800011126     C                   MOVEL     *blanks       VDcfi
145900011126     C                   SETON                                        75
146000011126     C                   else
146100020111     C                   MOVEL     TBLUNI        dsxd
146200020111     C                   MOVEL     §xddes        vdcfi
146300011126     C                   END
146400011126     c*
146500011126     C                   ENDSR
146600011126     c**********************************************************************
146700011126     C** tariffa
146800011126     c**********************************************************************
146900011126     C     srvcta        BEGSR
147000011126     c*
147100011126     c                   movel     'XI'          parcod
147200011126     c                   movel(P)  vcta          parkey
147300011126     c                   exsr      srtab
147400011126     c                   movel     parkey        vcta
147500011126     c                   if        *in99
147600011126     C                   MOVEL     *blanks       dstar
147700011126     C                   SETON                                        63
147800011126     C                   else
147900011126     C                   MOVEL     TBLUNI        dstar
148000011126     C                   END
148100011126     c*
148200011126     C                   ENDSR
148300011123     C**********************************************************************
148400011123     C** Controlli e decodifiche campi con tabel00f
148500011123     C**********************************************************************
148600011123     C     srtab         BEGSR
148700011123     c                   if        fld <> *blanks
148800011126     c                   movel     parkey        parkeys           8
148900011123     C                   CALL      'TRUL19R'
149000011123     c                   parm                    parcod            2
149100011123     c                   parm      '1'           parord            1
149200011123     c                   parm      *blanks       parkey            8
149300011126     c                   parm      *blanks       cmd               1
149400011126     c* ritorno senza scelta
149500011126     c                   if        cmd = 'L'
149600011126     c* in aggiornamento non è possibile cambiare l'utista
149700011126     c                             or (§fld = 'VCAU' and *in84)
149800011126     c                   movel     parkeys       parkey
149900011123     C                   END
150000011126     C                   END
150100011123     C                   MOVEL     *blanks       tbluni
150200011123     C                   MOVEL     parcod        COD
150300011123     C                   MOVEL(p)  parkey        KEY
150400011123     C     KTAB          CHAIN     TABEL                              99
150500011123     c*
150600011123     C                   ENDSR
150700031127     C**********************************************************************
150800031127     C** Controlli e decodifiche equipaggiamento
150900031127     C**********************************************************************
151000031127     C     srtabequi     BEGSR
151100031127     c                   if        fld = *blanks
151200031127     c                   eval      tipo = 'C'
151300031128     c                   movel(P)  vequi         parkey
151400031127     c                   else
151500031127     c                   eval      tipo = 'R'
151600031128     c                   clear                   parkey
151700031127     c                   end
151800031127     C                   CALL      'TRMZ46R'
151900031127     c                   parm                    tipo              1
152000031128     c                   parm                    parkey
152100031127     c                   parm      *blanks       des1             65
152200031127     c                   parm      *blanks       des2             65
152300031128     c                   if        parkey <> ' '
152400031127     C                   MOVEL     *blanks       tbluni
152500031127     C                   MOVEL     'XT'          COD
152600031127     C                   MOVEL(p)  parkey        KEY
152700031127     C     KTAB          CHAIN     TABEL                              99
152800031128     c                   end
152900031127     c*
153000031127     C                   ENDSR
153100011123     c**********************************************************************
153200011123     c* pulisce 1° formato
153300011123     c**********************************************************************
153400011123     C     clr00         BEGSR
153500011123     C                   Z-ADD     0             §RIGA
153600011123     C                   Z-ADD     0             §COLON
153700011123     C                   CLEAR                   §FLD
153800011123     C                   ENDSR
153900011123     c**********************************************************************
154000011123     c* pulisce 1° formato
154100011123     c**********************************************************************
154200011123     C     clr01         BEGSR
154300011126     c                   exsr      clr00
154400011123     C                   Z-ADD     0             §rig
154500011123     C                   Z-ADD     0             §col
154600011123     C                   Z-ADD     0             VNRF
154700070925     C                   MOVEL     *BLANKS       savsoc
154800070925     C                   MOVEL     *BLANKS       Vcau
154900011218     C                   MOVEL     *BLANKS       VTGA
155000011123     C                   MOVEL     *BLANKS       VTGN
155100011123     C                   MOVEL     *BLANKS       VTLA
155200011123     C                   ENDSR
155300011126     c**********************************************************************
155400011126     c* pulisce 2° formato
155500011126     c**********************************************************************
155600011126     C     clr03         BEGSR
155700011126     C                   Z-ADD     0             §rig
155800011126     C                   Z-ADD     0             §col
155900011126     c                   exsr      clr00
156000011126     C                   ENDSR
156100011122     c**********************************************************************
156200011122     C*PULIZIA DEI CAMPI A VIDEO
156300011122     c**********************************************************************
156400011122     c     clr02         begsr
156500011126     c                   exsr      clr03
156600011122     C                   MOVEL     *BLANKS       VTLA
156700011122     C                   MOVEL     *BLANKS       VSTA
156800011122     C                   MOVEL     *BLANKS       VDSTA
156900011122     C                   MOVEL     *BLANKS       VTGA
157000011122     C                   MOVEL     *BLANKS       VTGN
157100011122     C                   MOVEL     *BLANKS       VCAT
157200011122     C                   MOVEL     *BLANKS       VDCAT
157300160516     C*                  MOVEL     *BLANKS       VMAR
157400160516     C*                  MOVEL     *BLANKS       VTIP
157500160516     C                   MOVEL     *BLANKS       VMAu
157600160516     C                   MOVEL     *BLANKS       Vcaut
157700011122RE   C                   MOVEL     *BLANKS       VCIL
157800011122     C                   MOVEL     *BLANKS       VCAR
157900011122     C                   MOVEL     *BLANKS       VDCAR
158000011122     C                   Z-ADD     0             VIMM
158100011220     C                   MOVE      udate         dataiso
158200011220     C                   move      dataiso       DATAeur
158300011220     C                   move      dataeur       DATAUT
158400011122     C                   MOVEL     *BLANKS       VMAN
158500011122     C                   MOVEL     *BLANKS       VDMAN
158600011122     C                   MOVEL     *BLANKS       VUTI
158700011122     C                   MOVEL     *BLANKS       VDES
158800011122     C                   MOVE      *BLANKS       VDSP
158900011122     C                   MOVE      *BLANKS       VCSP
159000011122     C                   MOVE      *BLANKS       VCAU
159100011122     C                   MOVE      *BLANKS       VDAU
159200011122     C                   Z-ADD     0             VSCAD
159300011122     C                   Z-ADD     0             VASS
159400011122     C                   MOVEL     *BLANKS       VALI
159500011122     C                   MOVEL     *BLANKS       Vcon
159600011122     C                   MOVEL     *BLANKS       Vdcon
159700011207     C                   z-add     0             Vmed
159800011122     C                   Z-ADD     0             VTAR
159900011122     C                   Z-ADD     0             VPPC
160000011122     C                   Z-ADD     0             VPTR
160100011122     C                   Z-ADD     0             VNGM
160200011122     C                   MOVEL     *BLANKS       VTG1
160300011122     C                   MOVEL     *BLANKS       VTG2
160400011122     C                   MOVEL     *BLANKS       VTG3
160500011123     c                   movel     *blanks       vDIT
160600011219     c                   movel     *blanks       vdDIT
160700011122     C                   MOVEL     *BLANKS       VFIL
160800011126     C                   MOVEL     *BLANKS       VFOR
160900020607     c                   movel     *blanks       vsoc
161000020607     c                   movel     *blanks       vdsoc
161100011126     C                   MOVEL     *BLANKS       VDFOR
161200011122     C                   MOVEL     *BLANKS       VDFIL
161300011122     C                   MOVEL     *BLANKS       VCDC
161400011122     C                   MOVEL     *BLANKS       VDCDC
161500031028     C                   Z-ADD     0             telev
161600031028     C                   Z-ADD     0             VDUR
161700011122     C                   Z-ADD     0             VDTC
161800011122     C                   Z-ADD     0             VDTP
161900011122     C                   Z-ADD     0             VDRC
162000011122     C                   MOVEL     *BLANKS       VCRC
162100011122     C                   MOVEL     *BLANKS       VDCRC
162200031127     C                   MOVEL     *BLANKS       Vequi
162300031128     C                   MOVEL     *BLANKS       VDequ1
162400031128     C                   MOVEL     *BLANKS       VDequ2
162500011122     C                   Z-ADD     0             VDFI
162600011122     C                   MOVEL     *BLANKS       VCFI
162700011122     C                   MOVEL     *BLANKS       VDCFI
162800011122     C                   MOVEL     *BLANKS       VNAC
162900011122     C                   Z-ADD     0             VDAC
163000011122     C                   Z-ADD     0             VSAC
163100011122     C                   Z-ADD     0             VPQL
163200011122     C                   MOVEL     '1'           VTTA
163300011123     C                   z-add     0             VCTA
163400011122     C                   Z-ADD     0             VCVF
163500011122     C                   MOVEL     *BLANKS       VDCTA
163600011122     C                   MOVEL     *BLANKS       VPBL
163700011122     C                   Z-ADD     0             VIMT
163800011122     C                   Z-ADD     0             VIMP
163900011122     C                   Z-ADD     0             VIMA
164000011122     C                   Z-ADD     0             VIMF
164100011122     C                   Z-ADD     0             VIMK
164200000000     C                   ENDSR
164300011122     C**********************************************************************
164400000000     C** AGGIORNAMENTO O INSERIMENTO
164500011122     C**********************************************************************
164600000000     C     AGGIOR        BEGSR
164700011122     C     KGAAM         CHAIN     mzAME000                           21
164800000000     C                   Z-ADD     0             AMEDAC
164900000000     C                   Z-ADD     0             AMESAC
165000000000     C                   Z-ADD     0             AMEDUR
165100000000     C                   Z-ADD     0             AMEDTC
165200000000     C                   Z-ADD     0             AMEDTP
165300000000     C                   Z-ADD     0             AMEDRC
165400000000     C                   Z-ADD     0             AMEDFI
165500000000     C                   Z-ADD     VNRF          AMENRF
165600000000     C                   MOVEL     VTLA          AMETLA
165700000000     C                   MOVEL     VTGA          AMETGA
165800000000     C                   MOVEL     VTGN          AMETGN
165900000000     C                   MOVEL     VSTA          AMESTA
166000000000     C                   MOVEL     VCAT          AMECAT
166100160516     C*                  MOVEL     VMAR          AMEMAR
166200160516     C*                  MOVEL     VTIP          AMETIP
166300160516     C                   MOVEL     Vmau          AMEmau
166400160516     C                   MOVEL     Vcaut         AMEcaut
166500970416RE   C                   MOVEL     VCIL          AMECIL
166600000000     C                   MOVEL     VCAR          AMECAR
166700910215     C                   Z-ADD     MES           ANTO
166800910215     C                   Z-ADD     ANN           MES
166900910215     C                   Z-ADD     ANTO          ANN
167000000000     C                   Z-ADD     VIMM          AMEIMM
167100000000     C                   MOVEL     VMAN          AMEMAN
167200000000     C                   MOVEL     VUTI          AMEUTI
167300000000     C                   Z-ADD     VASS          AMEASS
167400011122     C                   MOVEL     Vcon          AMEusi
167500011122     C                   MOVEL     VALI          AMEALI
167600000000     C                   Z-ADD     VTAR          AMETAR
167700000000     C                   Z-ADD     VPPC          AMEPPC
167800000000     C                   Z-ADD     VPQL          AMEPQL
167900000000     C                   Z-ADD     VPTR          AMEPTR
168000000000     C                   Z-ADD     VNGM          AMENGM
168100000000     C                   MOVEL     VTG1          AMETG1
168200000000     C                   MOVEL     VTG2          AMETG2
168300000000     C                   MOVEL     VTG3          AMETG3
168400011123     C                   movel     VDIT          AMEDIT
168500020607     C                   movel     Vsoc          AMEsoc
168600031029     C                   MOVEl     telev         AMEtel
168700031028     C                   MOVEL     VFIL          AMEFIL
168800000000     C                   MOVEL     VCDC          AMECDC
168900011123     C                   MOVEL     Vfor          AMEfor
169000000000     C*
169100000000     C     VDUR          IFNE      0
169200011122     C                   move      VDUR          DATAeur
169300011122     C                   MOVE      DATAeur       dataiso
169400011122     C                   MOVE      DATAIso       AMEDUR
169500000000     C                   END
169600000000     C*
169700000000     C     VDTC          IFNE      0
169800011122     C                   move      VDtc          DATAeur
169900011122     C                   MOVE      DATAeur       dataiso
170000011122     C                   MOVE      DATAIso       AMEDtc
170100000223     C                   END
170200000000     C                   Z-ADD     VIMT          AMEIMT
170300000000     C*
170400000000     C     VDTP          IFNE      0
170500011122     C                   move      VDtp          DATAeur
170600011122     C                   MOVE      DATAeur       dataiso
170700011122     C                   MOVE      DATAIso       AMEDtp
170800000223     C                   END
170900000000     C                   Z-ADD     VIMP          AMEIMP
171000000000     C*
171100000000     C     VDRC          IFNE      0
171200011122     C                   move      VDrc          DATAeur
171300011122     C                   MOVE      DATAeur       dataiso
171400011122     C                   MOVE      DATAIso       AMEDrc
171500000223     C                   END
171600000000     C                   MOVEL     VCRC          AMECRC
171700000000     C                   Z-ADD     VIMA          AMEIMA
171800031127     C                   MOVEL     Vequi         AMEequ
171900000000     C*
172000000000     C     VDFI          IFNE      0
172100011122     C                   move      VDfi          DATAeur
172200011122     C                   MOVE      DATAeur       dataiso
172300011122     C                   MOVE      DATAIso       AMEDfi
172400000000     C                   END
172500000000     C                   MOVEL     VCFI          AMECFI
172600000000     C                   Z-ADD     VIMF          AMEIMF
172700000000     C                   Z-ADD     VIMK          AMEIMK
172800000000     C                   MOVEL     VNAC          AMENAC
172900000000     C*
173000000000     C     VDAC          IFNE      0
173100011122     C                   move      VDac          DATAeur
173200011122     C                   MOVE      DATAeur       dataiso
173300011122     C                   MOVE      DATAIso       AMEDac
173400000000     C                   END
173500000000     C*
173600000000     C     VSAC          IFNE      0
173700011122     C                   move      Vsac          DATAeur
173800011122     C                   MOVE      DATAeur       dataiso
173900011122     C                   MOVE      DATAIso       AMEsac
174000000000     C                   END
174100910214     C     VSCAD         IFNE      0
174200011122     C                   move      Vscad         DATAeur
174300011122     C                   MOVE      DATAeur       dataiso
174400011122     C                   MOVE      DATAIso       AMEDsp
174500910214     C                   END
174600910215     C     DATAUT        IFNE      0
174700011122     C                   move      dataut        DATAeur
174800011122     C                   MOVE      DATAeur       dataiso
174900011122     C                   MOVE      DATAIso       AMEDal
175000910215     C                   END
175100910214     C                   MOVEL     VCSP          AMECSP
175200910214     C                   MOVEL     VCAU          AMECAU
175300000000     C                   MOVEL     VTTA          AMETTA
175400011123     C                   z-add     VCTA          AMECTA
175500000000     C                   Z-ADD     VCVF          AMECVF
175600000000     C                   MOVEL     VPBL          AMEPBL
175700011122     C   21              WRITE     mzAME000
175800011122     C  N21              UPDATE    mzAME000
175900011123     c*
176000011123     c                   exsr      clr01
176100011123     c*
176200000000     C                   ENDSR
176300011121     C*---------------------------------------------------------------*
176400011121     C     *inzsr        BEGSR
176500011121     C*---------------------------------------------------------------*
176600011121     C     *ENTRY        PLIST
176700011121     C                   PARM                    KPJBA
176800011122     c                   do
176900011121     C                   Z-ADD     1             CODUT
177000011121     C                   CALL      'XPARUT'
177100011121     C                   PARM                    UTEDSE
177200011122     C     CODUT         ifeq      -1
177300011122     c                   seton                                        lr
177400011122     c                   leave
177500011122     c                   end
177600011121     C                   MOVEL     RAGUT         RSUT             20
177700011122     C                   MOVEL     RAGUT         RSUT2            25
177800011221     C* CONTROLLO SE GESTIRE SOLO LA VISUALIZZAZIONE DEI RECORD IN BASE
177900011221     C* AL CODICE DELL'AZIONE
178000011221     C     KCDAZ         COMP      'MZ92'                                 20
178100011122      * Reperimento dati società
178200011122     C                   clear                   SocXsc
178300011122     C                   eval      tipxsc = 'SOC001'
178400011122     C                   exsr      rtvsocieta
178500011122     C                   if        rtnxsc = '1'
178600011122     C                   seton                                        lr
178700011122     c                   leave
178800011122     C                   else
178900011122     C                   MOVEL     XSOCDS        xsoc001ds
179000011122     C                   endif
179100011121     C* POSIZIONAMENTO TABELLE                             *
179200011121     C     KTAB          KLIST                                                  *
179300011121     C                   KFLD                    CODUT                          *
179400011121     C                   KFLD                    COD               2            *
179500011121     C                   KFLD                    KEY               8            *
179600011123     C* FORNITORE
179700011123     C     KFOR          KLIST                                                  *
179800011123     C                   KFLD                    VDIT                           *
179900011123     C                   KFLD                    VFOR                           *
180000011123     C                   KFLD                    FILLL                          *
180100011121     C* POSIZIONAMENTO X N.RIFERIMENTO                     *
180200011121     C     KGAAM         KLIST                                                  *
180300011121     C                   KFLD                    VNRF                           *
180400011121     C* POSIZIONAMENTO X N.DI TARGA                        *
180500011121     C     KTAR          KLIST                                                  *
180600011121     C                   KFLD                    VTGA                           *
180700011121     C                   KFLD                    VTGN                           *
180800011122     C* POSIZIONAMENTO SU ANENT                            *
180900011122     C     Kent          KLIST
181000011122     C                   KFLD                    xscsoc
181100011122     C                   KFLD                    enttprifan
181200011122     C                   KFLD                    ententita
181300011122     C                   move      'CDC'         enttprifan
181400011121     C*----------------------------------------------------*
181500011121     C* RICERCA TARGA X CITTA'
181600011121     C     RICTAR        PLIST
181700011121     C                   PARM                    CODUT
181800011121     C                   PARM                    RSUT
181900011121     C                   PARM                    VTGA
182000011121     C                   PARM                    VTGN
182100011122     c                   enddo
182200011122     c*
182300011121     C                   ENDSR
182400011122     C*---------------------------------------------------------------------
182500011122     c*  Reperisco società.
182600011122     C*---------------------------------------------------------------------
182700011122     C     rtvsocieta    begsr
182800011122     C*
182900011122     C                   CALLb     'XSOC'
183000011122   >>C                   Parm                    TipXsc            6
183100011122   >>C                   Parm                    SocXsc            3
183200011122   >>C                   Parm                    CdsXsc            9 0
183300011122   >>C                   Parm                    ModXsc            3
183400011122   >>C                   Parm      *BLANK        RtnXsc            1
183500011122     C                   Parm                    XsocDS
183600011122   >>C                   Parm                    Kpjba
183700011122     C*
183800011122     C                   endsr
183900000000     C*---------------------------------------------------------------*
