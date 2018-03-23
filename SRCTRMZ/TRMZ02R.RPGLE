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
031500160805     C* LETTURA X N.TELepass
031600160805     C     VTel          IFNE      *BLANKS
031700160805     C     VTel          CHAIN     mzAME5                             21
031800160805     C   21              SETON                                        7099
031900160805     C   21              leave
032000160805     C                   END
032100011221     C* SE POSSO SOLO VISUALIZZARE NON EMMETTO LA VIDEATA DI IMMISSIONE
032200011221     C                   IF        *IN20 AND *IN71
032300011221     C                   SETON                                        99
032400011221     C                   LEAVE
032500011221     C                   END
032600011221     C*
032700011123     c                   exsr      clr02
032800011123     C                   if        not *in21
032900011123     C                   SETON                                        84
033000011123     C                   Z-ADD     AMENRF        VNRF
033100011218     C                   MOVEL     AMETLA        VTLA
033200011123     C                   MOVEL     AMESTA        VSTA
033300011123     C*RICERCA SULLA TABELLA 'XF' LA DESCRIZIONE
033400011123     C*DELLO STATO AUTOMEZZO
033500011123     c                   if        vsta <> *blanks
033600011126     c                   exsr      srvSTA
033700011123     c                   else
033800011123     C                   MOVEL     *blanks       VDsta
033900011123     C                   END
034000011123     C                   MOVEL     AMETGA        VTGA
034100011123     C                   MOVEL     AMETGN        VTGN
034200011123     C                   MOVEL     AMECAT        VCAT
034300011123     C*RICERCA SULLA TABELLA 'XA' LA DESCRIZIONE
034400011123     C*DELLA CATEGORIA AUTOMEZZO
034500011126     c                   exsr      srvCAT
034600160516     C*                  MOVEL     AMEMAR        VMAR
034700160516     C*                  MOVEL     AMETIP        VTIP
034800160516     C                   MOVEL     AMEmau        VMAu
034900160517     c                   if        vmau <> *blank
035000160517     c                   clear                   tibs02ds
035100160517     c                   move      'C'           t02mod
035200160517     c                   clear                   vmaud
035300160517     c                   exsr      srvmau
035400160517     c                   endif
035500160516     C                   MOVEL     AMEcaut       Vcaut
035600160517     c                   if        vcaut <> *blank
035700160517     c                   clear                   tibs02ds
035800160517     c                   move      'C'           t02mod
035900160517     c                   clear                   vcautd
036000160517     c                   exsr      srvcaut
036100160517     c                   endif
036200011123RE   C                   MOVEL     AMECIL        VCIL
036300011123     C                   MOVEL     AMECAR        VCAR
036400011123     C*RICERCA SULLA TABELLA 'XB' LA DESCRIZIONE
036500011123     C*DEL TIPO CAROZZERIA
036600011123     c                   if        vcar <> *blanks
036700011126     c                   exsr      srvCAR
036800011123     C                   else
036900011123     C                   MOVEL     *blanks       VDcar
037000011123     C                   END
037100011123     C                   Z-ADD     AMEIMM        VIMM
037200011123     C                   Z-ADD     MES           ANTO              2 0
037300011123     C                   Z-ADD     ANN           MES
037400011123     C                   Z-ADD     ANTO          ANN
037500011123     C                   MOVEL     AMEMAN        VMAN
037600011123     C*RICERCA SULLA TABELLA 'XC' LA DESCRIZIONE
037700011123     C*DEL TIPO DI MANUTENZIONE
037800011126     c                   exsr      srvMAN
037900011123     C                   MOVEL     AMEUTI        VUTI
038000011123     C*RICERCA SULLA TABELLA 'XL' LA DESCRIZIONE
038100011123     C*DEL TIPO DI UTILIZZO
038200011123     c                   if        vuti <> *blanks
038300011126     c                   exsr      srvuti
038400011123     C                   else
038500011123     C                   MOVEL     *blanks       VDes
038600011123     C                   END
038700011123     C                   Z-ADD     AMEASS        VASS
038800011123     C                   movel     AMEusi        Vcon
038900011123     C*RICERCA SULLA TABELLA 'XH' LA DESCRIZIONE
039000011123     C*DEL COD. CONSUMI
039100011126     c                   exsr      srvCON
039200011123     C                   MOVEL     AMEALI        VALI
039300011123     C                   Z-ADD     AMETAR        VTAR
039400011123     C                   Z-ADD     AMEPPC        VPPC
039500011123     C                   Z-ADD     AMEPTR        VPTR
039600011123     C                   Z-ADD     AMEPQL        VPQL
039700011123     C                   Z-ADD     AMENGM        VNGM
039800011123     C                   MOVEL     AMETG1        VTG1
039900011123     C                   MOVEL     AMETG2        VTG2
040000011123     C                   MOVEL     AMETG3        VTG3
040100011123     C                   movel     AMEDIT        VDIT
040200011219     C*RICERCA LA DESCRIZIONE
040300011219     C*DELLA DITTA
040400020607     c                   exsr      srvdit
040500020607     C                   movel     AMEsoc        Vsoc
040600020607     C*RICERCA LA DESCRIZIONE
040700020607     C*DELLA DITTA
040800020607     c                   exsr      srvsoc
040900011123     C                   MOVEL     AMEFIL        VFIL
041000011123     C                   MOVEL     AMECDC        VCDC
041100011123     C                   MOVEL     AMEfor        Vfor
041200031029     c                   clear                   telev
041300031029     c                   if        ametel <> *blanks
041400031029     C                   MOVEl     AMEtel        telev
041500031029     c                   end
041600011126     C*RICERCA LA DESCRIZIONE
041700011123     C*DEL fornitore
041800011126     c                   if        vfor <> *blanks
041900011126     C                   EXSR      SRVFOR
042000011126     c                   end
042100011123     C*RICERCA SU AZORG LA DESCRIZIONE
042200011123     C*DELLA FILIALE IN DOTAZIONE
042300011126     C                   EXSR      SRVFIL
042400011123     C*RICERCA SU ANENT LA DESCRIZIONE
042500011123     C*DELLA FILIALE CDC
042600011126     C                   EXSR      SRVCDC
042700011220     c                   if        amedur <> 0
042800011123     C                   MOVE      AMEDUR        dataiso
042900011123     C                   move      dataiso       DATAeur
043000011123     C                   move      DATAeur       VDUR
043100011220     c                   else
043200011220     c                   z-add     0             vdur
043300011220     c                   end
043400011220     c                   if        amedsp <> 0
043500011123     C                   move      AMEDSP        dataiso
043600011123     C                   move      dataiso       dataeur
043700011123     C                   move      DATAeur       VSCAD
043800011220     c                   else
043900011220     c                   z-add     0             vscad
044000011220     c                   end
044100011123     C*RICERCA SULLA TABELLA 'XN' LA DESCRIZIONE
044200011123     C*DELL' AUTISTA AUTOMEZZO
044300011123     C                   MOVEL     AMECAU        VCAU
044400011126     C                   EXSR      SRVCAU
044500011220     c                   if        amedal <> 0
044600011123     C                   move      AMEDAL        DATAIso
044700011123     C                   move      dataiso       dataeur
044800011123     C                   move      DATAeur       dataut
044900011220     c                   else
045000011220     c                   z-add     0             dataut
045100011220     C                   END
045200011123     C*RICERCA SULLA TABELLA 'XR' LA DESCRIZIONE
045300011123     C*DEL TIPO PROPRIETA'
045400011123     C                   MOVEL     AMECSP        VCSP
045500011126     C                   EXSR      SRVCSP
045600011123     C* 3 VIDEATA
045700011123     C*TASSA DI CIRCOLAZIONE
045800011123     c                   if        amedtc <> 0
045900011123     C                   move      AMEDtc        DATAIso
046000011123     C                   move      dataiso       dataeur
046100011123     C                   move      DATAeur       vdtc
046200011123     c                   else
046300011123     c                   z-add     0             vdtc
046400011123     c                   end
046500011123     C                   Z-ADD     AMEIMT        VIMT
046600011123     C*TASSA PUBBLICITA'
046700011123     c                   if        amedtp <> 0
046800011123     C                   move      AMEDtp        DATAIso
046900011123     C                   move      dataiso       dataeur
047000011123     C                   move      DATAeur       vdtp
047100011123     c                   else
047200011123     c                   z-add     0             vdtp
047300011123     c                   end
047400011123     C                   Z-ADD     AMEIMP        VIMP
047500011123     C*SCADENZA ASS.RCA
047600011123     c                   if        amedrc <> 0
047700011123     C                   move      AMEDrc        DATAIso
047800011123     C                   move      dataiso       dataeur
047900011123     C                   move      DATAeur       vdrc
048000011123     c                   else
048100011123     c                   z-add     0             vdrc
048200011123     c                   end
048300011123     C                   MOVEL     AMECRC        VCRC
048400011123     C*RICERCA SULLA TABELLA 'XD' LA DESCRIZIONE
048500011123     C*DELLA COMP.ASS.RCA
048600011123     c                   if        vcrc <> *blanks
048700011126     C                   EXSR      SRVCRC
048800011123     C                   else
048900011123     C                   MOVEL     *blanks       VDcrc
049000011123     C                   END
049100031127     C*descrizione equipaggiamento
049200031127     C                   MOVEL     AMEequ        Vequi
049300031127     c                   if        vequi<> *blanks
049400031127     C                   EXSR      SRVequi
049500031127     C                   else
049600031128     C                   MOVEL     *blanks       VDequ1
049700031128     C                   MOVEL     *blanks       VDequ2
049800031127     C                   END
049900011123     c                   if        amedfi <> 0
050000011123     C                   move      AMEDfi        DATAIso
050100011123     C                   move      dataiso       dataeur
050200011123     C                   move      DATAeur       vdfi
050300011123     c                   else
050400011123     c                   z-add     0             vdfi
050500011123     c                   end
050600011123     C                   MOVEL     AMECFI        VCFI
050700011123     C                   Z-ADD     AMEIMA        VIMA
050800011123     C*RICERCA SULLA TABELLA 'XD' LA DESCRIZIONE
050900011123     C*DELLA COMP.ASS.INCENDIO/FURTO
051000011123     c                   if        vcfi <> *blanks
051100011126     C                   EXSR      SRVCFI
051200011123     C                   else
051300011123     C                   MOVEL     *blanks       VDcfi
051400011123     C                   END
051500011123     C                   Z-ADD     AMEIMF        VIMF
051600011123     C                   MOVEL     AMENAC        VNAC
051700011123     c                   if        amedac <> 0
051800011123     C                   move      AMEDac        DATAIso
051900011123     C                   move      dataiso       dataeur
052000011123     C                   move      DATAeur       vdac
052100011123     c                   else
052200011123     c                   z-add     0             vdac
052300011123     c                   end
052400011123     c                   if        amedac <> 0
052500011123     C                   move      AMEsac        DATAIso
052600011123     C                   move      dataiso       dataeur
052700011123     C                   move      DATAeur       vsac
052800011123     c                   else
052900011123     c                   z-add     0             vsac
053000011123     c                   end
053100011123     C                   Z-ADD     AMEIMK        VIMK
053200011123     C                   MOVEL     AMETTA        VTTA
053300011123     C                   MOVE      AMECTA        VCTA
053400011123     C*RICERCA SULLA TABELLA 'XI' LA DESCRIZIONE
053500011123     C*DELLA CODICE TARIFFA
053600011123     c                   if        vcta <> 0
053700011126     C                   EXSR      SRVCTA
053800011123     C                   else
053900011126     C                   MOVEL     *blanks       dstar
054000011123     C                   END
054100011123     C                   Z-ADD     AMECVF        VCVF
054200011123     C                   MOVEL     AMEPBL        VPBL
054300011123     C                   ENDif
054400011123     C                   enddo
054500011123     c                   if        *in99
054600011123     c                   exsr      clr01
054700011123     C                   END
054800011123     C                   ENDSR
054900011126     c**********************************************************************
055000011126     C* STORICO
055100011126     c**********************************************************************
055200011126     C     srstor        BEGSR
055300011126     c*
055400011122     C     *HIVAL        SETLL     mzAME01L
055500011122     C                   MOVEL     *BLANKS       FLAG
055600011122     C                   Z-ADD     VNRF          NRF
055700011122     C                   MOVEL     VTGA          TGA
055800011122     C                   MOVEL     VTGN          TGN
055900011122     C                   MOVEL     VCAU          CAU
056000011122     C                   MOVEL     VCSP          CSP
056100011122     C                   MOVEL     VFIL          FIL
056200040213     C                   MOVEl     telev         tele
056300031028     C                   MOVE      DATAUT        DAT
056400020607     C                   MOVEL     VSOC          SOC
056500030115     C                   MOVEL     Vcdc          cdc
056600030115     C                   MOVEL(p)  PARAM2        KPJBU
056700011129     C                   CALL      'TRMZ02R5'
056800011122     C                   PARM                    KPJBA
056900011122     C                   MOVEL     KPJBU         PARAM2
057000011122     C                   MOVEL     TGA           VTGA
057100011122     C                   MOVEL     TGN           VTGN
057200011122     C                   MOVEL     CAU           VCAU
057300011122     C                   MOVEL     CSP           VCSP
057400011129     C                   exsr      srvcsp
057500011122     C                   MOVEL     FIL           VFIL
057600011129     C                   exsr      srvfil
057700020607     C                   MOVEL     SOC           VSOC
057800020607     C                   exsr      srvSOC
057900030115     C                   MOVEL     cdc           Vcdc
058000030115     C                   exsr      srvcdc
058100011122     C     FLAG          IFEQ      'T'
058200011122     C                   MOVE      DAT           DATAUT
058300011129     C                   exsr      srvcau
058400011122     C                   END
058500040213     C                   MOVEl     tele          telev
058600011122     c*
058700011122     C                   ENDSR
058800011121     c**********************************************************************
058900011121     C* IMMISSIONE NUOVO CODICE
059000011121     c**********************************************************************
059100000000     C     NOVCOD        BEGSR
059200011122     C     *HIVAL        SETLL     mzAME000
059300011122     C                   READP     mzAME000                               22
059400011122     c                   if        not *in22
059500000000     C                   Z-ADD     AMENRF        VNRF
059600000000     C                   ADD       1             VNRF
059700011122     c                   end
059800000000     C                   ENDSR
059900011123     c**********************************************************************
060000011123     c* controlli 1° formato
060100011123     c**********************************************************************
060200011123     C     ctr01         BEGSR
060300011123     C                   SETOFF                                       99
060400011123     c                   do
060500011123     C     VNRF          IFNE      0
060600011123     C     VTGA          ANDNE     *BLANKS
060700011123     C                   SETON                                        99
060800011123     C                   LEAVE
060900011123     C                   END
061000011123     C     VTGA          IFNE      *BLANKS
061100011123     C     VTLA          ANDNE     *BLANKS
061200011123     C                   SETON                                        99
061300011123     C                   LEAVE
061400011123     C                   END
061500011123     C     VNRF          IFNE      0
061600011123     C     VTLA          ANDNE     *BLANKS
061700011123     C                   SETON                                        99
061800011123     C                   LEAVE
061900011123     C                   END
062000011123     C     VNRF          IFEQ      0
062100011123     C     VTGA          andeq     *BLANKS
062200011123     C     VTLA          andeq     *BLANKS
062300011218     C     Vcau          andeq     *BLANKS
062400160805     C     VTel          andeq     *BLANKS
062500011123     C                   SETON                                        99
062600011123     C                   LEAVE
062700011123     C                   END
062800011123     C                   ENDdo
062900011123     C                   ENDSR
063000011123     c**********************************************************************
063100011123     c* ricerca 1° videata
063200011123     c**********************************************************************
063300011123     C     ric01         BEGSR
063400011123     C*
063500011123     C                   SELECT
063600011221     C                   WHEN      §FLD = 'VTGN' OR §FLD = 'VTGA'
063700011221     C                   CALL      'TRMZ02R4'    RICTAR
063800011219     C                   WHEN      §FLD = 'VTLA'
063900011219     C                   CALL      'TRMZ42R'
064000011219     c                   parm                    vtla
064100011219     c                   parm                    cmd
064200160805     C                   WHEN      §FLD = 'VTEL'
064300160805     C                   CALL      'TRMZ49R'
064400160805     c                   parm                    vtel
064500160805     c                   parm                    cmd
064600011218     C                   WHEN      §FLD = 'VCAU'
064700011218     c                   if        naut > 1
064800011218     c                   move      ' '           ordina
064900011218     c                   else
065000011218     c                   move      '1'           ordina
065100011218     c                   movel     *blanks       vcau
065200011218     c                   end
065300011218     c                   z-add     0             vnrf
065400011218     C                   CALL      'TRMZ41R'
065500011218     c                   parm                    ordina            1
065600011218     c                   parm                    vcau
065700011218     c                   parm                    vnrf
065800011218     c                   parm                    cmd               1
065900011219     c                   if        cmd = 'L'
066000011219     c                   clear                   vnrf
066100011123     C                   END
066200011219     C                   END
066300011123     C* POSIZIONAMENTO CURSORE
066400011123     C                   Z-ADD     §RIGA         §RIG
066500011123     C                   Z-ADD     §COLON        §COL
066600011123     C                   ENDSR
066700011122     c**********************************************************************
066800000000     C** CONTROLLI 2 VIDEATA
066900011122     c**********************************************************************
067000011122     C     ctr02         BEGSR
067100011122     C                   SETOFF                                       99
067200011123     c                   movel     ' '           fld               1
067300011122     c                   do
067400020607     C*CODICE società proprietaria
067500020607     C     VDIT          IFEQ      *blanks
067600020607     C                   SETON                                        8799
067700020607     c                   movel     *blanks       vddit
067800020607     C                   leave
067900020607     C                   ELSE
068000020607     c                   exsr      srvdit
068100020607     C                   END
068200000000     C*CODICE FILIALE
068300011122     C                   MOVEL     *blanks       ORGDES
068400011122     C     VFIL          IFEQ      *BLANKS
068500011122     C                   SETON                                        8899
068600011122     C                   leave
068700011122     C                   else
068800000000     C                   TESTN                   VFIL                 10
068900011122     C                   if        not *in10
069000000000     C                   SETON                                        8899
069100011122     C                   leave
069200011122     C                   else
069300011126     c                   exsr      srvfil
069400011126     C   99              leave
069500000000     C                   END
069600011122     C                   END
069700020607     C*CODICE società in dotazione
069800020607     C     Vsoc          IFEQ      *blanks
069900020607     C                   SETON                                        6099
070000020607     c                   movel     *blanks       vdsoc
070100020607     C                   leave
070200020607     C                   ELSE
070300020607     c                   exsr      srvsoc
070400020607     C                   END
070500000000     C*CODICE CDC
070600011126     c                   exsr      srvcdc
070700011126     C   99              leave
070800000000     C*TARGA
070900000000     C     VTGA          IFEQ      *BLANKS
071000000000     C                   SETON                                        8299
071100011122     C                   leave
071200000000     C                   END
071300000000     C     VTGN          IFEQ      *BLANKS
071400000000     C                   SETON                                        8299
071500011122     C                   leave
071600000000     C                   END
071700011122     C*TELAIO
071800011122     C     VTLA          IFEQ      *BLANKS
071900011122     C                   SETON                                        7099
072000011122     C                   leave
072100011122     C                   END
072200160805     C*TELepass
072300160830     C*    telev         IFEQ      *zeros
072400160830     C*                  SETON                                        1499
072500160830     C*                  leave
072600160830     C*                  END
072700000000     C*MARCA
072800160516     C     Vcaut         IFEQ      *BLANKS
072900000000     C                   SETON                                        8199
073000011122     C                   leave
073100160516     C                   ELSE
073200160516     C                   clear                   tibs02ds
073300160516     c                   move      'C'           t02mod
073400160516     c                   clear                   vcautd
073500160516     c                   exsr      srvcaut
073600160516     C   99              leave
073700000000     C                   END
073800000000     C*TIPO
073900160516     C     Vmau          IFEQ      *BLANKS
074000000000     C                   SETON                                        8099
074100011122     C                   leave
074200160516     C                   ELSE
074300160516     c                   clear                   tibs02ds
074400160516     c                   move      'C'           t02mod
074500160516     c                   clear                   vmaud
074600160516     c                   exsr      srvmau
074700160516     C   99              leave
074800000000     C                   END
074900000000     C*CATEGORIA
075000011126     c                   exsr      srvcat
075100011126     C   99              leave
075200031106     C*Telepass
075300031106     c                   if        not *in84
075400031106     C                   IF        telev = 0 and §xatel = 'S'
075500031106     C                   SETON                                        1499
075600031106     C                   leave
075700031106     c                   end
075800040416     c                   if        telev <> 9999999999 and telev <> 0
075900031106     c                   movel     telev         com20
076000031106     c     com20         setll     mzame05l                               14
076100031106     c                   if        *in14
076200031106     C                   SETON                                        99
076300031106     C                   leave
076400031106     C                   END
076500031106     C                   END
076600031106     C                   END
076700910215     C*MESE/ANNO IMMATRICOLAZIONE
076800910215     C     MES           IFEQ      0
076900910215     C     MES           ORGT      12
077000000000     C                   SETON                                        7999
077100011122     C                   leave
077200000000     C                   END
077300910204     C*CODICE AUTISTA
077400011126     C                   EXSR      SRVCAU
077500011126     C   99              leave
077600910215     C* DATA ASSEGNAZIONE AUTISTA
077700011220     C                   IF        dataut = 0 and §xadau = 'S'
077800910215     C                   SETON                                        5899
077900011122     C                   leave
078000011220     C                   End
078100011220     C                   IF        dataut <> 0
078200011123     C     *EUR          test(d)                 dataut                 58
078300011122     c                   if        *in58
078400011122     C                   SETON                                        99
078500011122     C                   leave
078600910215     C                   END
078700910215     C                   END
078800011122     C*FORNITORE
078900011123     c                   if        vfor <> *blanks
079000011126     c                   exsr      srvfor
079100011123     c                   end
079200000000     C*CARROZZERIA
079300011123     c                   if        vcar <> *blanks
079400011126     C                   EXSR      srvcar
079500011126     C   99              leave
079600011122     C                   END
079700011123     C*alimentazione
079800011123     c                   if        vali = *blanks
079900011123     c                   seton                                        9099
080000011123     c                   leave
080100011123     c                   end
080200011123     C*COD.CONSUMI
080300011126     c                   exsr      srvcon
080400011126     C   99              leave
080500910204     C*CODICE SIT. PROPRIETA'
080600011122     C                   TESTN                   VCsp                 10
080700011122     C                   if        not *in10
080800011122     C                   SETON                                        5699
080900011122     C                   leave
081000011122     C                   END
081100011126     c                   exsr      srvcsp
081200011126     C   99              leave
081300910214     C* DATA SCAD. PROPRIETA'
081400011220     C                   IF        vscad = 0 and §xadsc = 'S'
081500011122     C                   SETON                                        5799
081600011122     C                   leave
081700011220     C                   End
081800011220     C     vscad         IFne      0
081900011123     C     *EUR          test(d)                 vscad                  57
082000011122     c                   if        *in57
082100011122     C                   SETON                                        99
082200011122     C                   leave
082300011122     C                   END
082400011122     C                   END
082500000000     C*TARA + PORT.UTILE = PORTATA PIENO CARICO
082600000000     C     VTAR          ADD       VPQL          COMPPC            3 0
082700000000     C     COMPPC        IFNE      VPPC
082800000000     C                   SETON                                        6499
082900011122     C                   leave
083000000000     C                   END
083100000000     C*TIPO GOMME
083200011123     c                   movel     'XG'          parcod
083300011123     c                   if        vtg1 <> *blanks
083400011122     c                   movel(P)  VTG1          parkey
083500011122     c                   exsr      srtab
083600011122     c                   movel     parkey        vtg1
083700011123     c                   if        *in99
083800011122     C                   SETON                                        66
083900011122     C                   leave
084000011122     C                   END
084100011122     C                   END
084200011123     c                   if        vtg2 <> *blanks
084300011122     c                   movel(P)  VTG2          parkey
084400011122     c                   exsr      srtab
084500011122     c                   movel     parkey        vtg2
084600011123     c                   if        *in99
084700011122     C                   SETON                                        67
084800011122     C                   leave
084900011122     C                   END
085000011122     C                   END
085100011123     c                   if        vtg3 <> *blanks
085200011122     c                   movel(P)  VTG3          parkey
085300011122     c                   exsr      srtab
085400011122     c                   movel     parkey        vtg3
085500011123     c                   if        *in99
085600011122     C                   SETON                                        69
085700011122     C                   leave
085800011122     C                   END
085900011122     C                   END
086000011220     C* DATA ultima REVISIONE
086100011220     C                   IF        vdur = 0 and §xadur = 'S'
086200011122     C                   SETON                                        7799
086300011122     C                   leave
086400011220     C                   End
086500011220     c                   if        vdur <> 0
086600011123     C     *EUR          test(d)                 vdur                   77
086700011122     c                   if        *in77
086800011122     C                   SETON                                        99
086900011122     C                   leave
087000011122     C                   END
087100011122     C                   END
087200000000     C*TIPO MANUTENZIONE
087300011126     c                   exsr      srvman
087400011126     C   99              leave
087500000000     C*COD.UTILIZZO
087600011123     c                   if        vuti <> *blanks
087700011126     c                   exsr      srvUTI
087800011126     C   99              leave
087900011122     C                   END
088000000000     C*COD.STATO
088100011123     c                   if        vsta <> *blanks
088200011126     c                   exsr      srvSTA
088300011126     C   99              leave
088400011122     C                   END
088500070925     c* se la società in dotazione è diversa dalla società dell'autista
088600070925     c* richiamo il pgm di manutenzione anagrafico autista
088700070925     c                   if        vsoc <> §xnsoc and §xnsoc <> ' '
088800070925     c                             and vsoc <> savsoc
088900070925     c                   movel(p)  'XN'          kpjbu
089000070925     c                   move      vcau          kpjbu
089100070925     c                   call      'TRMZ40R'
089200070925     c                   parm                    kpjba
089300070925     c                   movel     vsoc          savsoc            3
089400070925     c                   end
089500011122     C                   enddo
089600011122     c*
089700000000     C                   ENDSR
089800011126     c**********************************************************************
089900011126     C** ricerca  2 VIDEATA
090000011126     c**********************************************************************
090100011126     C     ric02         BEGSR
090200011123     c                   movel     '1'           fld
090300011123     c                   select
090400020607     C*CODICE società proprietaria
090500011123     C                   when      §fld = 'VDIT'
090600011123     C                   reset                   ana007ds
090700011123     C                   eval      opz007 = '01'
090800011123     C                   eval      kpjbu = ana007ds
090900011123     C                   call      'ANA007R'
091000011123     C                   parm                    kpjba
091100011123     C                   eval      ana007ds = kpjbu
091200011123     C                   if        err007 <> '1' and soc007 <> *blanks
091300011123     C                   eval      vdit = soc007
091400011219     c                   exsr      srvdit
091500011123     C                   endif
091600020607     C*CODICE FILIALE
091700020607     C                   when      §fld = 'VFIL'
091800020607     C                   MOVEL     *BLANKS       §COD1
091900020607     C                   MOVE      *BLANKS       §DES1
092000020607     C                   CALL      'TNSD19R'
092100020607     C                   PARM                    §COD1             3
092200020607     C                   PARM                    §TIP              1
092300020607     C                   PARM                    §DES1            25
092400020607     c                   if        §cod1 <> *blanks
092500020607     C                   MOVEL     §COD1         VFIL                           *CODICE
092600020607     C                   MOVEL     §DES1         VDFIL                          *DESCRIZIONE
092700020607     c                   end
092800020607     C*CODICE società in dotazione
092900020607     C                   when      §fld = 'VSOC'
093000020607     C                   reset                   ana007ds
093100020607     C                   eval      opz007 = '01'
093200020607     C                   eval      kpjbu = ana007ds
093300020607     C                   call      'ANA007R'
093400020607     C                   parm                    kpjba
093500020607     C                   eval      ana007ds = kpjbu
093600020607     C                   if        err007 <> '1' and soc007 <> *blanks
093700020607     C                   eval      vsoc = soc007
093800020607     c                   exsr      srvsoc
093900020607     C                   endif
094000011123     C*CODICE CDC
094100011123     C                   when      §fld = 'VCDC'
094200011123    >C                   RESET                   ANA032DS
094300011123    >C                   MOVEL     '01'          OPZ032
094400011123     C                   MOVE      'S'           FSC032
094500011123     C                   MOVE      'CDC'         TPE032
094600011123    >C                   MOVEL     ANA032Ds      KPJBU
094700011123    >C                   CALL      'ANA032R'
094800011123     C                   PARM                    KPJBA
094900011123    >C                   MOVEL     KPJBU         ANA032DS
095000011212     c                   if        ent032 <> *blanks
095100011123     C                   MOVEL     ENT032        VCDC
095200011212     c                   end
095300160516      *marca
095400160516     c                   when      §fld = 'VCAUT'
095500160516     c                   clear                   tibs02ds
095600160516     c                   move      'R'           t02mod
095700160516     c                   exsr      srvcaut
095800160516      *modello
095900160516     c                   when      §fld = 'VMAU'
096000160516     c                   clear                   tibs02ds
096100160516     c                   move      'R'           t02mod
096200160516     c                   exsr      srvmau
096300160516      *
096400011123     C*CATEGORIA
096500011123     C                   when      §fld = 'VCAT'
096600011126     c                   exsr      srvcat
096700011123     C*CODICE AUTISTA
096800011123     C                   when      §fld = 'VCAU'
096900011126     c                   exsr      srvcau
097000011123     C*FORNITORE
097100011123     C                   when      §fld = 'VFOR'
097200011126     c                   movel     vfor          vfors             8
097300011126     C                   RESET                   Ana201DS
097400011126     c                   movel     socxsc        soc201
097500011126     c                   movel(p)  'F'           kcc201
097600011126     C                   EVAL      Kpjbu = Ana201DS
097700011126     C                   CALL      'ANA201R'
097800011126     C                   PARM                    Kpjba
097900011126     C                   EVAL      Ana201DS = Kpjbu
098000011126     C                   IF        Err201 = '0' and ksc201 <> *blanks
098100011126     c                   movel     ksc201        vfor
098200011126     c                   EXSR      SRVFOR
098300011126     c                   else
098400011126     c                   movel     vfors         vfor
098500011126     c                   end
098600011123     C*CARROZZERIA
098700011123     C                   when      §fld = 'VCAR'
098800011126     c                   exsr      srvcar
098900011123     C*COD.CONSUMI
099000011123     C                   when      §fld = 'VCON'
099100011126     c                   exsr      srvcon
099200011123     C*CODICE SIT. PROPRIETA'
099300011123     C                   when      §fld = 'VCSP'
099400011126     c                   exsr      srvcsp
099500011123     C*TIPO GOMME
099600011123     C                   when      §fld = 'VTG1'
099700011123     c                   movel     'XG'          parcod
099800011126     c                   movel     vtg1          parkey
099900011123     c                   exsr      srtab
100000011123     c                   movel     parkey        vtg1
100100011123     C                   when      §fld = 'VTG2'
100200011123     c                   movel     'XG'          parcod
100300011126     c                   movel     vtg2          parkey
100400011123     c                   exsr      srtab
100500011123     c                   movel     parkey        vtg2
100600011123     C                   when      §fld = 'VTG3'
100700011123     c                   movel     'XG'          parcod
100800011126     c                   movel     vtg3          parkey
100900011123     c                   exsr      srtab
101000011123     c                   movel     parkey        vtg3
101100011123     C*TIPO MANUTENZIONE
101200011123     C                   when      §fld = 'VMAN'
101300011126     c                   exsr      srvman
101400011123     C*COD.UTILIZZO
101500011123     C                   when      §fld = 'VUTI'
101600011126     c                   exsr      srvuti
101700011123     C*COD.STATO
101800011123     C                   when      §fld = 'VSTA'
101900011126     c                   exsr      srvsta
102000011123     C                   endsl
102100011123     C* POSIZIONAMENTO CURSORE
102200011123     C                   Z-ADD     §RIGA         §RIG
102300011123     C                   Z-ADD     §COLON        §COL
102400011123     c*
102500011123     C                   ENDSR
102600011122     C**********************************************************************
102700000000     C** CONTROLLI 3 VIDEATA
102800011122     C**********************************************************************
102900011122     C     ctr03         BEGSR
103000011123     c                   movel     ' '           fld
103100011122     c                   setoff                                       99
103200011122     c                   do
103300000000     C* TASSA CIRCOLAZIONE
103400011220     C                   IF        vdtc = 0 and §xadtc = 'S'
103500011122     C                   SETON                                        8999
103600011122     C                   leave
103700011220     C                   End
103800011220     C                   IF        vdtc <> 0
103900011126     C     *EUR          test(d)                 vdtc                   89
104000011122     c                   if        *in89
104100011122     C                   SETON                                        99
104200011122     C                   leave
104300011122     C                   END
104400011122     C                   END
104500011123     c* periodicità tassa circolazione
104600011123     c                   if        vpbl = *blanks
104700011123     c                   seton                                        9199
104800011123     c                   leave
104900011123     c                   end
105000000000     C* TASSA PUBBLICITA'
105100011126     C     vdtP          IFne      0
105200011126     C     *EUR          test(d)                 vdtp                   97
105300011122     c                   if        *in97
105400011122     C                   SETON                                        99
105500011122     C                   leave
105600011122     C                   END
105700011122     C                   END
105800031127     c* equipaggiamento
105900031127     c                   if        vequi <> ' '
106000031127     c                   exsr      srvequi
106100031127     c                   end
106200000000     C* DATA E COMPAGNIA ASS. RC AUTO
106300011126     C                   EXSR      SRVCRC
106400011126     C   99              leave
106500011122     C     vdrc          IFne      0
106600011126     C     *EUR          test(d)                 vdrc                   92
106700011122     c                   if        *in92
106800011122     C                   SETON                                        99
106900011122     C                   leave
107000011122     C                   END
107100011122     C                   END
107200000000     C* DATA E COMPAGNIA ASS. INCENDIO/FURTO
107300011123     c                   if        vcfi <> *blanks
107400011126     C                   EXSR      SRVCFI
107500011126     C   99              leave
107600020128     C                   else
107700020128     C                   MOVEL     *blanks       VDcfi
107800020128     C                   END
107900000000     C   43              Z-ADD     VDRC          VDFI
108000020128     C                   IF        vdfi = 0 and vcfi <> *blanks and
108100020128     c                             §xdass <> 'N'
108200011122     C                   SETON                                        9399
108300011122     C                   leave
108400020128     C                   end
108500020128     C                   IF        vdfi <> 0
108600020128     c                   if        vcfi = *blanks
108700020128     C                   SETON                                        7599
108800020128     C                   leave
108900020128     C                   end
109000011123     C     *EUR          test(d)                 vdfi                   93
109100011122     c                   if        *in93
109200011122     C                   SETON                                        99
109300011122     C                   leave
109400011122     C                   END
109500000000     C                   END
109600000000     C* ASS.C/TERZI
109700011122     C     VNAC          IFne      *BLANKS
109800011122     C     VDAC          orne      0
109900011122     C     VSAC          orne      0
110000000000      *
110100000000     C     VNAC          IFEQ      *BLANKS
110200000000     C                   SETON                                        7399
110300011122     C                   leave
110400000000     C                   END
110500000000     C     VDAC          IFEQ      0
110600000000     C                   SETON                                        9599
110700011122     C                   leave
110800000000     C                   ELSE
110900011123     C     *EUR          test(d)                 vdac                   95
111000011122     c                   if        *in95
111100011122     C                   SETON                                        99
111200011122     C                   leave
111300011122     C                   END
111400000000     C                   END
111500000000     C     VSAC          IFEQ      0
111600000000     C                   SETON                                        9699
111700011122     C                   leave
111800000000     C                   ELSE
111900011123     C     *EUR          test(d)                 vsac                   96
112000011122     c                   if        *in96
112100011122     C                   SETON                                        99
112200011122     C                   leave
112300011122     C                   END
112400000000     C                   END
112500011122     C                   end
112600011123     c* tipo tariffa
112700011123     c                   if        vtta = *blanks
112800011123     c                   seton                                        9899
112900011123     c                   leave
113000011123     c                   end
113100000000     C*CODICE TARIFFA
113200011126     C                   EXSR      SRVCTA
113300011126     C   99              leave
113400000000     C     DATFIS        IFEQ      'QL'
113500000000     C     VPQL          ANDEQ     0
113600000000     C                   SETON                                        6299
113700011122     C                   leave
113800000000     C                   END
113900000000     C     DATFIS        IFEQ      'CV'
114000000000     C     VCVF          ANDEQ     0
114100000000     C                   SETON                                        6299
114200011122     C                   leave
114300000000     C                   END
114400011122     C                   enddo
114500011122     c*
114600011122     c                   endsr
114700011123     C**********************************************************************
114800011123     C** RICERCA 3 VIDEATA
114900011123     C**********************************************************************
115000011123     C     RIC03         BEGSR
115100011123     c                   movel     '1'           fld
115200011123     c                   SELECT
115300011123     C* COMPAGNIA ASS. RC AUTO
115400011123     C                   WHEN      §fld = 'VCRC'
115500011126     c                   exsr      srvcrc
115600011123     C* COMPAGNIA ASS. INCENDIO/FURTO
115700011123     C                   WHEN      §fld = 'VCFI'
115800031127     C* equipaggiamento
115900031127     C                   WHEN      §fld = 'VEQUI'
116000031127     c                   exsr      srvequi
116100011123     C*CODICE TARIFFA
116200011123     C                   WHEN      §fld = 'VCTA'
116300011126     c                   exsr      srvcta
116400011123     C                   endSL
116500011123     C* POSIZIONAMENTO CURSORE
116600011123     C                   Z-ADD     §RIGA         §RIG
116700011123     C                   Z-ADD     §COLON        §COL
116800011123     c*
116900011123     c                   endsr
117000011126     c**********************************************************************
117100011126     C** fornitore
117200011126     c**********************************************************************
117300011126     C     srvfor        BEGSR
117400011126     c*
117500011126     c     KFOR          CHAIN     ANFRN01L                           42
117600011126     C                   IF        NOT *IN42
117700011126     c     FRNSOGG       CHAIN     ANSOG01L                           42
117800011126     C                   END
117900011126     C                   IF        NOT *IN42
118000011126     C                   MOVEL(P)  SOGDES        VDFOR
118100011126     C                   ELSE
118200011126     C                   CLEAR                   VDFOR
118300011126     c                   end
118400011126     c*
118500011126     C                   ENDSR
118600020607     c**********************************************************************
118700020607     C** società proprietaria
118800020607     c**********************************************************************
118900020607     C     srvdit        BEGSR
119000020607     c*
119100020607     C     vdit          CHAIN     ansif01L
119200020607     C                   if        not %found(ansif01l)
119300020607     C                   SETON                                        8799
119400020607     c                   movel     *blanks       vddit
119500020607     c                   else
119600020607     c                   movel     sifdesbrev    vddit
119700020607     C                   END
119800020607     c*
119900020607     C                   ENDSR
120000011126     c**********************************************************************
120100011126     C** FILIALE
120200011126     c**********************************************************************
120300011126     C     srvfil        BEGSR
120400011126     c*
120500011126     C                   MOVE      VFIL          FILV              3 0
120600011126     C     FILV          CHAIN     AZORG
120700011126     C                   if        not %found(azorg01l)
120800011126     C                   SETON                                        8899
120900011126     c                   else
121000011126     C                   MOVEL     ORGDES        VDFIL
121100011126     C                   END
121200011126     c*
121300011126     C                   ENDSR
121400020607     c**********************************************************************
121500020607     C** società in dotazione
121600020607     c**********************************************************************
121700020607     C     srvsoc        BEGSR
121800020607     c*
121900020607     C     vsoc          CHAIN     ansif01L
122000020607     C                   if        not %found(ansif01l)
122100020607     C                   SETON                                        6099
122200020607     c                   movel     *blanks       vdsoc
122300020607     c                   else
122400020607     c                   movel     sifdesbrev    vdsoc
122500020607     C                   END
122600020607     c*
122700020607     C                   ENDSR
122800011126     c**********************************************************************
122900011126     C** centro di costo
123000011126     c**********************************************************************
123100011126     C     srvcdc        BEGSR
123200011126     c*
123300011126     C                   movel     *blanks       VDCDC
123400011126     C                   movel     VCDC          ententita
123500011126     C     kent          chain     anent01l
123600011126     C                   IF        NOT %FOUND(ANENT01L)
123700011126     C                   SETON                                        6199
123800011126     C                   ELSE
123900011126     C                   move      entdesbrev    VDCDC
124000011126     C                   END
124100011126     c*
124200011126     C                   ENDSR
124300011126     c**********************************************************************
124400011126     C** categoria
124500011126     c**********************************************************************
124600011126     C     srvcat        BEGSR
124700011126     c*
124800011126     c                   movel     'XA'          parcod            2
124900011126     c                   movel(P)  VCAT          parkey
125000011126     c                   exsr      srtab
125100011126     c                   movel     parkey        vcat
125200011126     c                   if        *in99
125300011220     C                   MOVEL     *blanks       dsxa
125400011220     C                   MOVEL     *blanks       VDCAT
125500011126     C                   SETON                                        85
125600011126     C                   else
125700011220     C                   MOVEL     TBLUNI        dsxa
125800011220     C                   MOVEL     §xades        VDCAT
125900011126     C                   END
126000011126     c*
126100011126     C                   ENDSR
126200160516     c**********************************************************************
126300160516     C** controllo marca
126400160516     c**********************************************************************
126500160516     C     srvcaut       BEGSR
126600160516     c*
126700160516     c                   if        vcaut <> *blank and t02mod <> 'R'
126800160516     c                   movel     vcaut         t02ke1
126900160516     c                   endif
127000160516     c                   movel     knsif         t02sif
127100160516     c                   movel     'CAU'         t02cod
127200160516     c                   call      'TIBS02R'
127300160516     c                   parm                    KPJBA
127400160516     c                   parm                    TIBS02DS
127500160516     c                   if        t02err =  *blanks
127600160516     c                   movel     t02uni        dcau
127700160516     c                   movel     t02ke1        vcaut
127800160516     c                   movel     §caudes       vcautd
127900160516     c                   else
128000160516     c                   seton                                        8199
128100160516     c                   endif
128200160516     C                   ENDSR
128300160516     c**********************************************************************
128400160516     C** controllo modello
128500160516     c**********************************************************************
128600160516     C     srvmau        BEGSR
128700160516     c*
128800160516     c                   if        vmau <> *blank and t02mod <> 'R'
128900160516     c                   movel     vmau          t02ke1
129000160516     c                   endif
129100160516     c                   movel     knsif         t02sif
129200160516     c                   movel     'MAU'         t02cod
129300160516     c                   call      'TIBS02R'
129400160516     c                   parm                    KPJBA
129500160516     c                   parm                    TIBS02DS
129600160516     c                   if        t02err =  *blanks
129700160516     c                   movel     t02uni        dmau
129800160516     c                   movel     t02ke1        vmau
129900160516     c                   movel     §maudes       vmaud
130000160516     c                   else
130100160516     c                   seton                                        8099
130200160516     c                   endif
130300160516     c*
130400160516     C                   ENDSR
130500011126     c**********************************************************************
130600011126     C** autista
130700011126     c**********************************************************************
130800011126     C     srvcau        BEGSR
130900011126     c                   movel     'XN'          parcod
131000011126     c                   movel(P)  VCAU          parkey
131100011126     c                   exsr      srtab
131200011126     c                   movel     parkey        vcau
131300011126     c                   if        *in99
131400011126     C                   MOVEL     *blanks       VDAU
131500070925     c                   clear                   dsxn
131600011126     C                   SETON                                        55
131700011126     C                   else
131800050216     C                   MOVEL     TBLUNI        dsxn
131900050216     C                   MOVEL     §XNdes        VDAU
132000050216     C                   MOVEL     §XNFUN        Vfun
132100011126     C                   END
132200011126     c*
132300011126     C                   ENDSR
132400011126     c**********************************************************************
132500011126     C** carrozzeria
132600011126     c**********************************************************************
132700011126     C     srvcar        BEGSR
132800011126     c*
132900011126     c                   movel(P)  VCAR          parkey
133000011126     c                   movel     'XB'          parcod
133100011126     c                   exsr      srtab
133200011126     c                   movel     parkey        vcar
133300011126     c                   if        *in99
133400011126     C                   MOVEL     *blanks       VDCAR
133500011126     C                   SETON                                        86
133600011126     C                   else
133700011126     C                   MOVEL     TBLUNI        VDCAR
133800011126     C                   END
133900011126     c*
134000011126     C                   ENDSR
134100011126     c**********************************************************************
134200011126     C** consumi
134300011126     c**********************************************************************
134400011126     C     srvcon        BEGSR
134500011126     c*
134600011207     C                   clear                   dsxh
134700011126     c                   movel     'XH'          parcod
134800011126     c                   movel(P)  VCON          parkey
134900011126     c                   exsr      srtab
135000011126     c                   movel     parkey        vcon
135100011126     c                   if        *in99
135200011126     C                   SETON                                        44
135300011126     C                   else
135400011207     C                   MOVEL     TBLUNI        dsxh
135500011126     C                   END
135600011207     c                   movel     §xhdes        vdcon
135700011207     c                   z-add     §xhmed        vmed
135800011126     c*
135900011126     C                   ENDSR
136000011126     c**********************************************************************
136100011126     C** tipo proprietà
136200011126     c**********************************************************************
136300011126     C     srvcsp        BEGSR
136400011126     c*
136500011126     c                   movel     'XR'          parcod
136600011126     c                   movel(P)  VCSP          parkey
136700011126     c                   exsr      srtab
136800011126     c                   movel     parkey        vcsp
136900011126     c                   if        *in99
137000011126     C                   MOVEL     *blanks       VDSP
137100011126     C                   SETON                                        56
137200011126     C                   else
137300011126     C                   MOVEL     TBLUNI        VDSP
137400011126     C                   END
137500011126     c*
137600011126     C                   ENDSR
137700011126     c**********************************************************************
137800011126     C** tipo manutenzione
137900011126     c**********************************************************************
138000011126     C     srvman        BEGSR
138100011126     c*
138200011126     c                   movel     'XC'          parcod
138300011126     c                   movel(P)  VMAN          parkey
138400011126     c                   exsr      srtab
138500011126     c                   movel     parkey        vman
138600011126     c                   if        *in99
138700011126     C                   MOVEL     *blanks       VDMAN
138800011126     C                   SETON                                        78
138900011126     C                   else
139000011126     C                   MOVEL     TBLUNI        VDMAN
139100011126     C                   END
139200011126     c*
139300011126     C                   ENDSR
139400011126     c**********************************************************************
139500011126     C** utilizzo
139600011126     c**********************************************************************
139700011126     C     srvuti        BEGSR
139800011126     c*
139900011126     c                   movel     'XL'          parcod
140000011126     c                   movel(P)  VUTI          parkey
140100011126     c                   exsr      srtab
140200011126     c                   movel     parkey        vuti
140300011126     c                   if        *in99
140400011126     C                   MOVEL     *blanks       VDES
140500011126     C                   SETON                                        45
140600011126     C                   else
140700011126     C                   MOVEL     TBLUNI        VDES
140800011126     C                   END
140900011126     c*
141000011126     C                   ENDSR
141100011126     c**********************************************************************
141200011126     C** stato
141300011126     c**********************************************************************
141400011126     C     srvsta        BEGSR
141500011126     c*
141600011126     c                   movel     'XF'          parcod
141700011126     c                   movel(P)  vsta          parkey
141800011126     c                   exsr      srtab
141900011126     c                   movel     parkey        vsta
142000011126     c                   if        *in99
142100011126     C                   MOVEL     *blanks       VDsta
142200011126     C                   SETON                                        83
142300011126     C                   else
142400011126     C                   MOVEL     TBLUNI        VDsta
142500011126     C                   END
142600011126     c*
142700011126     C                   ENDSR
142800011126     c**********************************************************************
142900011126     C** ass.ne RCA auto
143000011126     c**********************************************************************
143100011126     C     srvcrc        BEGSR
143200011126     c*
143300011126     c                   movel     'XD'          parcod
143400011126     c                   movel(P)  vcrc          parkey
143500011126     c                   exsr      srtab
143600011126     c                   movel     parkey        vcrc
143700011126     c                   if        *in99
143800011126     C                   MOVEL     *blanks       VDcrc
143900011126     C                   SETON                                        74
144000011126     C                   else
144100011126     C                   MOVEL     TBLUNI        VDcrc
144200011126     C                   END
144300011126     c*
144400011126     C                   ENDSR
144500031127     c**********************************************************************
144600031127     C** equipaggiamento
144700031127     c**********************************************************************
144800031127     C     srvequi       BEGSR
144900031127     c*
145000031127     c                   movel     'XT'          parcod
145100031127     c                   exsr      srtabequi
145200031127     c                   movel     parkey        vequi
145300031128     C                   MOVEL     *blanks       VDequ1
145400031128     C                   MOVEL     *blanks       VDequ2
145500031127     c                   if        *in99
145600031127     C                   SETON                                        76
145700031127     C                   else
145800031128     C                   MOVEL     des1          VDequ1
145900031128     C                   MOVEL     des2          VDequ2
146000031127     C                   END
146100031127     c*
146200031127     C                   ENDSR
146300011126     c**********************************************************************
146400011126     C** ass.ne inc/furto
146500011126     c**********************************************************************
146600011126     C     srvcfi        BEGSR
146700011126     c*
146800011126     c                   movel     'XD'          parcod
146900011126     c                   movel(P)  vcfi          parkey
147000011126     c                   exsr      srtab
147100011126     c                   movel     parkey        vcfi
147200011126     c                   if        *in99
147300020111     c                   clear                   dsxd
147400011126     C                   MOVEL     *blanks       VDcfi
147500011126     C                   SETON                                        75
147600011126     C                   else
147700020111     C                   MOVEL     TBLUNI        dsxd
147800020111     C                   MOVEL     §xddes        vdcfi
147900011126     C                   END
148000011126     c*
148100011126     C                   ENDSR
148200011126     c**********************************************************************
148300011126     C** tariffa
148400011126     c**********************************************************************
148500011126     C     srvcta        BEGSR
148600011126     c*
148700011126     c                   movel     'XI'          parcod
148800011126     c                   movel(P)  vcta          parkey
148900011126     c                   exsr      srtab
149000011126     c                   movel     parkey        vcta
149100011126     c                   if        *in99
149200011126     C                   MOVEL     *blanks       dstar
149300011126     C                   SETON                                        63
149400011126     C                   else
149500011126     C                   MOVEL     TBLUNI        dstar
149600011126     C                   END
149700011126     c*
149800011126     C                   ENDSR
149900011123     C**********************************************************************
150000011123     C** Controlli e decodifiche campi con tabel00f
150100011123     C**********************************************************************
150200011123     C     srtab         BEGSR
150300011123     c                   if        fld <> *blanks
150400011126     c                   movel     parkey        parkeys           8
150500011123     C                   CALL      'TRUL19R'
150600011123     c                   parm                    parcod            2
150700011123     c                   parm      '1'           parord            1
150800011123     c                   parm      *blanks       parkey            8
150900011126     c                   parm      *blanks       cmd               1
151000011126     c* ritorno senza scelta
151100011126     c                   if        cmd = 'L'
151200011126     c* in aggiornamento non è possibile cambiare l'utista
151300011126     c                             or (§fld = 'VCAU' and *in84)
151400011126     c                   movel     parkeys       parkey
151500011123     C                   END
151600011126     C                   END
151700011123     C                   MOVEL     *blanks       tbluni
151800011123     C                   MOVEL     parcod        COD
151900011123     C                   MOVEL(p)  parkey        KEY
152000011123     C     KTAB          CHAIN     TABEL                              99
152100011123     c*
152200011123     C                   ENDSR
152300031127     C**********************************************************************
152400031127     C** Controlli e decodifiche equipaggiamento
152500031127     C**********************************************************************
152600031127     C     srtabequi     BEGSR
152700031127     c                   if        fld = *blanks
152800031127     c                   eval      tipo = 'C'
152900031128     c                   movel(P)  vequi         parkey
153000031127     c                   else
153100031127     c                   eval      tipo = 'R'
153200031128     c                   clear                   parkey
153300031127     c                   end
153400031127     C                   CALL      'TRMZ46R'
153500031127     c                   parm                    tipo              1
153600031128     c                   parm                    parkey
153700031127     c                   parm      *blanks       des1             65
153800031127     c                   parm      *blanks       des2             65
153900031128     c                   if        parkey <> ' '
154000031127     C                   MOVEL     *blanks       tbluni
154100031127     C                   MOVEL     'XT'          COD
154200031127     C                   MOVEL(p)  parkey        KEY
154300031127     C     KTAB          CHAIN     TABEL                              99
154400031128     c                   end
154500031127     c*
154600031127     C                   ENDSR
154700011123     c**********************************************************************
154800011123     c* pulisce 1° formato
154900011123     c**********************************************************************
155000011123     C     clr00         BEGSR
155100011123     C                   Z-ADD     0             §RIGA
155200011123     C                   Z-ADD     0             §COLON
155300011123     C                   CLEAR                   §FLD
155400011123     C                   ENDSR
155500011123     c**********************************************************************
155600011123     c* pulisce 1° formato
155700011123     c**********************************************************************
155800011123     C     clr01         BEGSR
155900011126     c                   exsr      clr00
156000011123     C                   Z-ADD     0             §rig
156100011123     C                   Z-ADD     0             §col
156200011123     C                   Z-ADD     0             VNRF
156300070925     C                   MOVEL     *BLANKS       savsoc
156400070925     C                   MOVEL     *BLANKS       Vcau
156500011218     C                   MOVEL     *BLANKS       VTGA
156600011123     C                   MOVEL     *BLANKS       VTGN
156700011123     C                   MOVEL     *BLANKS       VTLA
156800160805     C                   MOVEL     *BLANKS       VTel
156900011123     C                   ENDSR
157000011126     c**********************************************************************
157100011126     c* pulisce 2° formato
157200011126     c**********************************************************************
157300011126     C     clr03         BEGSR
157400011126     C                   Z-ADD     0             §rig
157500011126     C                   Z-ADD     0             §col
157600011126     c                   exsr      clr00
157700011126     C                   ENDSR
157800011122     c**********************************************************************
157900011122     C*PULIZIA DEI CAMPI A VIDEO
158000011122     c**********************************************************************
158100011122     c     clr02         begsr
158200011126     c                   exsr      clr03
158300011122     C                   MOVEL     *BLANKS       VTLA
158400011122     C                   MOVEL     *BLANKS       VSTA
158500011122     C                   MOVEL     *BLANKS       VDSTA
158600011122     C                   MOVEL     *BLANKS       VTGA
158700011122     C                   MOVEL     *BLANKS       VTGN
158800011122     C                   MOVEL     *BLANKS       VCAT
158900011122     C                   MOVEL     *BLANKS       VDCAT
159000160516     C*                  MOVEL     *BLANKS       VMAR
159100160516     C*                  MOVEL     *BLANKS       VTIP
159200160516     C                   MOVEL     *BLANKS       VMAu
159300160516     C                   MOVEL     *BLANKS       Vcaut
159400011122RE   C                   MOVEL     *BLANKS       VCIL
159500011122     C                   MOVEL     *BLANKS       VCAR
159600011122     C                   MOVEL     *BLANKS       VDCAR
159700011122     C                   Z-ADD     0             VIMM
159800011220     C                   MOVE      udate         dataiso
159900011220     C                   move      dataiso       DATAeur
160000011220     C                   move      dataeur       DATAUT
160100011122     C                   MOVEL     *BLANKS       VMAN
160200011122     C                   MOVEL     *BLANKS       VDMAN
160300011122     C                   MOVEL     *BLANKS       VUTI
160400011122     C                   MOVEL     *BLANKS       VDES
160500011122     C                   MOVE      *BLANKS       VDSP
160600011122     C                   MOVE      *BLANKS       VCSP
160700011122     C                   MOVE      *BLANKS       VCAU
160800011122     C                   MOVE      *BLANKS       VDAU
160900011122     C                   Z-ADD     0             VSCAD
161000011122     C                   Z-ADD     0             VASS
161100011122     C                   MOVEL     *BLANKS       VALI
161200011122     C                   MOVEL     *BLANKS       Vcon
161300011122     C                   MOVEL     *BLANKS       Vdcon
161400011207     C                   z-add     0             Vmed
161500011122     C                   Z-ADD     0             VTAR
161600011122     C                   Z-ADD     0             VPPC
161700011122     C                   Z-ADD     0             VPTR
161800011122     C                   Z-ADD     0             VNGM
161900011122     C                   MOVEL     *BLANKS       VTG1
162000011122     C                   MOVEL     *BLANKS       VTG2
162100011122     C                   MOVEL     *BLANKS       VTG3
162200011123     c                   movel     *blanks       vDIT
162300011219     c                   movel     *blanks       vdDIT
162400011122     C                   MOVEL     *BLANKS       VFIL
162500011126     C                   MOVEL     *BLANKS       VFOR
162600020607     c                   movel     *blanks       vsoc
162700020607     c                   movel     *blanks       vdsoc
162800011126     C                   MOVEL     *BLANKS       VDFOR
162900011122     C                   MOVEL     *BLANKS       VDFIL
163000011122     C                   MOVEL     *BLANKS       VCDC
163100011122     C                   MOVEL     *BLANKS       VDCDC
163200031028     C                   Z-ADD     0             telev
163300031028     C                   Z-ADD     0             VDUR
163400011122     C                   Z-ADD     0             VDTC
163500011122     C                   Z-ADD     0             VDTP
163600011122     C                   Z-ADD     0             VDRC
163700011122     C                   MOVEL     *BLANKS       VCRC
163800011122     C                   MOVEL     *BLANKS       VDCRC
163900031127     C                   MOVEL     *BLANKS       Vequi
164000031128     C                   MOVEL     *BLANKS       VDequ1
164100031128     C                   MOVEL     *BLANKS       VDequ2
164200011122     C                   Z-ADD     0             VDFI
164300011122     C                   MOVEL     *BLANKS       VCFI
164400011122     C                   MOVEL     *BLANKS       VDCFI
164500011122     C                   MOVEL     *BLANKS       VNAC
164600011122     C                   Z-ADD     0             VDAC
164700011122     C                   Z-ADD     0             VSAC
164800011122     C                   Z-ADD     0             VPQL
164900011122     C                   MOVEL     '1'           VTTA
165000011123     C                   z-add     0             VCTA
165100011122     C                   Z-ADD     0             VCVF
165200011122     C                   MOVEL     *BLANKS       VDCTA
165300011122     C                   MOVEL     *BLANKS       VPBL
165400011122     C                   Z-ADD     0             VIMT
165500011122     C                   Z-ADD     0             VIMP
165600011122     C                   Z-ADD     0             VIMA
165700011122     C                   Z-ADD     0             VIMF
165800011122     C                   Z-ADD     0             VIMK
165900000000     C                   ENDSR
166000011122     C**********************************************************************
166100000000     C** AGGIORNAMENTO O INSERIMENTO
166200011122     C**********************************************************************
166300000000     C     AGGIOR        BEGSR
166400011122     C     KGAAM         CHAIN     mzAME000                           21
166500000000     C                   Z-ADD     0             AMEDAC
166600000000     C                   Z-ADD     0             AMESAC
166700000000     C                   Z-ADD     0             AMEDUR
166800000000     C                   Z-ADD     0             AMEDTC
166900000000     C                   Z-ADD     0             AMEDTP
167000000000     C                   Z-ADD     0             AMEDRC
167100000000     C                   Z-ADD     0             AMEDFI
167200000000     C                   Z-ADD     VNRF          AMENRF
167300000000     C                   MOVEL     VTLA          AMETLA
167400000000     C                   MOVEL     VTGA          AMETGA
167500000000     C                   MOVEL     VTGN          AMETGN
167600000000     C                   MOVEL     VSTA          AMESTA
167700000000     C                   MOVEL     VCAT          AMECAT
167800160516     C*                  MOVEL     VMAR          AMEMAR
167900160516     C*                  MOVEL     VTIP          AMETIP
168000160516     C                   MOVEL     Vmau          AMEmau
168100160516     C                   MOVEL     Vcaut         AMEcaut
168200970416RE   C                   MOVEL     VCIL          AMECIL
168300000000     C                   MOVEL     VCAR          AMECAR
168400910215     C                   Z-ADD     MES           ANTO
168500910215     C                   Z-ADD     ANN           MES
168600910215     C                   Z-ADD     ANTO          ANN
168700000000     C                   Z-ADD     VIMM          AMEIMM
168800000000     C                   MOVEL     VMAN          AMEMAN
168900000000     C                   MOVEL     VUTI          AMEUTI
169000000000     C                   Z-ADD     VASS          AMEASS
169100011122     C                   MOVEL     Vcon          AMEusi
169200011122     C                   MOVEL     VALI          AMEALI
169300000000     C                   Z-ADD     VTAR          AMETAR
169400000000     C                   Z-ADD     VPPC          AMEPPC
169500000000     C                   Z-ADD     VPQL          AMEPQL
169600000000     C                   Z-ADD     VPTR          AMEPTR
169700000000     C                   Z-ADD     VNGM          AMENGM
169800000000     C                   MOVEL     VTG1          AMETG1
169900000000     C                   MOVEL     VTG2          AMETG2
170000000000     C                   MOVEL     VTG3          AMETG3
170100011123     C                   movel     VDIT          AMEDIT
170200020607     C                   movel     Vsoc          AMEsoc
170300031029     C                   MOVEl     telev         AMEtel
170400031028     C                   MOVEL     VFIL          AMEFIL
170500000000     C                   MOVEL     VCDC          AMECDC
170600011123     C                   MOVEL     Vfor          AMEfor
170700000000     C*
170800000000     C     VDUR          IFNE      0
170900011122     C                   move      VDUR          DATAeur
171000011122     C                   MOVE      DATAeur       dataiso
171100011122     C                   MOVE      DATAIso       AMEDUR
171200000000     C                   END
171300000000     C*
171400000000     C     VDTC          IFNE      0
171500011122     C                   move      VDtc          DATAeur
171600011122     C                   MOVE      DATAeur       dataiso
171700011122     C                   MOVE      DATAIso       AMEDtc
171800000223     C                   END
171900000000     C                   Z-ADD     VIMT          AMEIMT
172000000000     C*
172100000000     C     VDTP          IFNE      0
172200011122     C                   move      VDtp          DATAeur
172300011122     C                   MOVE      DATAeur       dataiso
172400011122     C                   MOVE      DATAIso       AMEDtp
172500000223     C                   END
172600000000     C                   Z-ADD     VIMP          AMEIMP
172700000000     C*
172800000000     C     VDRC          IFNE      0
172900011122     C                   move      VDrc          DATAeur
173000011122     C                   MOVE      DATAeur       dataiso
173100011122     C                   MOVE      DATAIso       AMEDrc
173200000223     C                   END
173300000000     C                   MOVEL     VCRC          AMECRC
173400000000     C                   Z-ADD     VIMA          AMEIMA
173500031127     C                   MOVEL     Vequi         AMEequ
173600000000     C*
173700000000     C     VDFI          IFNE      0
173800011122     C                   move      VDfi          DATAeur
173900011122     C                   MOVE      DATAeur       dataiso
174000011122     C                   MOVE      DATAIso       AMEDfi
174100000000     C                   END
174200000000     C                   MOVEL     VCFI          AMECFI
174300000000     C                   Z-ADD     VIMF          AMEIMF
174400000000     C                   Z-ADD     VIMK          AMEIMK
174500000000     C                   MOVEL     VNAC          AMENAC
174600000000     C*
174700000000     C     VDAC          IFNE      0
174800011122     C                   move      VDac          DATAeur
174900011122     C                   MOVE      DATAeur       dataiso
175000011122     C                   MOVE      DATAIso       AMEDac
175100000000     C                   END
175200000000     C*
175300000000     C     VSAC          IFNE      0
175400011122     C                   move      Vsac          DATAeur
175500011122     C                   MOVE      DATAeur       dataiso
175600011122     C                   MOVE      DATAIso       AMEsac
175700000000     C                   END
175800910214     C     VSCAD         IFNE      0
175900011122     C                   move      Vscad         DATAeur
176000011122     C                   MOVE      DATAeur       dataiso
176100011122     C                   MOVE      DATAIso       AMEDsp
176200910214     C                   END
176300910215     C     DATAUT        IFNE      0
176400011122     C                   move      dataut        DATAeur
176500011122     C                   MOVE      DATAeur       dataiso
176600011122     C                   MOVE      DATAIso       AMEDal
176700910215     C                   END
176800910214     C                   MOVEL     VCSP          AMECSP
176900910214     C                   MOVEL     VCAU          AMECAU
177000000000     C                   MOVEL     VTTA          AMETTA
177100011123     C                   z-add     VCTA          AMECTA
177200000000     C                   Z-ADD     VCVF          AMECVF
177300000000     C                   MOVEL     VPBL          AMEPBL
177400011122     C   21              WRITE     mzAME000
177500011122     C  N21              UPDATE    mzAME000
177600011123     c*
177700011123     c                   exsr      clr01
177800011123     c*
177900000000     C                   ENDSR
178000011121     C*---------------------------------------------------------------*
178100011121     C     *inzsr        BEGSR
178200011121     C*---------------------------------------------------------------*
178300011121     C     *ENTRY        PLIST
178400011121     C                   PARM                    KPJBA
178500011122     c                   do
178600011121     C                   Z-ADD     1             CODUT
178700011121     C                   CALL      'XPARUT'
178800011121     C                   PARM                    UTEDSE
178900011122     C     CODUT         ifeq      -1
179000011122     c                   seton                                        lr
179100011122     c                   leave
179200011122     c                   end
179300011121     C                   MOVEL     RAGUT         RSUT             20
179400011122     C                   MOVEL     RAGUT         RSUT2            25
179500011221     C* CONTROLLO SE GESTIRE SOLO LA VISUALIZZAZIONE DEI RECORD IN BASE
179600011221     C* AL CODICE DELL'AZIONE
179700011221     C     KCDAZ         COMP      'MZ92'                                 20
179800011122      * Reperimento dati società
179900011122     C                   clear                   SocXsc
180000011122     C                   eval      tipxsc = 'SOC001'
180100011122     C                   exsr      rtvsocieta
180200011122     C                   if        rtnxsc = '1'
180300011122     C                   seton                                        lr
180400011122     c                   leave
180500011122     C                   else
180600011122     C                   MOVEL     XSOCDS        xsoc001ds
180700011122     C                   endif
180800011121     C* POSIZIONAMENTO TABELLE                             *
180900011121     C     KTAB          KLIST                                                  *
181000011121     C                   KFLD                    CODUT                          *
181100011121     C                   KFLD                    COD               2            *
181200011121     C                   KFLD                    KEY               8            *
181300011123     C* FORNITORE
181400011123     C     KFOR          KLIST                                                  *
181500011123     C                   KFLD                    VDIT                           *
181600011123     C                   KFLD                    VFOR                           *
181700011123     C                   KFLD                    FILLL                          *
181800011121     C* POSIZIONAMENTO X N.RIFERIMENTO                     *
181900011121     C     KGAAM         KLIST                                                  *
182000011121     C                   KFLD                    VNRF                           *
182100011121     C* POSIZIONAMENTO X N.DI TARGA                        *
182200011121     C     KTAR          KLIST                                                  *
182300011121     C                   KFLD                    VTGA                           *
182400011121     C                   KFLD                    VTGN                           *
182500011122     C* POSIZIONAMENTO SU ANENT                            *
182600011122     C     Kent          KLIST
182700011122     C                   KFLD                    xscsoc
182800011122     C                   KFLD                    enttprifan
182900011122     C                   KFLD                    ententita
183000011122     C                   move      'CDC'         enttprifan
183100011121     C*----------------------------------------------------*
183200011121     C* RICERCA TARGA X CITTA'
183300011121     C     RICTAR        PLIST
183400011121     C                   PARM                    CODUT
183500011121     C                   PARM                    RSUT
183600011121     C                   PARM                    VTGA
183700011121     C                   PARM                    VTGN
183800011122     c                   enddo
183900011122     c*
184000011121     C                   ENDSR
184100011122     C*---------------------------------------------------------------------
184200011122     c*  Reperisco società.
184300011122     C*---------------------------------------------------------------------
184400011122     C     rtvsocieta    begsr
184500011122     C*
184600011122     C                   CALLb     'XSOC'
184700011122   >>C                   Parm                    TipXsc            6
184800011122   >>C                   Parm                    SocXsc            3
184900011122   >>C                   Parm                    CdsXsc            9 0
185000011122   >>C                   Parm                    ModXsc            3
185100011122   >>C                   Parm      *BLANK        RtnXsc            1
185200011122     C                   Parm                    XsocDS
185300011122   >>C                   Parm                    Kpjba
185400011122     C*
185500011122     C                   endsr
185600000000     C*---------------------------------------------------------------*
