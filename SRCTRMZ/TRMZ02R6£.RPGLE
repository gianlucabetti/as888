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
001200011121     FmzAME01L  UF A E           K DISK
001300011121     FmzAME02L  IF   E           K DISK
001400011122     F                                     RENAME(mzAME000:mzAME1)
001500011121     FmzAME03L  IF   E           K DISK
001600011122     F                                     RENAME(mzAME000:mzAME3)
001700011218     FmzAME04L  IF   E           K DISK
001800011218     F                                     RENAME(mzAME000:mzAME4)
001900031028     FmzAME05L  IF   E           K DISK
002000031028     F                                     RENAME(mzAME000:mzAME5)
002100011121     Ftrmz02D   CF   E             WORKSTN
002200011122     D  dataiso        s               d
002300011122     D  dataeur        s               d   datfmt(*eur)
002400011123     D  filll          s                   like(frnfiliale) inz
002500011122     D*STORICO
002600000000     D PARAM2          DS
002700000000     D  FLAG                   1      1
002800000000     D  NRF                    2      8  0
002900000000     D  TGA                    9     10
003000000000     D  TGN                   11     19
003100011129     D  CAU                   20     21
003200011129     D  CSP                   22     23
003300011129     D  FIL                   24     26
003400011129     D  DAT                   27     34  0
003500020607     D  SOC                   35     37
003600030115     D  cdc                   38     45
003700031028     D  tele                  46     53  0
003800910215     D                 DS
003900910215     D  VIMM                   1      4  0
004000910215     D  MES                    1      2  0
004100910215     D  ANN                    3      4  0
004200000000     D DSTAR           DS
004300000000     D  VDCTA                  1     25
004400000000     D  DESFLA                26     26
004500000000     D  DATFIS                27     28
004600011122     D* Dati di ambiente ottenuti da XSOC
004700011122     D Ana032ds      E DS                  INZ
004800011122    >D xsoc001ds     E DS
004900011123     D ana007ds      e ds                  inz
005000011126     D Ana201DS      E DS
005100011207     D dsxh          E DS
005200011220     D dsxa          E DS
005300020111     D dsxd          E DS
005400011122     D* DS Interna per dati di output di XSOC
005500011122     D XSOCDS          DS          1000
005600000000     D KPJBA         E DS
005700000000     D  LIBSYS                92    100
005800000000     D  MSGERR               453    502
005900000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
006000000000     D  TCU                  398    697
006100000000     D                                     DIM(50)                              TAB FLAG CNT
006200000000     D  KCU                  698    847P 0
006300000000     D                                     DIM(50)                              TAB CAPO CONTO
006400000000     D                                     PACKEVEN
006500000000     D  DTU                  848    895P 0
006600000000     D                                     DIM(12)                              DATE PARAM
006700000000     D                                     PACKEVEN
006800031028     d com20           s                   like(ametel) inz
006900000000      * 10 TESTN FILIALE
007000000000      * 11 TESTN CODICE TARIFFA
007100011122      * 22 generico
007200000000      * 40 DUP TASSE GOVERNATIVE
007300011122      * 42 errore cod. fornitore
007400000000      * 43 DUP INCENDIO E FURTO
007500011122      * 44 errore cod. consumi
007600000000      * 45 CODICE UTILIZZO
007700020607      * 60 codice società in dotazione
007800020607      * 61 FILIALE CENTRO DI COSTO
007900000000      * 62 PORTATA O CAVLLI FISCALI
008000000000      * 63 CODICE TARIFFA
008100000000      * 64 PORT.PIENO CARICO
008200000000      * 66 CONTROLLO GOMME
008300000000      * 67     "      "
008400000000      * 68     "      "
008500000000      * 69     "      "
008600000000      * 70 N.TELAIO
008700000000      * 71 IMMISSIONE
008800000000      * 72 TARGA
008900000000      * 73 NUMERO ASS.C/TERZI
009000000000      * 74 COMPAGNIA ASS.RC AUTO
009100000000      * 75 COMPAGNIA ASS.INCENDIO/FURTO
009200000000      * 77 DATA ULT.REVISIONE
009300000000      * 78 CODICE MANUTENZIONE
009400000000      * 79 ANNO
009500000000      * 80 TIPO
009600000000      * 81 MARCA
009700000000      * 82 TARGA
009800000000      * 83 CODICE STATO
009900000000      * 84 COSTANTE AGGIORNAMENTO
010000000000      * 85 CODICE CATEGORIA
010100000000      * 86 CODICE CARROZZERIA
010200000000      * 87 CODICE DITTA
010300000000      * 88 CODICE FILIALE
010400000000      * 89 TASSA DI CIRCOLAZIONE
010500000000      * 92 DATA ASS.RC AUTO
010600000000      * 93 DATA ASS. INCENDIO/FURTO
010700000000      * 95 DATA ASS.  C/TERZI
010800000000      * 96 SCADENZA
010900000000      * 97 TASSA PUBBLICITA'
011000000000      * 99 INDICATORE GENERICO DI ERRORE
011100000000     C*----------------------------------------------------*
011200011122     c                   if        not *inlr
011300011123     C                   EXSR      CLR01
011400011121     C                   do        *hival
011500011122     c* primo video
011600011123     c                   EXSR      CLR00
011700011123     c                   if        *in99
011800011123     c                   setoff                                       99
011900011123     c                   write     ga02d001
012000011123     c                   seton                                        99
012100011123     c                   end
012200000000     C                   EXFMT     GA02D001
012300011121     C   Kc              leave
012400011121     C* CMD06 INSERIMENTO NUOVO CODICE
012500011121     C   Kf              EXSR      NOVCOD
012600011219     c                   if        *inkd
012700011219     c                   z-add     0             naut
012800011219     C                   EXSR      ric01
012900011219     c                   end
013000011121     c* controlli
013100011121     C                   EXSR      ctr01
013200011122     C   Kc              leave
013300011122     c   99              iter
013400011122     c* Altri formati
013500011122     C                   EXSR      fmt02
013600011122     C   Kc              leave
013700011122     c   99              iter
013800011122     c                   enddo
013900000000     C                   SETON                                        LR
014000011122     c                   end
014100011122     c**********************************************************************
014200011122     c* Gestione secondo formato
014300011122     c**********************************************************************
014400011122     C     fmt02         BEGSR
014500011122     c*
014600011123     c                   z-add     0             §rig
014700011123     c                   z-add     0             §col
014800011122     C                   EXSR      LEGGI
014900011122     C  n99              do        *hival
015000011122     c* secondo video
015100011123     c                   EXSR      CLR00
015200011123     c                   if        *in99
015300011123     c                   setoff                                       99
015400011123     c                   write     ga02d002
015500011123     c                   seton                                        99
015600011123     c                   end
015700011221     c                   if        *in20
015800011221     c                   write     ga02d002
015900011221     c                   EXFMT     PROTECT1
016000011221     C                   ELSE
016100011122     C                   EXFMT     GA02D002
016200011221     C                   END
016300011122     C* FINE LAVORO
016400011122     C                   if        *inkc or *inkl
016500011126     C   kl              EXSR      CLR01
016600011122     C                   leave
016700011122     c                   end
016800011123     C* ricerca
016900011123     C                   if        *inkd
017000011123     c                   exsr      ric02
017100011123     C                   iter
017200011123     c                   end
017300011122     C* CONTROLLI 2° VIDEATA
017400011122     C                   EXSR      ctr02
017500011122     C   99              iter
017600011122     C* storico
017700011122     C                   if        *inki
017800011122     c                   exsr      srstor
017900011122     C     FLAG          IFEQ      'A'
018000011129     c                   exsr      clr01
018100011122     C                   leave
018200011122     C                   ELSE
018300011122     C                   iter
018400011122     C                   END
018500011122     C                   END
018600011122     c* TASSE E ASS.NE
018700011220     C                   if        *inkh or *in71
018800011122     c                   exsr      fmt03
018900011220     C                   IF        *INKC OR *INKL
019000011220     C   kl              EXSR      CLR01
019100011220     c                   leave
019200011122     c                   end
019300011220     c                   end
019400011122     C* AGGIORNAMENTO FILE
019500011126     c                   if        *inkF
019600011122     C                   EXSR      AGGIOR
019700011122     C                   leave
019800011122     c                   end
019900011122     c                   enddo
020000011122     c*
020100011122     C                   ENDSR
020200011122     c**********************************************************************
020300011122     C* terzo formato
020400011122     c**********************************************************************
020500011122     C     fmt03         BEGSR
020600011122     c*
020700011123     c                   z-add     0             §rig
020800011123     c                   z-add     0             §col
020900011122     c                   do        *hival
021000011122     c*terzo formato
021100011123     c                   EXSR      CLR00
021200011123     c                   if        *in99
021300011123     c                   setoff                                       99
021400011123     c                   write     ga02d003
021500011123     c                   seton                                        99
021600011123     c                   end
021700011221     c                   if        *in20
021800011221     c                   write     ga02d003
021900011221     c                   EXFMT     PROTECT2
022000011221     C                   ELSE
022100011122     C                   EXFMT     GA02D003
022200011221     C                   END
022300011122     C* FINE LAVORO
022400011126     C                   if        *inkc
022500011122     C                   leave
022600011122     c                   end
022700011123     C* ricerca
022800011123     C                   if        *inkd
022900011123     c                   exsr      ric03
023000011123     C                   iter
023100011123     c                   end
023200011220     C* VIDEO PRECEDENTE
023300011220     C                   if        *inkl
023400011220     C                   EXSR      CLR03
023500011220     C                   leave
023600011220     c                   end
023700011122     C* CONTROLLI 3 VIDEATA
023800011122     C                   EXSR      ctr03
023900011122     c   99              iter
024000011122     C* AGGIORNAMENTO FILE
024100011126     C   kF              leave
024200011122     c                   enddo
024300011122     c*
024400011122     C                   ENDSR
024500011123     C**********************************************************************
024600011123     C** LETTURA X CHIAVE N.RIFERIMENTO
024700011123     C**********************************************************************
024800011123     C     LEGGI         BEGSR
024900011123     C                   SETOFF                                       842171
025000011123     c                   do
025100011218     C* LETTURA X AUTISTA
025200011218     c                   z-add     0             naut
025300011218     C     VCAU          IFNE      *BLANKS
025400011218     C     VNRF          andeq     0
025500011218     C     vcau          setll     mzAME4
025600011218     c                   do        *hival
025700011218     c     vcau          reade     mzame04l                               21
025800011218     c   21              leave
025900011218     c                   add       1             naut              2 0
026000011218     c                   if        naut > 1
026100011218     c                   eval      §fld = 'VCAU'
026200011218     c                   exsr      ric01
026300011219     c* non è stata fatta alcuna scelta
026400011219     c                   if        cmd = 'L'
026500011219     c                   seton                                        99
026600011219     c                   end
026700011218     c                   leave
026800011218     c                   end
026900011218     c                   enddo
027000011219     c* non trovato automezzo con l'autista scelto
027100011218     c                   if        *in21 and naut = 0
027200011218     C                   SETON                                        5999
027300011218     C                   leave
027400011218     C                   END
027500011219     c                   if        naut <> 0
027600011219     c                   if        not *in99
027700011219     c                   setoff                                       21
027800011219     c                   else
027900011219     c                   leave
028000011219     c                   end
028100011219     c                   end
028200011219     C                   END
028300011123     C* LETTURA X N.RIFERIMENTO
028400011123     C     VNRF          IFNE      0
028500011123     C     KGAAM         CHAIN     mzAME000                           21
028600011123     C   21              SETON                                        71
028700011123     C                   END
028800011123     C* LETTURA X N. DI TARGA
028900011123     C     VTGA          IFNE      *BLANKS
029000011123     C     VTGN          ANDNE     *BLANKS
029100011123     C     KTAR          CHAIN     mzAME1                             21
029200011123     C   21              SETON                                        7299
029300011123     C   21              leave
029400011123     C                   END
029500011123     C* LETTURA X N.TELAIO
029600011123     C     VTLA          IFNE      *BLANKS
029700011123     C     VTLA          CHAIN     mzAME3                             21
029800011123     C   21              SETON                                        7099
029900011123     C   21              leave
030000011123     C                   END
030100011221     C* SE POSSO SOLO VISUALIZZARE NON EMMETTO LA VIDEATA DI IMMISSIONE
030200011221     C                   IF        *IN20 AND *IN71
030300011221     C                   SETON                                        99
030400011221     C                   LEAVE
030500011221     C                   END
030600011221     C*
030700011123     c                   exsr      clr02
030800011123     C                   if        not *in21
030900011123     C                   SETON                                        84
031000011123     C                   Z-ADD     AMENRF        VNRF
031100011218     C                   MOVEL     AMETLA        VTLA
031200011123     C                   MOVEL     AMESTA        VSTA
031300011123     C*RICERCA SULLA TABELLA 'XF' LA DESCRIZIONE
031400011123     C*DELLO STATO AUTOMEZZO
031500011123     c                   if        vsta <> *blanks
031600011126     c                   exsr      srvSTA
031700011123     c                   else
031800011123     C                   MOVEL     *blanks       VDsta
031900011123     C                   END
032000011123     C                   MOVEL     AMETGA        VTGA
032100011123     C                   MOVEL     AMETGN        VTGN
032200011123     C                   MOVEL     AMECAT        VCAT
032300011123     C*RICERCA SULLA TABELLA 'XA' LA DESCRIZIONE
032400011123     C*DELLA CATEGORIA AUTOMEZZO
032500011126     c                   exsr      srvCAT
032600011123     C                   MOVEL     AMEMAR        VMAR
032700011123     C                   MOVEL     AMETIP        VTIP
032800011123RE   C                   MOVEL     AMECIL        VCIL
032900011123     C                   MOVEL     AMECAR        VCAR
033000011123     C*RICERCA SULLA TABELLA 'XB' LA DESCRIZIONE
033100011123     C*DEL TIPO CAROZZERIA
033200011123     c                   if        vcar <> *blanks
033300011126     c                   exsr      srvCAR
033400011123     C                   else
033500011123     C                   MOVEL     *blanks       VDcar
033600011123     C                   END
033700011123     C                   Z-ADD     AMEIMM        VIMM
033800011123     C                   Z-ADD     MES           ANTO              2 0
033900011123     C                   Z-ADD     ANN           MES
034000011123     C                   Z-ADD     ANTO          ANN
034100011123     C                   MOVEL     AMEMAN        VMAN
034200011123     C*RICERCA SULLA TABELLA 'XC' LA DESCRIZIONE
034300011123     C*DEL TIPO DI MANUTENZIONE
034400011126     c                   exsr      srvMAN
034500011123     C                   MOVEL     AMEUTI        VUTI
034600011123     C*RICERCA SULLA TABELLA 'XL' LA DESCRIZIONE
034700011123     C*DEL TIPO DI UTILIZZO
034800011123     c                   if        vuti <> *blanks
034900011126     c                   exsr      srvuti
035000011123     C                   else
035100011123     C                   MOVEL     *blanks       VDes
035200011123     C                   END
035300011123     C                   Z-ADD     AMEASS        VASS
035400011123     C                   movel     AMEusi        Vcon
035500011123     C*RICERCA SULLA TABELLA 'XH' LA DESCRIZIONE
035600011123     C*DEL COD. CONSUMI
035700011126     c                   exsr      srvCON
035800011123     C                   MOVEL     AMEALI        VALI
035900011123     C                   Z-ADD     AMETAR        VTAR
036000011123     C                   Z-ADD     AMEPPC        VPPC
036100011123     C                   Z-ADD     AMEPTR        VPTR
036200011123     C                   Z-ADD     AMEPQL        VPQL
036300011123     C                   Z-ADD     AMENGM        VNGM
036400011123     C                   MOVEL     AMETG1        VTG1
036500011123     C                   MOVEL     AMETG2        VTG2
036600011123     C                   MOVEL     AMETG3        VTG3
036700011123     C                   movel     AMEDIT        VDIT
036800011219     C*RICERCA LA DESCRIZIONE
036900011219     C*DELLA DITTA
037000020607     c                   exsr      srvdit
037100020607     C                   movel     AMEsoc        Vsoc
037200020607     C*RICERCA LA DESCRIZIONE
037300020607     C*DELLA DITTA
037400020607     c                   exsr      srvsoc
037500011123     C                   MOVEL     AMEFIL        VFIL
037600011123     C                   MOVEL     AMECDC        VCDC
037700011123     C                   MOVEL     AMEfor        Vfor
037800031029     c                   clear                   telev
037900031029     c                   if        ametel <> *blanks
038000031029     C                   MOVEl     AMEtel        telev
038100031029     c                   end
038200011126     C*RICERCA LA DESCRIZIONE
038300011123     C*DEL fornitore
038400011126     c                   if        vfor <> *blanks
038500011126     C                   EXSR      SRVFOR
038600011126     c                   end
038700011123     C*RICERCA SU AZORG LA DESCRIZIONE
038800011123     C*DELLA FILIALE IN DOTAZIONE
038900011126     C                   EXSR      SRVFIL
039000011123     C*RICERCA SU ANENT LA DESCRIZIONE
039100011123     C*DELLA FILIALE CDC
039200011126     C                   EXSR      SRVCDC
039300011220     c                   if        amedur <> 0
039400011123     C                   MOVE      AMEDUR        dataiso
039500011123     C                   move      dataiso       DATAeur
039600011123     C                   move      DATAeur       VDUR
039700011220     c                   else
039800011220     c                   z-add     0             vdur
039900011220     c                   end
040000011220     c                   if        amedsp <> 0
040100011123     C                   move      AMEDSP        dataiso
040200011123     C                   move      dataiso       dataeur
040300011123     C                   move      DATAeur       VSCAD
040400011220     c                   else
040500011220     c                   z-add     0             vscad
040600011220     c                   end
040700011123     C*RICERCA SULLA TABELLA 'XN' LA DESCRIZIONE
040800011123     C*DELL' AUTISTA AUTOMEZZO
040900011123     C                   MOVEL     AMECAU        VCAU
041000011126     C                   EXSR      SRVCAU
041100011220     c                   if        amedal <> 0
041200011123     C                   move      AMEDAL        DATAIso
041300011123     C                   move      dataiso       dataeur
041400011123     C                   move      DATAeur       dataut
041500011220     c                   else
041600011220     c                   z-add     0             dataut
041700011220     C                   END
041800011123     C*RICERCA SULLA TABELLA 'XR' LA DESCRIZIONE
041900011123     C*DEL TIPO PROPRIETA'
042000011123     C                   MOVEL     AMECSP        VCSP
042100011126     C                   EXSR      SRVCSP
042200011123     C* 3 VIDEATA
042300011123     C*TASSA DI CIRCOLAZIONE
042400011123     c                   if        amedtc <> 0
042500011123     C                   move      AMEDtc        DATAIso
042600011123     C                   move      dataiso       dataeur
042700011123     C                   move      DATAeur       vdtc
042800011123     c                   else
042900011123     c                   z-add     0             vdtc
043000011123     c                   end
043100011123     C                   Z-ADD     AMEIMT        VIMT
043200011123     C*TASSA PUBBLICITA'
043300011123     c                   if        amedtp <> 0
043400011123     C                   move      AMEDtp        DATAIso
043500011123     C                   move      dataiso       dataeur
043600011123     C                   move      DATAeur       vdtp
043700011123     c                   else
043800011123     c                   z-add     0             vdtp
043900011123     c                   end
044000011123     C                   Z-ADD     AMEIMP        VIMP
044100011123     C*SCADENZA ASS.RCA
044200011123     c                   if        amedrc <> 0
044300011123     C                   move      AMEDrc        DATAIso
044400011123     C                   move      dataiso       dataeur
044500011123     C                   move      DATAeur       vdrc
044600011123     c                   else
044700011123     c                   z-add     0             vdrc
044800011123     c                   end
044900011123     C                   MOVEL     AMECRC        VCRC
045000011123     C*RICERCA SULLA TABELLA 'XD' LA DESCRIZIONE
045100011123     C*DELLA COMP.ASS.RCA
045200011123     c                   if        vcrc <> *blanks
045300011126     C                   EXSR      SRVCRC
045400011123     C                   else
045500011123     C                   MOVEL     *blanks       VDcrc
045600011123     C                   END
045601031127     C*descrizione equipaggiamento
045602031127     C                   MOVEL     AMEequ        Vequi
045603031127     c                   if        vequi<> *blanks
045604031127     C                   EXSR      SRVequi
045605031127     C                   else
045606031128     C                   MOVEL     *blanks       VDequ1
045607031128     C                   MOVEL     *blanks       VDequ2
045608031127     C                   END
045700011123     c                   if        amedfi <> 0
045800011123     C                   move      AMEDfi        DATAIso
045900011123     C                   move      dataiso       dataeur
046000011123     C                   move      DATAeur       vdfi
046100011123     c                   else
046200011123     c                   z-add     0             vdfi
046300011123     c                   end
046400011123     C                   MOVEL     AMECFI        VCFI
046500011123     C                   Z-ADD     AMEIMA        VIMA
046600011123     C*RICERCA SULLA TABELLA 'XD' LA DESCRIZIONE
046700011123     C*DELLA COMP.ASS.INCENDIO/FURTO
046800011123     c                   if        vcfi <> *blanks
046900011126     C                   EXSR      SRVCFI
047000011123     C                   else
047100011123     C                   MOVEL     *blanks       VDcfi
047200011123     C                   END
047300011123     C                   Z-ADD     AMEIMF        VIMF
047400011123     C                   MOVEL     AMENAC        VNAC
047500011123     c                   if        amedac <> 0
047600011123     C                   move      AMEDac        DATAIso
047700011123     C                   move      dataiso       dataeur
047800011123     C                   move      DATAeur       vdac
047900011123     c                   else
048000011123     c                   z-add     0             vdac
048100011123     c                   end
048200011123     c                   if        amedac <> 0
048300011123     C                   move      AMEsac        DATAIso
048400011123     C                   move      dataiso       dataeur
048500011123     C                   move      DATAeur       vsac
048600011123     c                   else
048700011123     c                   z-add     0             vsac
048800011123     c                   end
048900011123     C                   Z-ADD     AMEIMK        VIMK
049000011123     C                   MOVEL     AMETTA        VTTA
049100011123     C                   MOVE      AMECTA        VCTA
049200011123     C*RICERCA SULLA TABELLA 'XI' LA DESCRIZIONE
049300011123     C*DELLA CODICE TARIFFA
049400011123     c                   if        vcta <> 0
049500011126     C                   EXSR      SRVCTA
049600011123     C                   else
049700011126     C                   MOVEL     *blanks       dstar
049800011123     C                   END
049900011123     C                   Z-ADD     AMECVF        VCVF
050000011123     C                   MOVEL     AMEPBL        VPBL
050100011123     C                   ENDif
050200011123     C                   enddo
050300011123     c                   if        *in99
050400011123     c                   exsr      clr01
050500011123     C                   END
050600011123     C                   ENDSR
050700011126     c**********************************************************************
050800011126     C* STORICO
050900011126     c**********************************************************************
051000011126     C     srstor        BEGSR
051100011126     c*
051200011122     C     *HIVAL        SETLL     mzAME01L
051300011122     C                   MOVEL     *BLANKS       FLAG
051400011122     C                   Z-ADD     VNRF          NRF
051500011122     C                   MOVEL     VTGA          TGA
051600011122     C                   MOVEL     VTGN          TGN
051700011122     C                   MOVEL     VCAU          CAU
051800011122     C                   MOVEL     VCSP          CSP
051900011122     C                   MOVEL     VFIL          FIL
052000031028     C                   MOVE      telev         tele
052100031028     C                   MOVE      DATAUT        DAT
052200020607     C                   MOVEL     VSOC          SOC
052300030115     C                   MOVEL     Vcdc          cdc
052400030115     C                   MOVEL(p)  PARAM2        KPJBU
052500011129     C                   CALL      'TRMZ02R5'
052600011122     C                   PARM                    KPJBA
052700011122     C                   MOVEL     KPJBU         PARAM2
052800011122     C                   MOVEL     TGA           VTGA
052900011122     C                   MOVEL     TGN           VTGN
053000011122     C                   MOVEL     CAU           VCAU
053100011122     C                   MOVEL     CSP           VCSP
053200011129     C                   exsr      srvcsp
053300011122     C                   MOVEL     FIL           VFIL
053400011129     C                   exsr      srvfil
053500020607     C                   MOVEL     SOC           VSOC
053600020607     C                   exsr      srvSOC
053700030115     C                   MOVEL     cdc           Vcdc
053800030115     C                   exsr      srvcdc
053900011122     C     FLAG          IFEQ      'T'
054000011122     C                   MOVE      DAT           DATAUT
054100011129     C                   exsr      srvcau
054200011122     C                   END
054300031028     C                   MOVE      tele          telev
054400011122     c*
054500011122     C                   ENDSR
054600011121     c**********************************************************************
054700011121     C* IMMISSIONE NUOVO CODICE
054800011121     c**********************************************************************
054900000000     C     NOVCOD        BEGSR
055000011122     C     *HIVAL        SETLL     mzAME000
055100011122     C                   READP     mzAME000                               22
055200011122     c                   if        not *in22
055300000000     C                   Z-ADD     AMENRF        VNRF
055400000000     C                   ADD       1             VNRF
055500011122     c                   end
055600000000     C                   ENDSR
055700011123     c**********************************************************************
055800011123     c* controlli 1° formato
055900011123     c**********************************************************************
056000011123     C     ctr01         BEGSR
056100011123     C                   SETOFF                                       99
056200011123     c                   do
056300011123     C     VNRF          IFNE      0
056400011123     C     VTGA          ANDNE     *BLANKS
056500011123     C                   SETON                                        99
056600011123     C                   LEAVE
056700011123     C                   END
056800011123     C     VTGA          IFNE      *BLANKS
056900011123     C     VTLA          ANDNE     *BLANKS
057000011123     C                   SETON                                        99
057100011123     C                   LEAVE
057200011123     C                   END
057300011123     C     VNRF          IFNE      0
057400011123     C     VTLA          ANDNE     *BLANKS
057500011123     C                   SETON                                        99
057600011123     C                   LEAVE
057700011123     C                   END
057800011123     C     VNRF          IFEQ      0
057900011123     C     VTGA          andeq     *BLANKS
058000011123     C     VTLA          andeq     *BLANKS
058100011218     C     Vcau          andeq     *BLANKS
058200011123     C                   SETON                                        99
058300011123     C                   LEAVE
058400011123     C                   END
058500011123     C                   ENDdo
058600011123     C                   ENDSR
058700011123     c**********************************************************************
058800011123     c* ricerca 1° videata
058900011123     c**********************************************************************
059000011123     C     ric01         BEGSR
059100011123     C*
059200011123     C                   SELECT
059300011221     C                   WHEN      §FLD = 'VTGN' OR §FLD = 'VTGA'
059400011221     C                   CALL      'TRMZ02R4'    RICTAR
059500011219     C                   WHEN      §FLD = 'VTLA'
059600011219     C                   CALL      'TRMZ42R'
059700011219     c                   parm                    vtla
059800011219     c                   parm                    cmd
059900011218     C                   WHEN      §FLD = 'VCAU'
060000011218     c                   if        naut > 1
060100011218     c                   move      ' '           ordina
060200011218     c                   else
060300011218     c                   move      '1'           ordina
060400011218     c                   movel     *blanks       vcau
060500011218     c                   end
060600011218     c                   z-add     0             vnrf
060700011218     C                   CALL      'TRMZ41R'
060800011218     c                   parm                    ordina            1
060900011218     c                   parm                    vcau
061000011218     c                   parm                    vnrf
061100011218     c                   parm                    cmd               1
061200011219     c                   if        cmd = 'L'
061300011219     c                   clear                   vnrf
061400011123     C                   END
061500011219     C                   END
061600011123     C* POSIZIONAMENTO CURSORE
061700011123     C                   Z-ADD     §RIGA         §RIG
061800011123     C                   Z-ADD     §COLON        §COL
061900011123     C                   ENDSR
062000011122     c**********************************************************************
062100000000     C** CONTROLLI 2 VIDEATA
062200011122     c**********************************************************************
062300011122     C     ctr02         BEGSR
062400011122     C                   SETOFF                                       99
062500011123     c                   movel     ' '           fld               1
062600011122     c                   do
062700020607     C*CODICE società proprietaria
062800020607     C     VDIT          IFEQ      *blanks
062900020607     C                   SETON                                        8799
063000020607     c                   movel     *blanks       vddit
063100020607     C                   leave
063200020607     C                   ELSE
063300020607     c                   exsr      srvdit
063400020607     C                   END
063500000000     C*CODICE FILIALE
063600011122     C                   MOVEL     *blanks       ORGDES
063700011122     C     VFIL          IFEQ      *BLANKS
063800011122     C                   SETON                                        8899
063900011122     C                   leave
064000011122     C                   else
064100000000     C                   TESTN                   VFIL                 10
064200011122     C                   if        not *in10
064300000000     C                   SETON                                        8899
064400011122     C                   leave
064500011122     C                   else
064600011126     c                   exsr      srvfil
064700011126     C   99              leave
064800000000     C                   END
064900011122     C                   END
065000020607     C*CODICE società in dotazione
065100020607     C     Vsoc          IFEQ      *blanks
065200020607     C                   SETON                                        6099
065300020607     c                   movel     *blanks       vdsoc
065400020607     C                   leave
065500020607     C                   ELSE
065600020607     c                   exsr      srvsoc
065700020607     C                   END
065800000000     C*CODICE CDC
065900011126     c                   exsr      srvcdc
066000011126     C   99              leave
066100000000     C*TARGA
066200000000     C     VTGA          IFEQ      *BLANKS
066300000000     C                   SETON                                        8299
066400011122     C                   leave
066500000000     C                   END
066600000000     C     VTGN          IFEQ      *BLANKS
066700000000     C                   SETON                                        8299
066800011122     C                   leave
066900000000     C                   END
067000011122     C*TELAIO
067100011122     C     VTLA          IFEQ      *BLANKS
067200011122     C                   SETON                                        7099
067300011122     C                   leave
067400011122     C                   END
067500000000     C*MARCA
067600000000     C     VMAR          IFEQ      *BLANKS
067700000000     C                   SETON                                        8199
067800011122     C                   leave
067900000000     C                   END
068000000000     C*TIPO
068100000000     C     VTIP          IFEQ      *BLANKS
068200000000     C                   SETON                                        8099
068300011122     C                   leave
068400000000     C                   END
068500000000     C*CATEGORIA
068600011126     c                   exsr      srvcat
068700011126     C   99              leave
068800031106     C*Telepass
068900031106     c                   if        not *in84
069000031106     C                   IF        telev = 0 and §xatel = 'S'
069100031106     C                   SETON                                        1499
069200031106     C                   leave
069300031106     c                   end
069400031106     c                   if        telev <> 99999999
069500031106     c                   movel     telev         com20
069600031106     c     com20         setll     mzame05l                               14
069700031106     c                   if        *in14
069800031106     C                   SETON                                        99
069900031106     C                   leave
070000031106     C                   END
070100031106     C                   END
070200031106     C                   END
070300910215     C*MESE/ANNO IMMATRICOLAZIONE
070400910215     C     MES           IFEQ      0
070500910215     C     MES           ORGT      12
070600000000     C                   SETON                                        7999
070700011122     C                   leave
070800000000     C                   END
070900910204     C*CODICE AUTISTA
071000011126     C                   EXSR      SRVCAU
071100011126     C   99              leave
071200910215     C* DATA ASSEGNAZIONE AUTISTA
071300011220     C                   IF        dataut = 0 and §xadau = 'S'
071400910215     C                   SETON                                        5899
071500011122     C                   leave
071600011220     C                   End
071700011220     C                   IF        dataut <> 0
071800011123     C     *EUR          test(d)                 dataut                 58
071900011122     c                   if        *in58
072000011122     C                   SETON                                        99
072100011122     C                   leave
072200910215     C                   END
072300910215     C                   END
072400011122     C*FORNITORE
072500011123     c                   if        vfor <> *blanks
072600011126     c                   exsr      srvfor
072700011123     c                   end
072800000000     C*CARROZZERIA
072900011123     c                   if        vcar <> *blanks
073000011126     C                   EXSR      srvcar
073100011126     C   99              leave
073200011122     C                   END
073300011123     C*alimentazione
073400011123     c                   if        vali = *blanks
073500011123     c                   seton                                        9099
073600011123     c                   leave
073700011123     c                   end
073800011123     C*COD.CONSUMI
073900011126     c                   exsr      srvcon
074000011126     C   99              leave
074100910204     C*CODICE SIT. PROPRIETA'
074200011122     C                   TESTN                   VCsp                 10
074300011122     C                   if        not *in10
074400011122     C                   SETON                                        5699
074500011122     C                   leave
074600011122     C                   END
074700011126     c                   exsr      srvcsp
074800011126     C   99              leave
074900910214     C* DATA SCAD. PROPRIETA'
075000011220     C                   IF        vscad = 0 and §xadsc = 'S'
075100011122     C                   SETON                                        5799
075200011122     C                   leave
075300011220     C                   End
075400011220     C     vscad         IFne      0
075500011123     C     *EUR          test(d)                 vscad                  57
075600011122     c                   if        *in57
075700011122     C                   SETON                                        99
075800011122     C                   leave
075900011122     C                   END
076000011122     C                   END
076100000000     C*TARA + PORT.UTILE = PORTATA PIENO CARICO
076200000000     C     VTAR          ADD       VPQL          COMPPC            3 0
076300000000     C     COMPPC        IFNE      VPPC
076400000000     C                   SETON                                        6499
076500011122     C                   leave
076600000000     C                   END
076700000000     C*TIPO GOMME
076800011123     c                   movel     'XG'          parcod
076900011123     c                   if        vtg1 <> *blanks
077000011122     c                   movel(P)  VTG1          parkey
077100011122     c                   exsr      srtab
077200011122     c                   movel     parkey        vtg1
077300011123     c                   if        *in99
077400011122     C                   SETON                                        66
077500011122     C                   leave
077600011122     C                   END
077700011122     C                   END
077800011123     c                   if        vtg2 <> *blanks
077900011122     c                   movel(P)  VTG2          parkey
078000011122     c                   exsr      srtab
078100011122     c                   movel     parkey        vtg2
078200011123     c                   if        *in99
078300011122     C                   SETON                                        67
078400011122     C                   leave
078500011122     C                   END
078600011122     C                   END
078700011123     c                   if        vtg3 <> *blanks
078800011122     c                   movel(P)  VTG3          parkey
078900011122     c                   exsr      srtab
079000011122     c                   movel     parkey        vtg3
079100011123     c                   if        *in99
079200011122     C                   SETON                                        69
079300011122     C                   leave
079400011122     C                   END
079500011122     C                   END
079600011220     C* DATA ultima REVISIONE
079700011220     C                   IF        vdur = 0 and §xadur = 'S'
079800011122     C                   SETON                                        7799
079900011122     C                   leave
080000011220     C                   End
080100011220     c                   if        vdur <> 0
080200011123     C     *EUR          test(d)                 vdur                   77
080300011122     c                   if        *in77
080400011122     C                   SETON                                        99
080500011122     C                   leave
080600011122     C                   END
080700011122     C                   END
080800000000     C*TIPO MANUTENZIONE
080900011126     c                   exsr      srvman
081000011126     C   99              leave
081100000000     C*COD.UTILIZZO
081200011123     c                   if        vuti <> *blanks
081300011126     c                   exsr      srvUTI
081400011126     C   99              leave
081500011122     C                   END
081600000000     C*COD.STATO
081700011123     c                   if        vsta <> *blanks
081800011126     c                   exsr      srvSTA
081900011126     C   99              leave
082000011122     C                   END
082100011122     C                   enddo
082200011122     c*
082300000000     C                   ENDSR
082400011126     c**********************************************************************
082500011126     C** ricerca  2 VIDEATA
082600011126     c**********************************************************************
082700011126     C     ric02         BEGSR
082800011123     c                   movel     '1'           fld
082900011123     c                   select
083000020607     C*CODICE società proprietaria
083100011123     C                   when      §fld = 'VDIT'
083200011123     C                   reset                   ana007ds
083300011123     C                   eval      opz007 = '01'
083400011123     C                   eval      kpjbu = ana007ds
083500011123     C                   call      'ANA007R'
083600011123     C                   parm                    kpjba
083700011123     C                   eval      ana007ds = kpjbu
083800011123     C                   if        err007 <> '1' and soc007 <> *blanks
083900011123     C                   eval      vdit = soc007
084000011219     c                   exsr      srvdit
084100011123     C                   endif
084200020607     C*CODICE FILIALE
084300020607     C                   when      §fld = 'VFIL'
084400020607     C                   MOVEL     *BLANKS       §COD1
084500020607     C                   MOVE      *BLANKS       §DES1
084600020607     C                   CALL      'TNSD19R'
084700020607     C                   PARM                    §COD1             3
084800020607     C                   PARM                    §TIP              1
084900020607     C                   PARM                    §DES1            25
085000020607     c                   if        §cod1 <> *blanks
085100020607     C                   MOVEL     §COD1         VFIL                           *CODICE
085200020607     C                   MOVEL     §DES1         VDFIL                          *DESCRIZIONE
085300020607     c                   end
085400020607     C*CODICE società in dotazione
085500020607     C                   when      §fld = 'VSOC'
085600020607     C                   reset                   ana007ds
085700020607     C                   eval      opz007 = '01'
085800020607     C                   eval      kpjbu = ana007ds
085900020607     C                   call      'ANA007R'
086000020607     C                   parm                    kpjba
086100020607     C                   eval      ana007ds = kpjbu
086200020607     C                   if        err007 <> '1' and soc007 <> *blanks
086300020607     C                   eval      vsoc = soc007
086400020607     c                   exsr      srvsoc
086500020607     C                   endif
086600011123     C*CODICE CDC
086700011123     C                   when      §fld = 'VCDC'
086800011123    >C                   RESET                   ANA032DS
086900011123    >C                   MOVEL     '01'          OPZ032
087000011123     C                   MOVE      'S'           FSC032
087100011123     C                   MOVE      'CDC'         TPE032
087200011123    >C                   MOVEL     ANA032Ds      KPJBU
087300011123    >C                   CALL      'ANA032R'
087400011123     C                   PARM                    KPJBA
087500011123    >C                   MOVEL     KPJBU         ANA032DS
087600011212     c                   if        ent032 <> *blanks
087700011123     C                   MOVEL     ENT032        VCDC
087800011212     c                   end
087900011123     C*CATEGORIA
088000011123     C                   when      §fld = 'VCAT'
088100011126     c                   exsr      srvcat
088200011123     C*CODICE AUTISTA
088300011123     C                   when      §fld = 'VCAU'
088400011126     c                   exsr      srvcau
088500011123     C*FORNITORE
088600011123     C                   when      §fld = 'VFOR'
088700011126     c                   movel     vfor          vfors             8
088800011126     C                   RESET                   Ana201DS
088900011126     c                   movel     socxsc        soc201
089000011126     c                   movel(p)  'F'           kcc201
089100011126     C                   EVAL      Kpjbu = Ana201DS
089200011126     C                   CALL      'ANA201R'
089300011126     C                   PARM                    Kpjba
089400011126     C                   EVAL      Ana201DS = Kpjbu
089500011126     C                   IF        Err201 = '0' and ksc201 <> *blanks
089600011126     c                   movel     ksc201        vfor
089700011126     c                   EXSR      SRVFOR
089800011126     c                   else
089900011126     c                   movel     vfors         vfor
090000011126     c                   end
090100011123     C*CARROZZERIA
090200011123     C                   when      §fld = 'VCAR'
090300011126     c                   exsr      srvcar
090400011123     C*COD.CONSUMI
090500011123     C                   when      §fld = 'VCON'
090600011126     c                   exsr      srvcon
090700011123     C*CODICE SIT. PROPRIETA'
090800011123     C                   when      §fld = 'VCSP'
090900011126     c                   exsr      srvcsp
091000011123     C*TIPO GOMME
091100011123     C                   when      §fld = 'VTG1'
091200011123     c                   movel     'XG'          parcod
091300011126     c                   movel     vtg1          parkey
091400011123     c                   exsr      srtab
091500011123     c                   movel     parkey        vtg1
091600011123     C                   when      §fld = 'VTG2'
091700011123     c                   movel     'XG'          parcod
091800011126     c                   movel     vtg2          parkey
091900011123     c                   exsr      srtab
092000011123     c                   movel     parkey        vtg2
092100011123     C                   when      §fld = 'VTG3'
092200011123     c                   movel     'XG'          parcod
092300011126     c                   movel     vtg3          parkey
092400011123     c                   exsr      srtab
092500011123     c                   movel     parkey        vtg3
092600011123     C*TIPO MANUTENZIONE
092700011123     C                   when      §fld = 'VMAN'
092800011126     c                   exsr      srvman
092900011123     C*COD.UTILIZZO
093000011123     C                   when      §fld = 'VUTI'
093100011126     c                   exsr      srvuti
093200011123     C*COD.STATO
093300011123     C                   when      §fld = 'VSTA'
093400011126     c                   exsr      srvsta
093500011123     C                   endsl
093600011123     C* POSIZIONAMENTO CURSORE
093700011123     C                   Z-ADD     §RIGA         §RIG
093800011123     C                   Z-ADD     §COLON        §COL
093900011123     c*
094000011123     C                   ENDSR
094100011122     C**********************************************************************
094200000000     C** CONTROLLI 3 VIDEATA
094300011122     C**********************************************************************
094400011122     C     ctr03         BEGSR
094500011123     c                   movel     ' '           fld
094600011122     c                   setoff                                       99
094700011122     c                   do
094800000000     C* TASSA CIRCOLAZIONE
094900011220     C                   IF        vdtc = 0 and §xadtc = 'S'
095000011122     C                   SETON                                        8999
095100011122     C                   leave
095200011220     C                   End
095300011220     C                   IF        vdtc <> 0
095400011126     C     *EUR          test(d)                 vdtc                   89
095500011122     c                   if        *in89
095600011122     C                   SETON                                        99
095700011122     C                   leave
095800011122     C                   END
095900011122     C                   END
096000011123     c* periodicità tassa circolazione
096100011123     c                   if        vpbl = *blanks
096200011123     c                   seton                                        9199
096300011123     c                   leave
096400011123     c                   end
096500000000     C* TASSA PUBBLICITA'
096600011126     C     vdtP          IFne      0
096700011126     C     *EUR          test(d)                 vdtp                   97
096800011122     c                   if        *in97
096900011122     C                   SETON                                        99
097000011122     C                   leave
097100011122     C                   END
097200011122     C                   END
097201031127     c* equipaggiamento
097202031127     c                   if        vequi <> ' '
097203031127     c                   exsr      srvequi
097204031127     c                   end
097300000000     C* DATA E COMPAGNIA ASS. RC AUTO
097400011126     C                   EXSR      SRVCRC
097500011126     C   99              leave
097600011122     C     vdrc          IFne      0
097700011126     C     *EUR          test(d)                 vdrc                   92
097800011122     c                   if        *in92
097900011122     C                   SETON                                        99
098000011122     C                   leave
098100011122     C                   END
098200011122     C                   END
098300000000     C* DATA E COMPAGNIA ASS. INCENDIO/FURTO
098400011123     c                   if        vcfi <> *blanks
098500011126     C                   EXSR      SRVCFI
098600011126     C   99              leave
098700020128     C                   else
098800020128     C                   MOVEL     *blanks       VDcfi
098900020128     C                   END
099000000000     C   43              Z-ADD     VDRC          VDFI
099100020128     C                   IF        vdfi = 0 and vcfi <> *blanks and
099200020128     c                             §xdass <> 'N'
099300011122     C                   SETON                                        9399
099400011122     C                   leave
099500020128     C                   end
099600020128     C                   IF        vdfi <> 0
099700020128     c                   if        vcfi = *blanks
099800020128     C                   SETON                                        7599
099900020128     C                   leave
100000020128     C                   end
100100011123     C     *EUR          test(d)                 vdfi                   93
100200011122     c                   if        *in93
100300011122     C                   SETON                                        99
100400011122     C                   leave
100500011122     C                   END
100600000000     C                   END
100700000000     C* ASS.C/TERZI
100800011122     C     VNAC          IFne      *BLANKS
100900011122     C     VDAC          orne      0
101000011122     C     VSAC          orne      0
101100000000      *
101200000000     C     VNAC          IFEQ      *BLANKS
101300000000     C                   SETON                                        7399
101400011122     C                   leave
101500000000     C                   END
101600000000     C     VDAC          IFEQ      0
101700000000     C                   SETON                                        9599
101800011122     C                   leave
101900000000     C                   ELSE
102000011123     C     *EUR          test(d)                 vdac                   95
102100011122     c                   if        *in95
102200011122     C                   SETON                                        99
102300011122     C                   leave
102400011122     C                   END
102500000000     C                   END
102600000000     C     VSAC          IFEQ      0
102700000000     C                   SETON                                        9699
102800011122     C                   leave
102900000000     C                   ELSE
103000011123     C     *EUR          test(d)                 vsac                   96
103100011122     c                   if        *in96
103200011122     C                   SETON                                        99
103300011122     C                   leave
103400011122     C                   END
103500000000     C                   END
103600011122     C                   end
103700011123     c* tipo tariffa
103800011123     c                   if        vtta = *blanks
103900011123     c                   seton                                        9899
104000011123     c                   leave
104100011123     c                   end
104200000000     C*CODICE TARIFFA
104300011126     C                   EXSR      SRVCTA
104400011126     C   99              leave
104500000000     C     DATFIS        IFEQ      'QL'
104600000000     C     VPQL          ANDEQ     0
104700000000     C                   SETON                                        6299
104800011122     C                   leave
104900000000     C                   END
105000000000     C     DATFIS        IFEQ      'CV'
105100000000     C     VCVF          ANDEQ     0
105200000000     C                   SETON                                        6299
105300011122     C                   leave
105400000000     C                   END
105500011122     C                   enddo
105600011122     c*
105700011122     c                   endsr
105800011123     C**********************************************************************
105900011123     C** RICERCA 3 VIDEATA
106000011123     C**********************************************************************
106100011123     C     RIC03         BEGSR
106200011123     c                   movel     '1'           fld
106300011123     c                   SELECT
106400011123     C* COMPAGNIA ASS. RC AUTO
106500011123     C                   WHEN      §fld = 'VCRC'
106600011126     c                   exsr      srvcrc
106700011123     C* COMPAGNIA ASS. INCENDIO/FURTO
106800011123     C                   WHEN      §fld = 'VCFI'
106801031127     C* equipaggiamento
106802031127     C                   WHEN      §fld = 'VEQUI'
106900031127     c                   exsr      srvequi
107000011123     C*CODICE TARIFFA
107100011123     C                   WHEN      §fld = 'VCTA'
107200011126     c                   exsr      srvcta
107300011123     C                   endSL
107400011123     C* POSIZIONAMENTO CURSORE
107500011123     C                   Z-ADD     §RIGA         §RIG
107600011123     C                   Z-ADD     §COLON        §COL
107700011123     c*
107800011123     c                   endsr
107900011126     c**********************************************************************
108000011126     C** fornitore
108100011126     c**********************************************************************
108200011126     C     srvfor        BEGSR
108300011126     c*
108400011126     c     KFOR          CHAIN     ANFRN01L                           42
108500011126     C                   IF        NOT *IN42
108600011126     c     FRNSOGG       CHAIN     ANSOG01L                           42
108700011126     C                   END
108800011126     C                   IF        NOT *IN42
108900011126     C                   MOVEL(P)  SOGDES        VDFOR
109000011126     C                   ELSE
109100011126     C                   CLEAR                   VDFOR
109200011126     c                   end
109300011126     c*
109400011126     C                   ENDSR
109500020607     c**********************************************************************
109600020607     C** società proprietaria
109700020607     c**********************************************************************
109800020607     C     srvdit        BEGSR
109900020607     c*
110000020607     C     vdit          CHAIN     ansif01L
110100020607     C                   if        not %found(ansif01l)
110200020607     C                   SETON                                        8799
110300020607     c                   movel     *blanks       vddit
110400020607     c                   else
110500020607     c                   movel     sifdesbrev    vddit
110600020607     C                   END
110700020607     c*
110800020607     C                   ENDSR
110900011126     c**********************************************************************
111000011126     C** FILIALE
111100011126     c**********************************************************************
111200011126     C     srvfil        BEGSR
111300011126     c*
111400011126     C                   MOVE      VFIL          FILV              3 0
111500011126     C     FILV          CHAIN     AZORG
111600011126     C                   if        not %found(azorg01l)
111700011126     C                   SETON                                        8899
111800011126     c                   else
111900011126     C                   MOVEL     ORGDES        VDFIL
112000011126     C                   END
112100011126     c*
112200011126     C                   ENDSR
112300020607     c**********************************************************************
112400020607     C** società in dotazione
112500020607     c**********************************************************************
112600020607     C     srvsoc        BEGSR
112700020607     c*
112800020607     C     vsoc          CHAIN     ansif01L
112900020607     C                   if        not %found(ansif01l)
113000020607     C                   SETON                                        6099
113100020607     c                   movel     *blanks       vdsoc
113200020607     c                   else
113300020607     c                   movel     sifdesbrev    vdsoc
113400020607     C                   END
113500020607     c*
113600020607     C                   ENDSR
113700011126     c**********************************************************************
113800011126     C** centro di costo
113900011126     c**********************************************************************
114000011126     C     srvcdc        BEGSR
114100011126     c*
114200011126     C                   movel     *blanks       VDCDC
114300011126     C                   movel     VCDC          ententita
114400011126     C     kent          chain     anent01l
114500011126     C                   IF        NOT %FOUND(ANENT01L)
114600011126     C                   SETON                                        6199
114700011126     C                   ELSE
114800011126     C                   move      entdesbrev    VDCDC
114900011126     C                   END
115000011126     c*
115100011126     C                   ENDSR
115200011126     c**********************************************************************
115300011126     C** categoria
115400011126     c**********************************************************************
115500011126     C     srvcat        BEGSR
115600011126     c*
115700011126     c                   movel     'XA'          parcod            2
115800011126     c                   movel(P)  VCAT          parkey
115900011126     c                   exsr      srtab
116000011126     c                   movel     parkey        vcat
116100011126     c                   if        *in99
116200011220     C                   MOVEL     *blanks       dsxa
116300011220     C                   MOVEL     *blanks       VDCAT
116400011126     C                   SETON                                        85
116500011126     C                   else
116600011220     C                   MOVEL     TBLUNI        dsxa
116700011220     C                   MOVEL     §xades        VDCAT
116800011126     C                   END
116900011126     c*
117000011126     C                   ENDSR
117100011126     c**********************************************************************
117200011126     C** autista
117300011126     c**********************************************************************
117400011126     C     srvcau        BEGSR
117500011126     c                   movel     'XN'          parcod
117600011126     c                   movel(P)  VCAU          parkey
117700011126     c                   exsr      srtab
117800011126     c                   movel     parkey        vcau
117900011126     c                   if        *in99
118000011126     C                   MOVEL     *blanks       VDAU
118100011126     C                   SETON                                        55
118200011126     C                   else
118300011126     C                   MOVEL     TBLUNI        VDAU
118400011126     C                   END
118500011126     c*
118600011126     C                   ENDSR
118700011126     c**********************************************************************
118800011126     C** carrozzeria
118900011126     c**********************************************************************
119000011126     C     srvcar        BEGSR
119100011126     c*
119200011126     c                   movel(P)  VCAR          parkey
119300011126     c                   movel     'XB'          parcod
119400011126     c                   exsr      srtab
119500011126     c                   movel     parkey        vcar
119600011126     c                   if        *in99
119700011126     C                   MOVEL     *blanks       VDCAR
119800011126     C                   SETON                                        86
119900011126     C                   else
120000011126     C                   MOVEL     TBLUNI        VDCAR
120100011126     C                   END
120200011126     c*
120300011126     C                   ENDSR
120400011126     c**********************************************************************
120500011126     C** consumi
120600011126     c**********************************************************************
120700011126     C     srvcon        BEGSR
120800011126     c*
120900011207     C                   clear                   dsxh
121000011126     c                   movel     'XH'          parcod
121100011126     c                   movel(P)  VCON          parkey
121200011126     c                   exsr      srtab
121300011126     c                   movel     parkey        vcon
121400011126     c                   if        *in99
121500011126     C                   SETON                                        44
121600011126     C                   else
121700011207     C                   MOVEL     TBLUNI        dsxh
121800011126     C                   END
121900011207     c                   movel     §xhdes        vdcon
122000011207     c                   z-add     §xhmed        vmed
122100011126     c*
122200011126     C                   ENDSR
122300011126     c**********************************************************************
122400011126     C** tipo proprietà
122500011126     c**********************************************************************
122600011126     C     srvcsp        BEGSR
122700011126     c*
122800011126     c                   movel     'XR'          parcod
122900011126     c                   movel(P)  VCSP          parkey
123000011126     c                   exsr      srtab
123100011126     c                   movel     parkey        vcsp
123200011126     c                   if        *in99
123300011126     C                   MOVEL     *blanks       VDSP
123400011126     C                   SETON                                        56
123500011126     C                   else
123600011126     C                   MOVEL     TBLUNI        VDSP
123700011126     C                   END
123800011126     c*
123900011126     C                   ENDSR
124000011126     c**********************************************************************
124100011126     C** tipo manutenzione
124200011126     c**********************************************************************
124300011126     C     srvman        BEGSR
124400011126     c*
124500011126     c                   movel     'XC'          parcod
124600011126     c                   movel(P)  VMAN          parkey
124700011126     c                   exsr      srtab
124800011126     c                   movel     parkey        vman
124900011126     c                   if        *in99
125000011126     C                   MOVEL     *blanks       VDMAN
125100011126     C                   SETON                                        78
125200011126     C                   else
125300011126     C                   MOVEL     TBLUNI        VDMAN
125400011126     C                   END
125500011126     c*
125600011126     C                   ENDSR
125700011126     c**********************************************************************
125800011126     C** utilizzo
125900011126     c**********************************************************************
126000011126     C     srvuti        BEGSR
126100011126     c*
126200011126     c                   movel     'XL'          parcod
126300011126     c                   movel(P)  VUTI          parkey
126400011126     c                   exsr      srtab
126500011126     c                   movel     parkey        vuti
126600011126     c                   if        *in99
126700011126     C                   MOVEL     *blanks       VDES
126800011126     C                   SETON                                        45
126900011126     C                   else
127000011126     C                   MOVEL     TBLUNI        VDES
127100011126     C                   END
127200011126     c*
127300011126     C                   ENDSR
127400011126     c**********************************************************************
127500011126     C** stato
127600011126     c**********************************************************************
127700011126     C     srvsta        BEGSR
127800011126     c*
127900011126     c                   movel     'XF'          parcod
128000011126     c                   movel(P)  vsta          parkey
128100011126     c                   exsr      srtab
128200011126     c                   movel     parkey        vsta
128300011126     c                   if        *in99
128400011126     C                   MOVEL     *blanks       VDsta
128500011126     C                   SETON                                        83
128600011126     C                   else
128700011126     C                   MOVEL     TBLUNI        VDsta
128800011126     C                   END
128900011126     c*
129000011126     C                   ENDSR
129100011126     c**********************************************************************
129200011126     C** ass.ne RCA auto
129300011126     c**********************************************************************
129400011126     C     srvcrc        BEGSR
129500011126     c*
129600011126     c                   movel     'XD'          parcod
129700011126     c                   movel(P)  vcrc          parkey
129800011126     c                   exsr      srtab
129900011126     c                   movel     parkey        vcrc
130000011126     c                   if        *in99
130100011126     C                   MOVEL     *blanks       VDcrc
130200011126     C                   SETON                                        74
130300011126     C                   else
130400011126     C                   MOVEL     TBLUNI        VDcrc
130500011126     C                   END
130600011126     c*
130700011126     C                   ENDSR
130701031127     c**********************************************************************
130702031127     C** equipaggiamento
130703031127     c**********************************************************************
130704031127     C     srvequi       BEGSR
130705031127     c*
130706031127     c                   movel     'XT'          parcod
130708031127     c                   exsr      srtabequi
130709031127     c                   movel     parkey        vequi
130710031128     C                   MOVEL     *blanks       VDequ1
130711031128     C                   MOVEL     *blanks       VDequ2
130712031127     c                   if        *in99
130713031127     C                   SETON                                        76
130714031127     C                   else
130715031128     C                   MOVEL     des1          VDequ1
130716031128     C                   MOVEL     des2          VDequ2
130717031127     C                   END
130718031127     c*
130719031127     C                   ENDSR
130800011126     c**********************************************************************
130900011126     C** ass.ne inc/furto
131000011126     c**********************************************************************
131100011126     C     srvcfi        BEGSR
131200011126     c*
131300011126     c                   movel     'XD'          parcod
131400011126     c                   movel(P)  vcfi          parkey
131500011126     c                   exsr      srtab
131600011126     c                   movel     parkey        vcfi
131700011126     c                   if        *in99
131800020111     c                   clear                   dsxd
131900011126     C                   MOVEL     *blanks       VDcfi
132000011126     C                   SETON                                        75
132100011126     C                   else
132200020111     C                   MOVEL     TBLUNI        dsxd
132300020111     C                   MOVEL     §xddes        vdcfi
132400011126     C                   END
132500011126     c*
132600011126     C                   ENDSR
132700011126     c**********************************************************************
132800011126     C** tariffa
132900011126     c**********************************************************************
133000011126     C     srvcta        BEGSR
133100011126     c*
133200011126     c                   movel     'XI'          parcod
133300011126     c                   movel(P)  vcta          parkey
133400011126     c                   exsr      srtab
133500011126     c                   movel     parkey        vcta
133600011126     c                   if        *in99
133700011126     C                   MOVEL     *blanks       dstar
133800011126     C                   SETON                                        63
133900011126     C                   else
134000011126     C                   MOVEL     TBLUNI        dstar
134100011126     C                   END
134200011126     c*
134300011126     C                   ENDSR
134400011123     C**********************************************************************
134500011123     C** Controlli e decodifiche campi con tabel00f
134600011123     C**********************************************************************
134700011123     C     srtab         BEGSR
134800011123     c                   if        fld <> *blanks
134900011126     c                   movel     parkey        parkeys           8
135000011123     C                   CALL      'TRUL19R'
135100011123     c                   parm                    parcod            2
135200011123     c                   parm      '1'           parord            1
135300011123     c                   parm      *blanks       parkey            8
135400011126     c                   parm      *blanks       cmd               1
135500011126     c* ritorno senza scelta
135600011126     c                   if        cmd = 'L'
135700011126     c* in aggiornamento non è possibile cambiare l'utista
135800011126     c                             or (§fld = 'VCAU' and *in84)
135900011126     c                   movel     parkeys       parkey
136000011123     C                   END
136100011126     C                   END
136200011123     C                   MOVEL     *blanks       tbluni
136300011123     C                   MOVEL     parcod        COD
136400011123     C                   MOVEL(p)  parkey        KEY
136500011123     C     KTAB          CHAIN     TABEL                              99
136600011123     c*
136700011123     C                   ENDSR
136701031127     C**********************************************************************
136702031127     C** Controlli e decodifiche equipaggiamento
136703031127     C**********************************************************************
136704031127     C     srtabequi     BEGSR
136705031127     c                   if        fld = *blanks
136706031127     c                   eval      tipo = 'C'
136707031128     c                   movel(P)  vequi         parkey
136708031127     c                   else
136709031127     c                   eval      tipo = 'R'
136710031128     c                   clear                   parkey
136711031127     c                   end
136712031127     C                   CALL      'TRMZ46R'
136713031127     c                   parm                    tipo              1
136714031128     c                   parm                    parkey
136715031127     c                   parm      *blanks       des1             65
136716031127     c                   parm      *blanks       des2             65
136717031128     c                   if        parkey <> ' '
136724031127     C                   MOVEL     *blanks       tbluni
136725031127     C                   MOVEL     'XT'          COD
136726031127     C                   MOVEL(p)  parkey        KEY
136727031127     C     KTAB          CHAIN     TABEL                              99
136728031128     c                   end
136729031127     c*
136730031127     C                   ENDSR
136800011123     c**********************************************************************
136900011123     c* pulisce 1° formato
137000011123     c**********************************************************************
137100011123     C     clr00         BEGSR
137200011123     C                   Z-ADD     0             §RIGA
137300011123     C                   Z-ADD     0             §COLON
137400011123     C                   CLEAR                   §FLD
137500011123     C                   ENDSR
137600011123     c**********************************************************************
137700011123     c* pulisce 1° formato
137800011123     c**********************************************************************
137900011123     C     clr01         BEGSR
138000011126     c                   exsr      clr00
138100011123     C                   Z-ADD     0             §rig
138200011123     C                   Z-ADD     0             §col
138300011123     C                   Z-ADD     0             VNRF
138400011218     C                   MOVEL     *BLANKS       Vcau
138500011218     C                   MOVEL     *BLANKS       VTGA
138600011123     C                   MOVEL     *BLANKS       VTGN
138700011123     C                   MOVEL     *BLANKS       VTLA
138800011123     C                   ENDSR
138900011126     c**********************************************************************
139000011126     c* pulisce 2° formato
139100011126     c**********************************************************************
139200011126     C     clr03         BEGSR
139300011126     C                   Z-ADD     0             §rig
139400011126     C                   Z-ADD     0             §col
139500011126     c                   exsr      clr00
139600011126     C                   ENDSR
139700011122     c**********************************************************************
139800011122     C*PULIZIA DEI CAMPI A VIDEO
139900011122     c**********************************************************************
140000011122     c     clr02         begsr
140100011126     c                   exsr      clr03
140200011122     C                   MOVEL     *BLANKS       VTLA
140300011122     C                   MOVEL     *BLANKS       VSTA
140400011122     C                   MOVEL     *BLANKS       VDSTA
140500011122     C                   MOVEL     *BLANKS       VTGA
140600011122     C                   MOVEL     *BLANKS       VTGN
140700011122     C                   MOVEL     *BLANKS       VCAT
140800011122     C                   MOVEL     *BLANKS       VDCAT
140900011122     C                   MOVEL     *BLANKS       VMAR
141000011122     C                   MOVEL     *BLANKS       VTIP
141100011122RE   C                   MOVEL     *BLANKS       VCIL
141200011122     C                   MOVEL     *BLANKS       VCAR
141300011122     C                   MOVEL     *BLANKS       VDCAR
141400011122     C                   Z-ADD     0             VIMM
141500011220     C                   MOVE      udate         dataiso
141600011220     C                   move      dataiso       DATAeur
141700011220     C                   move      dataeur       DATAUT
141800011122     C                   MOVEL     *BLANKS       VMAN
141900011122     C                   MOVEL     *BLANKS       VDMAN
142000011122     C                   MOVEL     *BLANKS       VUTI
142100011122     C                   MOVEL     *BLANKS       VDES
142200011122     C                   MOVE      *BLANKS       VDSP
142300011122     C                   MOVE      *BLANKS       VCSP
142400011122     C                   MOVE      *BLANKS       VCAU
142500011122     C                   MOVE      *BLANKS       VDAU
142600011122     C                   Z-ADD     0             VSCAD
142700011122     C                   Z-ADD     0             VASS
142800011122     C                   MOVEL     *BLANKS       VALI
142900011122     C                   MOVEL     *BLANKS       Vcon
143000011122     C                   MOVEL     *BLANKS       Vdcon
143100011207     C                   z-add     0             Vmed
143200011122     C                   Z-ADD     0             VTAR
143300011122     C                   Z-ADD     0             VPPC
143400011122     C                   Z-ADD     0             VPTR
143500011122     C                   Z-ADD     0             VNGM
143600011122     C                   MOVEL     *BLANKS       VTG1
143700011122     C                   MOVEL     *BLANKS       VTG2
143800011122     C                   MOVEL     *BLANKS       VTG3
143900011123     c                   movel     *blanks       vDIT
144000011219     c                   movel     *blanks       vdDIT
144100011122     C                   MOVEL     *BLANKS       VFIL
144200011126     C                   MOVEL     *BLANKS       VFOR
144300020607     c                   movel     *blanks       vsoc
144400020607     c                   movel     *blanks       vdsoc
144500011126     C                   MOVEL     *BLANKS       VDFOR
144600011122     C                   MOVEL     *BLANKS       VDFIL
144700011122     C                   MOVEL     *BLANKS       VCDC
144800011122     C                   MOVEL     *BLANKS       VDCDC
144900031028     C                   Z-ADD     0             telev
145000031028     C                   Z-ADD     0             VDUR
145100011122     C                   Z-ADD     0             VDTC
145200011122     C                   Z-ADD     0             VDTP
145300011122     C                   Z-ADD     0             VDRC
145400011122     C                   MOVEL     *BLANKS       VCRC
145500011122     C                   MOVEL     *BLANKS       VDCRC
145501031127     C                   MOVEL     *BLANKS       Vequi
145502031128     C                   MOVEL     *BLANKS       VDequ1
145503031128     C                   MOVEL     *BLANKS       VDequ2
145600011122     C                   Z-ADD     0             VDFI
145700011122     C                   MOVEL     *BLANKS       VCFI
145800011122     C                   MOVEL     *BLANKS       VDCFI
145900011122     C                   MOVEL     *BLANKS       VNAC
146000011122     C                   Z-ADD     0             VDAC
146100011122     C                   Z-ADD     0             VSAC
146200011122     C                   Z-ADD     0             VPQL
146300011122     C                   MOVEL     '1'           VTTA
146400011123     C                   z-add     0             VCTA
146500011122     C                   Z-ADD     0             VCVF
146600011122     C                   MOVEL     *BLANKS       VDCTA
146700011122     C                   MOVEL     *BLANKS       VPBL
146800011122     C                   Z-ADD     0             VIMT
146900011122     C                   Z-ADD     0             VIMP
147000011122     C                   Z-ADD     0             VIMA
147100011122     C                   Z-ADD     0             VIMF
147200011122     C                   Z-ADD     0             VIMK
147300000000     C                   ENDSR
147400011122     C**********************************************************************
147500000000     C** AGGIORNAMENTO O INSERIMENTO
147600011122     C**********************************************************************
147700000000     C     AGGIOR        BEGSR
147800011122     C     KGAAM         CHAIN     mzAME000                           21
147900000000     C                   Z-ADD     0             AMEDAC
148000000000     C                   Z-ADD     0             AMESAC
148100000000     C                   Z-ADD     0             AMEDUR
148200000000     C                   Z-ADD     0             AMEDTC
148300000000     C                   Z-ADD     0             AMEDTP
148400000000     C                   Z-ADD     0             AMEDRC
148500000000     C                   Z-ADD     0             AMEDFI
148600000000     C                   Z-ADD     VNRF          AMENRF
148700000000     C                   MOVEL     VTLA          AMETLA
148800000000     C                   MOVEL     VTGA          AMETGA
148900000000     C                   MOVEL     VTGN          AMETGN
149000000000     C                   MOVEL     VSTA          AMESTA
149100000000     C                   MOVEL     VCAT          AMECAT
149200000000     C                   MOVEL     VMAR          AMEMAR
149300000000     C                   MOVEL     VTIP          AMETIP
149400970416RE   C                   MOVEL     VCIL          AMECIL
149500000000     C                   MOVEL     VCAR          AMECAR
149600910215     C                   Z-ADD     MES           ANTO
149700910215     C                   Z-ADD     ANN           MES
149800910215     C                   Z-ADD     ANTO          ANN
149900000000     C                   Z-ADD     VIMM          AMEIMM
150000000000     C                   MOVEL     VMAN          AMEMAN
150100000000     C                   MOVEL     VUTI          AMEUTI
150200000000     C                   Z-ADD     VASS          AMEASS
150300011122     C                   MOVEL     Vcon          AMEusi
150400011122     C                   MOVEL     VALI          AMEALI
150500000000     C                   Z-ADD     VTAR          AMETAR
150600000000     C                   Z-ADD     VPPC          AMEPPC
150700000000     C                   Z-ADD     VPQL          AMEPQL
150800000000     C                   Z-ADD     VPTR          AMEPTR
150900000000     C                   Z-ADD     VNGM          AMENGM
151000000000     C                   MOVEL     VTG1          AMETG1
151100000000     C                   MOVEL     VTG2          AMETG2
151200000000     C                   MOVEL     VTG3          AMETG3
151300011123     C                   movel     VDIT          AMEDIT
151400020607     C                   movel     Vsoc          AMEsoc
151500031029     C                   MOVEl     telev         AMEtel
151600031028     C                   MOVEL     VFIL          AMEFIL
151700000000     C                   MOVEL     VCDC          AMECDC
151800011123     C                   MOVEL     Vfor          AMEfor
151900000000     C*
152000000000     C     VDUR          IFNE      0
152100011122     C                   move      VDUR          DATAeur
152200011122     C                   MOVE      DATAeur       dataiso
152300011122     C                   MOVE      DATAIso       AMEDUR
152400000000     C                   END
152500000000     C*
152600000000     C     VDTC          IFNE      0
152700011122     C                   move      VDtc          DATAeur
152800011122     C                   MOVE      DATAeur       dataiso
152900011122     C                   MOVE      DATAIso       AMEDtc
153000000223     C                   END
153100000000     C                   Z-ADD     VIMT          AMEIMT
153200000000     C*
153300000000     C     VDTP          IFNE      0
153400011122     C                   move      VDtp          DATAeur
153500011122     C                   MOVE      DATAeur       dataiso
153600011122     C                   MOVE      DATAIso       AMEDtp
153700000223     C                   END
153800000000     C                   Z-ADD     VIMP          AMEIMP
153900000000     C*
154000000000     C     VDRC          IFNE      0
154100011122     C                   move      VDrc          DATAeur
154200011122     C                   MOVE      DATAeur       dataiso
154300011122     C                   MOVE      DATAIso       AMEDrc
154400000223     C                   END
154500000000     C                   MOVEL     VCRC          AMECRC
154600000000     C                   Z-ADD     VIMA          AMEIMA
154601031127     C                   MOVEL     Vequi         AMEequ
154700000000     C*
154800000000     C     VDFI          IFNE      0
154900011122     C                   move      VDfi          DATAeur
155000011122     C                   MOVE      DATAeur       dataiso
155100011122     C                   MOVE      DATAIso       AMEDfi
155200000000     C                   END
155300000000     C                   MOVEL     VCFI          AMECFI
155400000000     C                   Z-ADD     VIMF          AMEIMF
155500000000     C                   Z-ADD     VIMK          AMEIMK
155600000000     C                   MOVEL     VNAC          AMENAC
155700000000     C*
155800000000     C     VDAC          IFNE      0
155900011122     C                   move      VDac          DATAeur
156000011122     C                   MOVE      DATAeur       dataiso
156100011122     C                   MOVE      DATAIso       AMEDac
156200000000     C                   END
156300000000     C*
156400000000     C     VSAC          IFNE      0
156500011122     C                   move      Vsac          DATAeur
156600011122     C                   MOVE      DATAeur       dataiso
156700011122     C                   MOVE      DATAIso       AMEsac
156800000000     C                   END
156900910214     C     VSCAD         IFNE      0
157000011122     C                   move      Vscad         DATAeur
157100011122     C                   MOVE      DATAeur       dataiso
157200011122     C                   MOVE      DATAIso       AMEDsp
157300910214     C                   END
157400910215     C     DATAUT        IFNE      0
157500011122     C                   move      dataut        DATAeur
157600011122     C                   MOVE      DATAeur       dataiso
157700011122     C                   MOVE      DATAIso       AMEDal
157800910215     C                   END
157900910214     C                   MOVEL     VCSP          AMECSP
158000910214     C                   MOVEL     VCAU          AMECAU
158100000000     C                   MOVEL     VTTA          AMETTA
158200011123     C                   z-add     VCTA          AMECTA
158300000000     C                   Z-ADD     VCVF          AMECVF
158400000000     C                   MOVEL     VPBL          AMEPBL
158500011122     C   21              WRITE     mzAME000
158600011122     C  N21              UPDATE    mzAME000
158700011123     c*
158800011123     c                   exsr      clr01
158900011123     c*
159000000000     C                   ENDSR
159100011121     C*---------------------------------------------------------------*
159200011121     C     *inzsr        BEGSR
159300011121     C*---------------------------------------------------------------*
159400011121     C     *ENTRY        PLIST
159500011121     C                   PARM                    KPJBA
159600011122     c                   do
159700011121     C                   Z-ADD     1             CODUT
159800011121     C                   CALL      'XPARUT'
159900011121     C                   PARM                    UTEDSE
160000011122     C     CODUT         ifeq      -1
160100011122     c                   seton                                        lr
160200011122     c                   leave
160300011122     c                   end
160400011121     C                   MOVEL     RAGUT         RSUT             20
160500011122     C                   MOVEL     RAGUT         RSUT2            25
160600011221     C* CONTROLLO SE GESTIRE SOLO LA VISUALIZZAZIONE DEI RECORD IN BASE
160700011221     C* AL CODICE DELL'AZIONE
160800011221     C     KCDAZ         COMP      'MZ92'                                 20
160900011122      * Reperimento dati società
161000011122     C                   clear                   SocXsc
161100011122     C                   eval      tipxsc = 'SOC001'
161200011122     C                   exsr      rtvsocieta
161300011122     C                   if        rtnxsc = '1'
161400011122     C                   seton                                        lr
161500011122     c                   leave
161600011122     C                   else
161700011122     C                   MOVEL     XSOCDS        xsoc001ds
161800011122     C                   endif
161900011121     C* POSIZIONAMENTO TABELLE                             *
162000011121     C     KTAB          KLIST                                                  *
162100011121     C                   KFLD                    CODUT                          *
162200011121     C                   KFLD                    COD               2            *
162300011121     C                   KFLD                    KEY               8            *
162400011123     C* FORNITORE
162500011123     C     KFOR          KLIST                                                  *
162600011123     C                   KFLD                    VDIT                           *
162700011123     C                   KFLD                    VFOR                           *
162800011123     C                   KFLD                    FILLL                          *
162900011121     C* POSIZIONAMENTO X N.RIFERIMENTO                     *
163000011121     C     KGAAM         KLIST                                                  *
163100011121     C                   KFLD                    VNRF                           *
163200011121     C* POSIZIONAMENTO X N.DI TARGA                        *
163300011121     C     KTAR          KLIST                                                  *
163400011121     C                   KFLD                    VTGA                           *
163500011121     C                   KFLD                    VTGN                           *
163600011122     C* POSIZIONAMENTO SU ANENT                            *
163700011122     C     Kent          KLIST
163800011122     C                   KFLD                    xscsoc
163900011122     C                   KFLD                    enttprifan
164000011122     C                   KFLD                    ententita
164100011122     C                   move      'CDC'         enttprifan
164200011121     C*----------------------------------------------------*
164300011121     C* RICERCA TARGA X CITTA'
164400011121     C     RICTAR        PLIST
164500011121     C                   PARM                    CODUT
164600011121     C                   PARM                    RSUT
164700011121     C                   PARM                    VTGA
164800011121     C                   PARM                    VTGN
164900011122     c                   enddo
165000011122     c*
165100011121     C                   ENDSR
165200011122     C*---------------------------------------------------------------------
165300011122     c*  Reperisco società.
165400011122     C*---------------------------------------------------------------------
165500011122     C     rtvsocieta    begsr
165600011122     C*
165700011122     C                   CALLb     'XSOC'
165800011122   >>C                   Parm                    TipXsc            6
165900011122   >>C                   Parm                    SocXsc            3
166000011122   >>C                   Parm                    CdsXsc            9 0
166100011122   >>C                   Parm                    ModXsc            3
166200011122   >>C                   Parm      *BLANK        RtnXsc            1
166300011122     C                   Parm                    XsocDS
166400011122   >>C                   Parm                    Kpjba
166500011122     C*
166600011122     C                   endsr
166700000000     C*---------------------------------------------------------------*
