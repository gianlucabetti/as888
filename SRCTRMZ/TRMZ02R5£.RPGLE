000100030113     H DFTACTGRP(*NO) BNDDIR('PJXBND') ACTGRP('QILE')
000200000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*          ANNULLO E CAMBIO TARGA AUTOMEZZI          *
000400000000     F*----------------------------------------------------*
000500011129     FmzAME01L  UF   E           K DISK
000600011129     FmzAME02L  IF   E           K DISK
000700011129     F                                     RENAME(MZAME000:mzAME002)
000800031028     FmzAME05L  IF   E           K DISK
000900031028     F                                     RENAME(MZAME000:mzAME005)
001000030113     Fanent01l  IF   E           K DISK
001100011129     FmzAMH00F  O  A E             DISK
001200910201     FTABEL00F  IF   E           K DISK
001300910218     FAZORG01L  IF   E           K DISK
001400020607     Fansif01L  IF   E           K DISK
001500011129     FTRmz02D5  CF   E             WORKSTN
001600031028     D SK              S             20    DIM(7) CTDATA PERRCD(1)
001700000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001800000000     D KPJBA         E DS
001900030113     D Ana032ds      E DS                  INZ
002000020607     D ana007ds      e ds                  inz
002100030113    >D xsoc001ds     E DS
002200030113     D* DS Interna per dati di output di XSOC
002300030113     D XSOCDS          DS          1000
002400011129     D DTAiso          S               D   INZ
002500011129     D DTAEUR          S               D   DATFMT(*EUR) INZ
002600031028     D com20           S                   like(ametel) INZ
002700000000     D*
002800000000     D PARAM           DS
002900000000I    D  PARANN                 1      1
003000000000     D  PARNRF                 2      8  0
003100000000     D  PARTNA                 9     10
003200000000     D  PARTNN                11     19
003300011129     D  PARCAU                20     21
003400011129     D  PARCSP                22     23
003500011129     D  PARFIL                24     26
003600011129     D  PARDAT                27     34  0
003700020607     D  PARsoc                35     37
003800030113     D  PARcdc                38     45
003900040213     D  PARtel                46     55  0
004000021021     c                   z-add     0             datain
004100011129     c  nlr              do        *hival
004200020607     C*                  Z-ADD     *DATE         DATAIN
004300031029     c                   if        *in99
004400031029     c                   setoff                                       99
004500031029     c                   write     ga02d500
004600031029     c                   seton                                        99
004700031029     c                   end
004800910201     C                   EXFMT     GA02D500
004900011129     C   Kc              leave
005000031028     C   Kd              exsr      ric00
005100011129     c                   exsr      ctr00
005200011129     c   99              iter
005300000000      *
005400011129     c                   do        *hival
005500011129     c* primo video
005600011129     c                   EXSR      CLR00
005700011129     c                   if        *in99
005800011129     c                   setoff                                       99
005900011129     c                   write     ga02d501
006000011129     c                   seton                                        99
006100011129     c                   end
006200000000     C                   EXFMT     GA02D501
006300141126     c                   clear                   $msg
006400141126     c                   setoff                                       28
006500011129      * FINE LAVORO
006600011129     c                   if        *inkc or *inkl
006700011129     C                   leave
006800011129     c                   end
006900011129      * RICERCA
007000011129     C                   if        *inkd
007100011129     C                   EXSR      RIC01
007200011129     c                   iter
007300011129     c                   end
007400000000     C*
007500910215     C  N49              EXSR      CTRLP1
007600011129     C   99              iter
007700011129     c* conferma
007800011129     C                   if        *inkf
007900011129      * CONFERMA ANNULLAMENTO
008000910201     C   49              EXSR      ANNULL
008100910215      * CONFERMA CAMBIO VARI
008200910524     C  N49              EXSR      CAMBIO
008300011129     C   49              MOVE      'A'           PARANN
008400011129     C   51              MOVE      'T'           PARANN
008500011129     c                   leave
008600011129     c                   end
008700011129     c                   enddo
008800011129     c   kf              leave
008900011129     c   kc              leave
009000011129     c                   enddo
009100000000     C                   SETON                                            LR
009200000000     C                   MOVEL     PARAM         KPJBU
009300011129     c**********************************************************************
009400011129     c     ctr00         begsr
009500031029     c                   movel     *blanks       fld
009600011129     c                   setoff                                       99
009700011129     c                   do
009800020109     C                   IF        DATAIN = 0
009900021021     C                   SETON                                        9940
010000020109     c                   leave
010100020109     C                   ELSE
010200021021     c     *eur          test(d)                 datain                 40
010300021021     c                   if        *in40
010400021021     c                   seton                                        99
010500021021     c                   leave
010600020109     C                   END
010700021021     C                   END
010800011219     c*
010900031029     c                   clear                   dcambiov
011000011219     c                   if        vscl = *blanks
011100031028     c                   seton                                        41
011200031028     c                   else
011300031028     c                   exsr      srvscl
011400031028     c                   end
011500031028     c*
011600031028     c                   if        *in41
011700031028     c                   seton                                        99
011800031028     c                   leave
011900031028     c                   end
012000011129      * STABILISCE DI QUALE MODIFICA SI TRATTA
012100011129     C     VSCL          COMP      'AN'                                   49
012200011129     C     VSCL          COMP      'TG'                                   50
012300011129     C     VSCL          COMP      'AU'                                   51
012400011129     C     VSCL          COMP      'FL'                                   52
012500011129     C     VSCL          COMP      'PR'                                   53
012600020716     C     VSCL          COMP      'SC'                                   54
012700030113     C     VSCL          COMP      'CD'                                   55
012800031028     C     VSCL          COMP      'TL'                                   56
012900011129      * PRELEVO RECORD DA AGGIORNARE
013000011129     C                   Z-ADD     PARNRF        VNRF
013100011129     C     parnrf        CHAIN     mzaME000                           21
013200011129     C                   MOVE      AMETGA        VTGA
013300011129     C                   MOVEL     AMETGN        VTGN
013400011129     C                   MOVE      AMETLA        VTLA
013500011129     C                   MOVE      AMEMAR        VMAR
013600011129     C                   MOVE      AMETIP        VTIP
013700031029     c                   clear                   telev
013800031029     c                   if        ametel <> *blanks
013900040213     C                   MOVEl     AMEtel        telev
014000031029     c                   end
014100011129     C                   MOVE      *BLANK        VDS2
014200011129     C   49              MOVEL     'VENDITA'     VDS1
014300011129     C   50              MOVEL     SK(1)         VDS1
014400011129     C   51              MOVEL     SK(2)         VDS1
014500011129     C   52              MOVEL     SK(3)         VDS1
014600011129     C   53              MOVEL     SK(4)         VDS1
014700021021     C   54              MOVEL     SK(5)         VDS1
014800030113     C   55              MOVEL     SK(6)         VDS1
014900031028     C   56              MOVEL     SK(7)         VDS1
015000011129     C   50              MOVE      *BLANKs       VTNA
015100011129     C   50              MOVE      *blanks       VTNN
015200011129     C   51              MOVE      *BLANKS       VCAU
015300011129     C   51              MOVE      *BLANKS       Vnom
015400011129     C   52              MOVE      *blanks       VFIL
015500011129     C   52              MOVE      *blanks       Vdes
015600011129     C   53              MOVE      *blanks       VCSP
015700011129     C   53              MOVE      *blanks       VdSP
015800020607     C   54              MOVE      *blanks       Vsoc
015900020607     C   54              MOVE      *blanks       Vdsoc
016000030113     C   55              MOVE      *blanks       Vcdc
016100030113     C   55              MOVE      *blanks       Vdcdc
016200031028     C   56              clear                   telev
016300011129     c                   enddo
016400011129     c                   endsr
016500011129     C*----------------------------------------------------*
016600000000     C*  CAMBIO TARGA
016700000000     C*----------------------------------------------------*
016800000000     C     CAMBIO        BEGSR
016900910204     C   50              MOVE      VTNA          AMETGA
017000910204     C   50              MOVEL     VTNN          AMETGN
017100910201     C   51              MOVE      VCAU          AMECAU
017200011129     c   51              move      datain        dtaeur
017300011129     c   51              move      dtaeur        dtaiso
017400040212     c   51              adddur    1:*d          dtaiso
017500011129     C   51              MOVE      Dtaiso        AMEDAL
017600910201     C   53              MOVE      VCSP          AMECSP
017700910201     C   52              MOVE      VFIL          AMEFIL
017800020607     C   54              MOVEl     Vsoc          AMEsoc
017900030113     C   55              MOVEl     Vcdc          AMEcdc
018000031029     C   56              MOVEl     telev         AMEtel
018100011129     C                   UPDATE    mzAME000
018200011129      * SCRIVE RECORD DI mzAMH00F
018300910507     C                   Z-ADD     VNRF          AMHNRF
018400910507     C                   MOVE      VTGA          AMHTGA
018500910507     C                   MOVEL     VTGN          AMHTGN
018600910507     C                   MOVE      VTLA          AMHTLA
018700910507     C                   MOVE      VDS2          AMHDS2
018800910507     C                   MOVE      VDS1          AMHDS1
018900911002     C   50              MOVE      VTNA          AMHTNA
019000911002     C   50              MOVE      VTNN          AMHTNN
019100011129     c                   move      datain        dtaeur
019200011129     c                   move      dtaeur        dtaiso
019300011129     C                   MOVE      Dtaiso        AMHdta
019400910507   N C                   MOVE      PARCAU        AMHCAU
019500011129     c                   move      pardat        dtaeur
019600011129     c                   move      dtaeur        dtaiso
019700011129     C                   MOVE      Dtaiso        AMHdal
019800910507   N C                   MOVE      PARFIL        AMHFIL
019900910507   N C                   MOVE      PARCSP        AMHCSP
020000020607   N C                   MOVEl     PARsoc        AMHsoc
020100030113   N C                   MOVEl     PARcdc        AMHcdc
020200910507     C                   MOVE      VSCL          AMHTIP
020300011129     c                   z-add     0             amhal
020400031029     c                   movel     partel        amhtel
020500011129     C                   WRITE     mzAMH000
020600910507     C*
020700910204     C   50              MOVE      VTNA          PARTNA
020800910204     C   50              MOVEL     VTNN          PARTNN
020900910204     C   51              MOVEL     VCAU          PARCAU
021000040212     c   51              move      datain        dtaeur
021100040212     c   51              adddur    1:*d          dtaeur
021200040212     C   51              MOVE      Dtaeur        pardat
021300910204     C   53              MOVEL     VCSP          PARCSP
021400910204     C   52              MOVEL     VFIL          PARFIL
021500020607     C   54              MOVEL     Vsoc          PARsoc
021600030113     C   55              MOVEL     Vcdc          PARcdc
021700031029     C   56              z-add     telev         PARtel
021800000000     C*
021900000000     C                   ENDSR
022000011129     C*----------------------------------------------------*
022100000000     C*  ANNULLO AUTOMEZZO
022200000000     C*----------------------------------------------------*
022300000000     C     ANNULL        BEGSR
022400011129     C                   DELETE    mzAME000
022500000000      *
022600011129      * SCRIVE RECORD DI mzAMH00F
022700910524      *
022800910524     C                   Z-ADD     VNRF          AMHNRF
022900910524     C                   MOVE      VTGA          AMHTGA
023000910524     C                   MOVEL     VTGN          AMHTGN
023100910524     C                   MOVE      VTLA          AMHTLA
023200910524     C                   MOVE      VDS2          AMHDS2
023300910524     C                   MOVE      VDS1          AMHDS1
023400011129     c                   move      datain        dtaeur
023500011129     c                   move      dtaeur        dtaiso
023600011129     C                   MOVE      Dtaiso        AMHdta
023700910524   N C                   MOVE      PARCAU        AMHCAU
023800011129     c                   move      pardat        dtaeur
023900011129     c                   move      dtaeur        dtaiso
024000011129     C                   MOVE      Dtaiso        AMHdal
024100910524   N C                   MOVE      PARFIL        AMHFIL
024200020607   N C                   MOVEl     PARsoc        AMHsoc
024300910524   N C                   MOVE      PARCSP        AMHCSP
024400910524     C                   MOVE      VSCL          AMHTIP
024500011129     c                   z-add     0             amhal
024600030115     C                   MOVEl     amecat        AMHcat
024700030115   N C                   MOVEl     PARCdc        AMHCdc
024800031028   N C                   MOVEl     PARtel        AMHtel
024900011129     C                   WRITE     mzAMH000
025000040212     C*                  Z-ADD     *DATE         DATAIN
025100000000     C                   ENDSR
025200011129     C*----------------------------------------------------*
025300011129     C*  ricerca
025400000000     C*----------------------------------------------------*
025500031028     C     ric00         BEGSR
025600011129     c                   movel     '1'           fld
025700011129     c                   select
025800031028     C                   when      §fld = 'VSCL'
025900031028     c                   exsr      srvscl
026000031028     c                   endsl
026100031028     C* POSIZIONAMENTO CURSORE
026200031028     C                   Z-ADD     §RIGA         §RIG
026300031028     C                   Z-ADD     §COLON        §COL
026400031028     c                   endsr
026500031028     C*----------------------------------------------------*
026600031028     C*  ricerca
026700031028     C*----------------------------------------------------*
026800031028     C     ric01         BEGSR
026900031028     c                   movel     '1'           fld
027000031028     c                   select
027100031028     C                   when      §fld = 'VCAU' and *in51
027200031028     c                   exsr      srvcau
027300011129     C                   when      §fld = 'VCSP' and *in53
027400011129     c                   exsr      srvcsp
027500011129     C*CODICE FILIALE
027600011129     C                   when      §fld = 'VFIL' and *in52
027700011129     C                   MOVEL     *BLANKS       §COD1
027800011129     C                   MOVE      *BLANKS       §DES1
027900011129     C                   CALL      'TNSD19R'
028000011129     C                   PARM                    §COD1             3
028100011129     C                   PARM                    §TIP              1
028200011129     C                   PARM                    §DES1            25
028300011129     C                   MOVEL     §COD1         VFIL                           *CODICE
028400011129     C                   MOVEL     §DES1         Vdes                           *DESCRIZIONE
028500011221     C                   WHEN      §FLD = 'VTNN' or §fld = 'VTNA'
028600011221     C                   CALL      'TRMZ02R4'    RICTAR
028700020607     C*CODICE societa in dotazione
028800020607     C                   when      §fld = 'VSOC' and *in54
028900020607     C                   reset                   ana007ds
029000020607     C                   eval      opz007 = '01'
029100020607     C                   eval      kpjbu = ana007ds
029200020607     C                   call      'ANA007R'
029300020607     C                   parm                    kpjba
029400020607     C                   eval      ana007ds = kpjbu
029500020607     C                   if        err007 <> '1' and soc007 <> *blanks
029600020607     C                   eval      vsoc = soc007
029700020607     c                   exsr      srvsoc
029800020607     c                   end
029900030113     C*CODICE CDC
030000030113     C                   when      §fld = 'VCDC' and *in55
030100030113    >C                   RESET                   ANA032DS
030200030113    >C                   MOVEL     '01'          OPZ032
030300030113     C                   MOVE      'S'           FSC032
030400030113     C                   MOVE      'CDC'         TPE032
030500030113    >C                   MOVEL     ANA032Ds      KPJBU
030600030113    >C                   CALL      'ANA032R'
030700030113     C                   PARM                    KPJBA
030800030113    >C                   MOVEL     KPJBU         ANA032DS
030900030113     c                   if        ent032 <> *blanks
031000030113     C                   MOVEL     ENT032        VCDC
031100030113     c                   end
031200011129     c                   endsl
031300011129     C* POSIZIONAMENTO CURSORE
031400011129     C                   Z-ADD     §RIGA         §RIG
031500011129     C                   Z-ADD     §COLON        §COL
031600011129     C                   ENDSR
031700011129     C*----------------------------------------------------*
031800011129     C*  CONTROLLI IMMISSIONE
031900011129     C*----------------------------------------------------*
032000011129     C     CTRLP1        BEGSR
032100011129     c                   movel     *blanks       fld               1
032200011129     c                   setoff                                       99
032300011129     c                   select
032400011129      * CONTROLLO TARGA GIA ESISTENTE
032500011129     c                   when      *in50
032600150114     c                   if        vtga = vtna and
032700150114     c                             vtgn = vtnn
032800141126     c                   eval      $msg = 'La targa non è stata variata'
032900141126     c                   seton                                        2899
033000141126     c                   leavesr
033100141126     c                   endif
033200011129     C     KTAR          CHAIN     mzAME002                           21
033300011129     C  N21              SETON                                        7399
033400011129     C* autista
033500011129     c                   when      *in51
033600141126     c                   if        vcau = amecau
033700141126     c                   eval      $msg = 'Autista non variato '
033800141126     c                   seton                                        2899
033900141126     c                   leavesr
034000141126     c                   endif
034100011129     C                   EXSR      SRVCAU
034200011129     C* filiale
034300011129     c                   when      *in52
034400141126     c                   if        vfil <> *blank
034500141126     c                   move      vfil          vfiln             3 0
034600141126     c                   if        vfiln = amefil
034700141126     c                   eval      $msg = 'Filiale non variata '
034800141126     c                   seton                                        2899
034900141126     c                   leavesr
035000141126     c                   endif
035100141126     c                   endif
035200011129     C                   EXSR      SRVfil
035300011129     C* proprietà
035400011129     c                   when      *in53
035500141126     c                   if        vcsp = amecsp
035600141126     c                   eval      $msg = 'Proprietà non variata '
035700141126     c                   seton                                        2899
035800141126     c                   leavesr
035900141126     c                   endif
036000011129     C                   EXSR      SRVCsp
036100020607     C* società in dotazione
036200020607     c                   when      *in54
036300141126     c                   if        vsoc = amesoc
036400141126     c                   eval      $msg = 'Società in dotazione non variata '
036500141126     c                   seton                                        2899
036600141126     c                   leavesr
036700141126     c                   endif
036800020607     C                   EXSR      SRVsoc
036900030115     C* centro di costo
037000030115     c                   when      *in55
037100141126     c                   if        vcdc = amecdc
037200141126     c                   eval      $msg = 'Centro di costo non variato'
037300141126     c                   seton                                        2899
037400141126     c                   leavesr
037500141126     c                   endif
037600030115     C                   EXSR      SRVcdc
037700031028     C* telepass
037800031028     c                   when      *in56
037900141126     c                   if        telev > 0
038000141126     c                   movel     telev         televa           20
038100141126     c                   if        televa = ametel
038200141126     c                   eval      $msg = 'Tessera Telepass non variata'
038300141126     c                   seton                                        2899
038400141126     c                   leavesr
038500141126     c                   endif
038600141126     c                   endif
038700031028     C                   EXSR      SRVtel
038800011129     c                   endsl
038900910204     C*
039000000000     C                   ENDSR
039100011129     C**********************************************************************
039200011129     C** Controlli e decodifiche campi con tabel00f
039300011129     C**********************************************************************
039400011129     C     srtab         BEGSR
039500011129     c                   if        fld <> *blanks
039600011129     c                   movel     parkey        parkeys           8
039700011129     C                   CALL      'TRUL19R'
039800011129     c                   parm                    parcod            2
039900011129     c                   parm      '1'           parord            1
040000011129     c                   parm      *blanks       parkey            8
040100011129     c                   parm      *blanks       cmd               1
040200011129     c* ritorno senza scelta
040300011129     c                   if        cmd = 'L'
040400011129     c                   movel     parkeys       parkey
040500011129     C                   END
040600011129     C                   END
040700011129     C                   MOVEL     *blanks       tbluni
040800011129     C                   MOVEL     parcod        COD
040900011129     C                   MOVEL(p)  parkey        KEY
041000011129     C     KTAB          CHAIN     TABEL                              99
041100011129     c*
041200011129     C                   ENDSR
041300011129     c**********************************************************************
041400011129     c* pulisce 1° formato
041500011129     c**********************************************************************
041600011129     C     clr00         BEGSR
041700011129     C                   Z-ADD     0             §RIGA
041800011129     C                   Z-ADD     0             §COLON
041900011129     C                   CLEAR                   §FLD
042000011129     C                   ENDSR
042100011129     c**********************************************************************
042200031028     C** variazione
042300011129     c**********************************************************************
042400031028     C     srvscl        BEGSR
042500031028     c                   setoff                                       41
042600031028     c                   movel     'XS'          parcod
042700031028     c                   movel(P)  Vscl          parkey
042800011129     c                   exsr      srtab
042900031028     c                   movel     parkey        vscl
043000011129     c                   if        *in99
043100031028     C                   MOVEL     *blanks       dcambiov
043200031028     C                   SETON                                        41
043300011129     C                   else
043400031028     C                   MOVEL     TBLUNI        dcambiov
043500011129     C                   END
043600011129     c*
043700011129     C                   ENDSR
043800031028     c**********************************************************************
043900031028     C** autista
044000031028     c**********************************************************************
044100031028     C     srvcau        BEGSR
044200031028     c                   movel     'XN'          parcod
044300031028     c                   movel(P)  VCAU          parkey
044400141126     c                   if        vcau = amecau
044500141126     c                   seton                                        9974
044600141126     c                   leavesr
044700141126     c                   endif
044800031028     c                   exsr      srtab
044900031028     c                   movel     parkey        vcau
045000031028     c                   if        *in99
045100031028     C                   MOVEL     *blanks       Vnom
045200031028     C                   SETON                                        74
045300031028     C                   else
045400031028     C                   MOVEL     TBLUNI        Vnom
045500031028     C                   END
045600031028     c*
045700031028     C                   ENDSR
045800011129     c**********************************************************************
045900011129     C** proprietà
046000011129     c**********************************************************************
046100011129     C     srvcsp        BEGSR
046200011129     c                   movel     'XR'          parcod
046300011129     c                   movel(P)  VCsp          parkey
046400011129     c                   exsr      srtab
046500011129     c                   movel     parkey        vcsp
046600011129     c                   if        *in99
046700011129     C                   MOVEL     *blanks       VDsp
046800011129     C                   SETON                                        75
046900011129     C                   else
047000011129     C                   MOVEL     TBLUNI        VDsp
047100011129     C                   END
047200011129     c*
047300011129     C                   ENDSR
047400020607     c**********************************************************************
047500020607     C** società in dotazione
047600020607     c**********************************************************************
047700020607     C     srvsoc        BEGSR
047800020607     c*
047900020607     C     vsoc          CHAIN     ansif01L
048000020607     C                   if        not %found(ansif01l)
048100030115     C                   SETON                                        7799
048200020607     c                   movel     *blanks       vdsoc
048300020607     c                   else
048400020607     c                   movel     sifdesbrev    vdsoc
048500020607     C                   END
048600020607     c*
048700020607     C                   ENDSR
048800011129     c**********************************************************************
048900011129     C** FILIALE
049000011129     c**********************************************************************
049100011129     C     srvfil        BEGSR
049200011129     c*
049300011129     C                   MOVE      VFIL          FILV              3 0
049400011129     C     FILV          CHAIN     AZORG
049500011129     C                   if        not %found(azorg01l)
049600011129     C                   MOVEL     *blanks       VDes
049700011129     C                   SETON                                        7699
049800011129     c                   else
049900011129     C                   MOVEL     ORGDES        VDes
050000011129     C                   END
050100011129     c*
050200011129     C                   ENDSR
050300030113     c**********************************************************************
050400030113     C** centro di costo
050500030113     c**********************************************************************
050600030113     C     srvcdc        BEGSR
050700030113     c*
050800030113     C                   movel     *blanks       VDCDC
050900030113     C                   movel     VCDC          ententita
051000030113     C     kent          chain     anent01l
051100030113     C                   IF        NOT %FOUND(ANENT01L)
051200030113     C                   SETON                                        7899
051300030113     C                   ELSE
051400030113     C                   move      entdesbrev    VDCDC
051500030113     C                   END
051600030113     c*
051700030113     C                   ENDSR
051800031028     c**********************************************************************
051900031028     C** telepass
052000031028     c**********************************************************************
052100031028     C     srvtel        BEGSR
052200031028     c*
052300040213     c                   if        telev <> 9999999999
052400031029     c                   movel     telev         com20
052500031028     C     com20         setll     mzame05l                               79
052600031028     C                   IF        *in79
052700031028     C                   SETON                                        99
052800031028     C                   END
052900031106     C                   END
053000031028     c*
053100031028     C                   ENDSR
053200011129     C*---------                                           *
053300011129      * CHIAVI                                             *
053400011129     C*---------                                           *
053500011129     c     *inzsr        begsr
053600011129     C* POSIZIONAMENTO X N.DI TARGA                        *
053700011129     C     KTAR          KLIST                                                  *
053800011129     C                   KFLD                    VTNA                           *
053900011129     C                   KFLD                    VTNN                           *
054000011129     C     KTAB          KLIST                                                  *
054100011129     C                   KFLD                    CODUT                          *
054200011129     C                   KFLD                    COD               2            *
054300011129     C                   KFLD                    KEY               8            *
054400030113     C* POSIZIONAMENTO SU ANENT                            *
054500030113     C     Kent          KLIST
054600030113     C                   KFLD                    xscsoc
054700030113     C                   KFLD                    enttprifan
054800030113     C                   KFLD                    ententita
054900030113     C                   move      'CDC'         enttprifan
055000011129     C* RICERCA TARGA X CITTA'
055100011129     C     RICTAR        PLIST
055200011129     C                   PARM                    CODUT
055300011129     C                   PARM                    RSUT
055400011129     C                   PARM                    vtna
055500011129     C                   PARM                    vtnn
055600011129      *                                                    *
055700011129     C     *ENTRY        PLIST                                                  *
055800011129     C                   PARM                    KPJBA                          *
055900011129     C                   Z-ADD     1             CODUT                          *
056000011129     C                   MOVEL     KPJBU         PARAM
056100011129      *
056200011129     C                   CALL      'XPARUT'                                     *
056300011129     C                   PARM                    PARUT                          *
056400011129     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
056500011129     C     CODUT         ifeq      -1                                           *
056600011129     c                   seton                                        lr
056700011129     c                   end
056800011129     C                   MOVEL     RAGUT         RSUT             20            *
056900030113      * Reperimento dati società
057000030113     C                   clear                   SocXsc
057100030113     C                   eval      tipxsc = 'SOC001'
057200030113     C                   exsr      rtvsocieta
057300030113     C                   if        rtnxsc = '1'
057400030113     C                   seton                                        lr
057500030113     C                   else
057600030113     C                   MOVEL     XSOCDS        xsoc001ds
057700030113     C                   endif
057800011129      *
057900011129     c                   endsr
058000030113     C*---------------------------------------------------------------------
058100030113     c*  Reperisco società.
058200030113     C*---------------------------------------------------------------------
058300030113     C     rtvsocieta    begsr
058400030113     C*
058500030113     C                   CALLb     'XSOC'
058600030113   >>C                   Parm                    TipXsc            6
058700030113   >>C                   Parm                    SocXsc            3
058800030113   >>C                   Parm                    CdsXsc            9 0
058900030113   >>C                   Parm                    ModXsc            3
059000030113   >>C                   Parm      *BLANK        RtnXsc            1
059100030113     C                   Parm                    XsocDS
059200030113   >>C                   Parm                    Kpjba
059300030113     C*
059400030113     C                   endsr
059500011129**
059600011129CAMBIO TARGA
059700011129CAMBIO AUTISTA
059800011129CAMBIO FILIALE
059900011129CAMBIO PROPRIETA'
060000021021CAMBIO SOC.DOTAZIONE
060100030113CAMBIO CDC
060200031028CAMBIO TELEPASS
