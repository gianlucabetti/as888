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
001300160517     FTntbe01l  IF   E           K DISK
001400910218     FAZORG01L  IF   E           K DISK
001500020607     Fansif01L  IF   E           K DISK
001600011129     FTRmz02D5  CF   E             WORKSTN
001700031028     D SK              S             20    DIM(7) CTDATA PERRCD(1)
001800000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001900000000     D KPJBA         E DS
002000030113     D Ana032ds      E DS                  INZ
002100020607     D ana007ds      e ds                  inz
002200030113    >D xsoc001ds     E DS
002300030113     D* DS Interna per dati di output di XSOC
002400160517    >D dmau          E DS
002500160517    >D dcau          E DS
002600030113     D XSOCDS          DS          1000
002700011129     D DTAiso          S               D   INZ
002800011129     D DTAEUR          S               D   DATFMT(*EUR) INZ
002900031028     D com20           S                   like(ametel) INZ
003000000000     D*
003100000000     D PARAM           DS
003200000000I    D  PARANN                 1      1
003300000000     D  PARNRF                 2      8  0
003400000000     D  PARTNA                 9     10
003500000000     D  PARTNN                11     19
003600011129     D  PARCAU                20     21
003700011129     D  PARCSP                22     23
003800011129     D  PARFIL                24     26
003900011129     D  PARDAT                27     34  0
004000020607     D  PARsoc                35     37
004100030113     D  PARcdc                38     45
004200040213     D  PARtel                46     55  0
004300021021     c                   z-add     0             datain
004400011129     c  nlr              do        *hival
004500020607     C*                  Z-ADD     *DATE         DATAIN
004600031029     c                   if        *in99
004700031029     c                   setoff                                       99
004800031029     c                   write     ga02d500
004900031029     c                   seton                                        99
005000031029     c                   end
005100910201     C                   EXFMT     GA02D500
005200011129     C   Kc              leave
005300031028     C   Kd              exsr      ric00
005400011129     c                   exsr      ctr00
005500011129     c   99              iter
005600000000      *
005700011129     c                   do        *hival
005800011129     c* primo video
005900011129     c                   EXSR      CLR00
006000011129     c                   if        *in99
006100011129     c                   setoff                                       99
006200011129     c                   write     ga02d501
006300011129     c                   seton                                        99
006400011129     c                   end
006500000000     C                   EXFMT     GA02D501
006600141126     c                   clear                   $msg
006700141126     c                   setoff                                       28
006800011129      * FINE LAVORO
006900011129     c                   if        *inkc or *inkl
007000011129     C                   leave
007100011129     c                   end
007200011129      * RICERCA
007300011129     C                   if        *inkd
007400011129     C                   EXSR      RIC01
007500011129     c                   iter
007600011129     c                   end
007700000000     C*
007800910215     C  N49              EXSR      CTRLP1
007900011129     C   99              iter
008000011129     c* conferma
008100011129     C                   if        *inkf
008200011129      * CONFERMA ANNULLAMENTO
008300910201     C   49              EXSR      ANNULL
008400910215      * CONFERMA CAMBIO VARI
008500910524     C  N49              EXSR      CAMBIO
008600011129     C   49              MOVE      'A'           PARANN
008700011129     C   51              MOVE      'T'           PARANN
008800011129     c                   leave
008900011129     c                   end
009000011129     c                   enddo
009100011129     c   kf              leave
009200011129     c   kc              leave
009300011129     c                   enddo
009400000000     C                   SETON                                            LR
009500000000     C                   MOVEL     PARAM         KPJBU
009600011129     c**********************************************************************
009700011129     c     ctr00         begsr
009800031029     c                   movel     *blanks       fld
009900011129     c                   setoff                                       99
010000011129     c                   do
010100020109     C                   IF        DATAIN = 0
010200021021     C                   SETON                                        9940
010300020109     c                   leave
010400020109     C                   ELSE
010500021021     c     *eur          test(d)                 datain                 40
010600021021     c                   if        *in40
010700021021     c                   seton                                        99
010800021021     c                   leave
010900020109     C                   END
011000021021     C                   END
011100011219     c*
011200031029     c                   clear                   dcambiov
011300011219     c                   if        vscl = *blanks
011400031028     c                   seton                                        41
011500031028     c                   else
011600031028     c                   exsr      srvscl
011700031028     c                   end
011800031028     c*
011900031028     c                   if        *in41
012000031028     c                   seton                                        99
012100031028     c                   leave
012200031028     c                   end
012300011129      * STABILISCE DI QUALE MODIFICA SI TRATTA
012400011129     C     VSCL          COMP      'AN'                                   49
012500011129     C     VSCL          COMP      'TG'                                   50
012600011129     C     VSCL          COMP      'AU'                                   51
012700011129     C     VSCL          COMP      'FL'                                   52
012800011129     C     VSCL          COMP      'PR'                                   53
012900020716     C     VSCL          COMP      'SC'                                   54
013000030113     C     VSCL          COMP      'CD'                                   55
013100031028     C     VSCL          COMP      'TL'                                   56
013200011129      * PRELEVO RECORD DA AGGIORNARE
013300011129     C                   Z-ADD     PARNRF        VNRF
013400011129     C     parnrf        CHAIN     mzaME000                           21
013500011129     C                   MOVE      AMETGA        VTGA
013600011129     C                   MOVEL     AMETGN        VTGN
013700011129     C                   MOVE      AMETLA        VTLA
013800160517     C*                  MOVE      AMEMAR        VMAR
013900160517     C*                  MOVE      AMETIP        VTIP
014000160517     c                   clear                   vmaud
014100160517     C                   MOVE      AMEmau        Vmau
014200160517     c                   movel     'MAU'         tbecod
014300160517     c                   movel(p)  vmau          tbeke1
014400160517     c     ktbe          chain     tntbe01l
014500160517     c                   if        %found(tntbe01l)
014600160517     c                   movel     tbeuni        dmau
014700160517     c                   movel     §maudes       vmaud
014800160517     c                   endif
014900160517     C                   MOVE      AMEcaut       Vcaut
015000160517     c                   movel     'CAU'         tbecod
015100160517     c                   movel(p)  vcaut         tbeke1
015200160517     c     ktbe          chain     tntbe01l
015300160517     c                   if        %found(tntbe01l)
015400160517     c                   movel     tbeuni        dcau
015500160517     c                   movel     §caudes       vcautd
015600160517     c                   endif
015700031029     c                   clear                   telev
015800031029     c                   if        ametel <> *blanks
015900040213     C                   MOVEl     AMEtel        telev
016000031029     c                   end
016100011129     C                   MOVE      *BLANK        VDS2
016200011129     C   49              MOVEL     'VENDITA'     VDS1
016300011129     C   50              MOVEL     SK(1)         VDS1
016400011129     C   51              MOVEL     SK(2)         VDS1
016500011129     C   52              MOVEL     SK(3)         VDS1
016600011129     C   53              MOVEL     SK(4)         VDS1
016700021021     C   54              MOVEL     SK(5)         VDS1
016800030113     C   55              MOVEL     SK(6)         VDS1
016900031028     C   56              MOVEL     SK(7)         VDS1
017000011129     C   50              MOVE      *BLANKs       VTNA
017100011129     C   50              MOVE      *blanks       VTNN
017200011129     C   51              MOVE      *BLANKS       VCAU
017300011129     C   51              MOVE      *BLANKS       Vnom
017400011129     C   52              MOVE      *blanks       VFIL
017500011129     C   52              MOVE      *blanks       Vdes
017600011129     C   53              MOVE      *blanks       VCSP
017700011129     C   53              MOVE      *blanks       VdSP
017800020607     C   54              MOVE      *blanks       Vsoc
017900020607     C   54              MOVE      *blanks       Vdsoc
018000030113     C   55              MOVE      *blanks       Vcdc
018100030113     C   55              MOVE      *blanks       Vdcdc
018200031028     C   56              clear                   telev
018300011129     c                   enddo
018400011129     c                   endsr
018500011129     C*----------------------------------------------------*
018600000000     C*  CAMBIO TARGA
018700000000     C*----------------------------------------------------*
018800000000     C     CAMBIO        BEGSR
018900910204     C   50              MOVE      VTNA          AMETGA
019000910204     C   50              MOVEL     VTNN          AMETGN
019100910201     C   51              MOVE      VCAU          AMECAU
019200011129     c   51              move      datain        dtaeur
019300011129     c   51              move      dtaeur        dtaiso
019400040212     c   51              adddur    1:*d          dtaiso
019500011129     C   51              MOVE      Dtaiso        AMEDAL
019600910201     C   53              MOVE      VCSP          AMECSP
019700910201     C   52              MOVE      VFIL          AMEFIL
019800020607     C   54              MOVEl     Vsoc          AMEsoc
019900030113     C   55              MOVEl     Vcdc          AMEcdc
020000031029     C   56              MOVEl     telev         AMEtel
020100011129     C                   UPDATE    mzAME000
020200011129      * SCRIVE RECORD DI mzAMH00F
020300910507     C                   Z-ADD     VNRF          AMHNRF
020400910507     C                   MOVE      VTGA          AMHTGA
020500910507     C                   MOVEL     VTGN          AMHTGN
020600910507     C                   MOVE      VTLA          AMHTLA
020700910507     C                   MOVE      VDS2          AMHDS2
020800910507     C                   MOVE      VDS1          AMHDS1
020900911002     C   50              MOVE      VTNA          AMHTNA
021000911002     C   50              MOVE      VTNN          AMHTNN
021100011129     c                   move      datain        dtaeur
021200011129     c                   move      dtaeur        dtaiso
021300011129     C                   MOVE      Dtaiso        AMHdta
021400910507   N C                   MOVE      PARCAU        AMHCAU
021500011129     c                   move      pardat        dtaeur
021600011129     c                   move      dtaeur        dtaiso
021700011129     C                   MOVE      Dtaiso        AMHdal
021800910507   N C                   MOVE      PARFIL        AMHFIL
021900910507   N C                   MOVE      PARCSP        AMHCSP
022000020607   N C                   MOVEl     PARsoc        AMHsoc
022100030113   N C                   MOVEl     PARcdc        AMHcdc
022200910507     C                   MOVE      VSCL          AMHTIP
022300011129     c                   z-add     0             amhal
022400031029     c                   movel     partel        amhtel
022500011129     C                   WRITE     mzAMH000
022600910507     C*
022700910204     C   50              MOVE      VTNA          PARTNA
022800910204     C   50              MOVEL     VTNN          PARTNN
022900910204     C   51              MOVEL     VCAU          PARCAU
023000040212     c   51              move      datain        dtaeur
023100040212     c   51              adddur    1:*d          dtaeur
023200040212     C   51              MOVE      Dtaeur        pardat
023300910204     C   53              MOVEL     VCSP          PARCSP
023400910204     C   52              MOVEL     VFIL          PARFIL
023500020607     C   54              MOVEL     Vsoc          PARsoc
023600030113     C   55              MOVEL     Vcdc          PARcdc
023700031029     C   56              z-add     telev         PARtel
023800000000     C*
023900000000     C                   ENDSR
024000011129     C*----------------------------------------------------*
024100000000     C*  ANNULLO AUTOMEZZO
024200000000     C*----------------------------------------------------*
024300000000     C     ANNULL        BEGSR
024400011129     C                   DELETE    mzAME000
024500000000      *
024600011129      * SCRIVE RECORD DI mzAMH00F
024700910524      *
024800910524     C                   Z-ADD     VNRF          AMHNRF
024900910524     C                   MOVE      VTGA          AMHTGA
025000910524     C                   MOVEL     VTGN          AMHTGN
025100910524     C                   MOVE      VTLA          AMHTLA
025200910524     C                   MOVE      VDS2          AMHDS2
025300910524     C                   MOVE      VDS1          AMHDS1
025400011129     c                   move      datain        dtaeur
025500011129     c                   move      dtaeur        dtaiso
025600011129     C                   MOVE      Dtaiso        AMHdta
025700910524   N C                   MOVE      PARCAU        AMHCAU
025800011129     c                   move      pardat        dtaeur
025900011129     c                   move      dtaeur        dtaiso
026000011129     C                   MOVE      Dtaiso        AMHdal
026100910524   N C                   MOVE      PARFIL        AMHFIL
026200020607   N C                   MOVEl     PARsoc        AMHsoc
026300910524   N C                   MOVE      PARCSP        AMHCSP
026400910524     C                   MOVE      VSCL          AMHTIP
026500011129     c                   z-add     0             amhal
026600030115     C                   MOVEl     amecat        AMHcat
026700030115   N C                   MOVEl     PARCdc        AMHCdc
026800031028   N C                   MOVEl     PARtel        AMHtel
026900011129     C                   WRITE     mzAMH000
027000040212     C*                  Z-ADD     *DATE         DATAIN
027100000000     C                   ENDSR
027200011129     C*----------------------------------------------------*
027300011129     C*  ricerca
027400000000     C*----------------------------------------------------*
027500031028     C     ric00         BEGSR
027600011129     c                   movel     '1'           fld
027700011129     c                   select
027800031028     C                   when      §fld = 'VSCL'
027900031028     c                   exsr      srvscl
028000031028     c                   endsl
028100031028     C* POSIZIONAMENTO CURSORE
028200031028     C                   Z-ADD     §RIGA         §RIG
028300031028     C                   Z-ADD     §COLON        §COL
028400031028     c                   endsr
028500031028     C*----------------------------------------------------*
028600031028     C*  ricerca
028700031028     C*----------------------------------------------------*
028800031028     C     ric01         BEGSR
028900031028     c                   movel     '1'           fld
029000031028     c                   select
029100031028     C                   when      §fld = 'VCAU' and *in51
029200031028     c                   exsr      srvcau
029300011129     C                   when      §fld = 'VCSP' and *in53
029400011129     c                   exsr      srvcsp
029500011129     C*CODICE FILIALE
029600011129     C                   when      §fld = 'VFIL' and *in52
029700011129     C                   MOVEL     *BLANKS       §COD1
029800011129     C                   MOVE      *BLANKS       §DES1
029900011129     C                   CALL      'TNSD19R'
030000011129     C                   PARM                    §COD1             3
030100011129     C                   PARM                    §TIP              1
030200011129     C                   PARM                    §DES1            25
030300011129     C                   MOVEL     §COD1         VFIL                           *CODICE
030400011129     C                   MOVEL     §DES1         Vdes                           *DESCRIZIONE
030500011221     C                   WHEN      §FLD = 'VTNN' or §fld = 'VTNA'
030600011221     C                   CALL      'TRMZ02R4'    RICTAR
030700020607     C*CODICE societa in dotazione
030800020607     C                   when      §fld = 'VSOC' and *in54
030900020607     C                   reset                   ana007ds
031000020607     C                   eval      opz007 = '01'
031100020607     C                   eval      kpjbu = ana007ds
031200020607     C                   call      'ANA007R'
031300020607     C                   parm                    kpjba
031400020607     C                   eval      ana007ds = kpjbu
031500020607     C                   if        err007 <> '1' and soc007 <> *blanks
031600020607     C                   eval      vsoc = soc007
031700020607     c                   exsr      srvsoc
031800020607     c                   end
031900030113     C*CODICE CDC
032000030113     C                   when      §fld = 'VCDC' and *in55
032100030113    >C                   RESET                   ANA032DS
032200030113    >C                   MOVEL     '01'          OPZ032
032300030113     C                   MOVE      'S'           FSC032
032400030113     C                   MOVE      'CDC'         TPE032
032500030113    >C                   MOVEL     ANA032Ds      KPJBU
032600030113    >C                   CALL      'ANA032R'
032700030113     C                   PARM                    KPJBA
032800030113    >C                   MOVEL     KPJBU         ANA032DS
032900030113     c                   if        ent032 <> *blanks
033000030113     C                   MOVEL     ENT032        VCDC
033100030113     c                   end
033200011129     c                   endsl
033300011129     C* POSIZIONAMENTO CURSORE
033400011129     C                   Z-ADD     §RIGA         §RIG
033500011129     C                   Z-ADD     §COLON        §COL
033600011129     C                   ENDSR
033700011129     C*----------------------------------------------------*
033800011129     C*  CONTROLLI IMMISSIONE
033900011129     C*----------------------------------------------------*
034000011129     C     CTRLP1        BEGSR
034100011129     c                   movel     *blanks       fld               1
034200011129     c                   setoff                                       99
034300011129     c                   select
034400011129      * CONTROLLO TARGA GIA ESISTENTE
034500011129     c                   when      *in50
034600150114     c                   if        vtga = vtna and
034700150114     c                             vtgn = vtnn
034800141126     c                   eval      $msg = 'La targa non è stata variata'
034900141126     c                   seton                                        2899
035000141126     c                   leavesr
035100141126     c                   endif
035200011129     C     KTAR          CHAIN     mzAME002                           21
035300011129     C  N21              SETON                                        7399
035400011129     C* autista
035500011129     c                   when      *in51
035600141126     c                   if        vcau = amecau
035700141126     c                   eval      $msg = 'Autista non variato '
035800141126     c                   seton                                        2899
035900141126     c                   leavesr
036000141126     c                   endif
036100011129     C                   EXSR      SRVCAU
036200011129     C* filiale
036300011129     c                   when      *in52
036400141126     c                   if        vfil <> *blank
036500141126     c                   move      vfil          vfiln             3 0
036600141126     c                   if        vfiln = amefil
036700141126     c                   eval      $msg = 'Filiale non variata '
036800141126     c                   seton                                        2899
036900141126     c                   leavesr
037000141126     c                   endif
037100141126     c                   endif
037200011129     C                   EXSR      SRVfil
037300011129     C* proprietà
037400011129     c                   when      *in53
037500141126     c                   if        vcsp = amecsp
037600141126     c                   eval      $msg = 'Proprietà non variata '
037700141126     c                   seton                                        2899
037800141126     c                   leavesr
037900141126     c                   endif
038000011129     C                   EXSR      SRVCsp
038100020607     C* società in dotazione
038200020607     c                   when      *in54
038300141126     c                   if        vsoc = amesoc
038400141126     c                   eval      $msg = 'Società in dotazione non variata '
038500141126     c                   seton                                        2899
038600141126     c                   leavesr
038700141126     c                   endif
038800020607     C                   EXSR      SRVsoc
038900030115     C* centro di costo
039000030115     c                   when      *in55
039100141126     c                   if        vcdc = amecdc
039200141126     c                   eval      $msg = 'Centro di costo non variato'
039300141126     c                   seton                                        2899
039400141126     c                   leavesr
039500141126     c                   endif
039600030115     C                   EXSR      SRVcdc
039700031028     C* telepass
039800031028     c                   when      *in56
039900141126     c                   if        telev > 0
040000141126     c                   movel     telev         televa           20
040100141126     c                   if        televa = ametel
040200141126     c                   eval      $msg = 'Tessera Telepass non variata'
040300141126     c                   seton                                        2899
040400141126     c                   leavesr
040500141126     c                   endif
040600141126     c                   endif
040700031028     C                   EXSR      SRVtel
040800011129     c                   endsl
040900910204     C*
041000000000     C                   ENDSR
041100011129     C**********************************************************************
041200011129     C** Controlli e decodifiche campi con tabel00f
041300011129     C**********************************************************************
041400011129     C     srtab         BEGSR
041500011129     c                   if        fld <> *blanks
041600011129     c                   movel     parkey        parkeys           8
041700011129     C                   CALL      'TRUL19R'
041800011129     c                   parm                    parcod            2
041900011129     c                   parm      '1'           parord            1
042000011129     c                   parm      *blanks       parkey            8
042100011129     c                   parm      *blanks       cmd               1
042200011129     c* ritorno senza scelta
042300011129     c                   if        cmd = 'L'
042400011129     c                   movel     parkeys       parkey
042500011129     C                   END
042600011129     C                   END
042700011129     C                   MOVEL     *blanks       tbluni
042800011129     C                   MOVEL     parcod        COD
042900011129     C                   MOVEL(p)  parkey        KEY
043000011129     C     KTAB          CHAIN     TABEL                              99
043100011129     c*
043200011129     C                   ENDSR
043300011129     c**********************************************************************
043400011129     c* pulisce 1° formato
043500011129     c**********************************************************************
043600011129     C     clr00         BEGSR
043700011129     C                   Z-ADD     0             §RIGA
043800011129     C                   Z-ADD     0             §COLON
043900011129     C                   CLEAR                   §FLD
044000011129     C                   ENDSR
044100011129     c**********************************************************************
044200031028     C** variazione
044300011129     c**********************************************************************
044400031028     C     srvscl        BEGSR
044500031028     c                   setoff                                       41
044600031028     c                   movel     'XS'          parcod
044700031028     c                   movel(P)  Vscl          parkey
044800011129     c                   exsr      srtab
044900031028     c                   movel     parkey        vscl
045000011129     c                   if        *in99
045100031028     C                   MOVEL     *blanks       dcambiov
045200031028     C                   SETON                                        41
045300011129     C                   else
045400031028     C                   MOVEL     TBLUNI        dcambiov
045500011129     C                   END
045600011129     c*
045700011129     C                   ENDSR
045800031028     c**********************************************************************
045900031028     C** autista
046000031028     c**********************************************************************
046100031028     C     srvcau        BEGSR
046200031028     c                   movel     'XN'          parcod
046300031028     c                   movel(P)  VCAU          parkey
046400141126     c                   if        vcau = amecau
046500141126     c                   seton                                        9974
046600141126     c                   leavesr
046700141126     c                   endif
046800031028     c                   exsr      srtab
046900031028     c                   movel     parkey        vcau
047000031028     c                   if        *in99
047100031028     C                   MOVEL     *blanks       Vnom
047200031028     C                   SETON                                        74
047300031028     C                   else
047400031028     C                   MOVEL     TBLUNI        Vnom
047500031028     C                   END
047600031028     c*
047700031028     C                   ENDSR
047800011129     c**********************************************************************
047900011129     C** proprietà
048000011129     c**********************************************************************
048100011129     C     srvcsp        BEGSR
048200011129     c                   movel     'XR'          parcod
048300011129     c                   movel(P)  VCsp          parkey
048400011129     c                   exsr      srtab
048500011129     c                   movel     parkey        vcsp
048600011129     c                   if        *in99
048700011129     C                   MOVEL     *blanks       VDsp
048800011129     C                   SETON                                        75
048900011129     C                   else
049000011129     C                   MOVEL     TBLUNI        VDsp
049100011129     C                   END
049200011129     c*
049300011129     C                   ENDSR
049400020607     c**********************************************************************
049500020607     C** società in dotazione
049600020607     c**********************************************************************
049700020607     C     srvsoc        BEGSR
049800020607     c*
049900020607     C     vsoc          CHAIN     ansif01L
050000020607     C                   if        not %found(ansif01l)
050100030115     C                   SETON                                        7799
050200020607     c                   movel     *blanks       vdsoc
050300020607     c                   else
050400020607     c                   movel     sifdesbrev    vdsoc
050500020607     C                   END
050600020607     c*
050700020607     C                   ENDSR
050800011129     c**********************************************************************
050900011129     C** FILIALE
051000011129     c**********************************************************************
051100011129     C     srvfil        BEGSR
051200011129     c*
051300011129     C                   MOVE      VFIL          FILV              3 0
051400011129     C     FILV          CHAIN     AZORG
051500011129     C                   if        not %found(azorg01l)
051600011129     C                   MOVEL     *blanks       VDes
051700011129     C                   SETON                                        7699
051800011129     c                   else
051900011129     C                   MOVEL     ORGDES        VDes
052000011129     C                   END
052100011129     c*
052200011129     C                   ENDSR
052300030113     c**********************************************************************
052400030113     C** centro di costo
052500030113     c**********************************************************************
052600030113     C     srvcdc        BEGSR
052700030113     c*
052800030113     C                   movel     *blanks       VDCDC
052900030113     C                   movel     VCDC          ententita
053000030113     C     kent          chain     anent01l
053100030113     C                   IF        NOT %FOUND(ANENT01L)
053200030113     C                   SETON                                        7899
053300030113     C                   ELSE
053400030113     C                   move      entdesbrev    VDCDC
053500030113     C                   END
053600030113     c*
053700030113     C                   ENDSR
053800031028     c**********************************************************************
053900031028     C** telepass
054000031028     c**********************************************************************
054100031028     C     srvtel        BEGSR
054200031028     c*
054300040213     c                   if        telev <> 9999999999
054400031029     c                   movel     telev         com20
054500031028     C     com20         setll     mzame05l                               79
054600031028     C                   IF        *in79
054700031028     C                   SETON                                        99
054800031028     C                   END
054900031106     C                   END
055000031028     c*
055100031028     C                   ENDSR
055200011129     C*---------                                           *
055300011129      * CHIAVI                                             *
055400011129     C*---------                                           *
055500011129     c     *inzsr        begsr
055600011129     C* POSIZIONAMENTO X N.DI TARGA                        *
055700011129     C     KTAR          KLIST                                                  *
055800011129     C                   KFLD                    VTNA                           *
055900011129     C                   KFLD                    VTNN                           *
056000011129     C     KTAB          KLIST                                                  *
056100011129     C                   KFLD                    CODUT                          *
056200011129     C                   KFLD                    COD               2            *
056300011129     C                   KFLD                    KEY               8            *
056400160517     C     KTbe          KLIST                                                  *
056500160517     C                   KFLD                    tbecod                         *
056600160517     C                   KFLD                    tbeke1                         *
056700030113     C* POSIZIONAMENTO SU ANENT                            *
056800030113     C     Kent          KLIST
056900030113     C                   KFLD                    xscsoc
057000030113     C                   KFLD                    enttprifan
057100030113     C                   KFLD                    ententita
057200030113     C                   move      'CDC'         enttprifan
057300011129     C* RICERCA TARGA X CITTA'
057400011129     C     RICTAR        PLIST
057500011129     C                   PARM                    CODUT
057600011129     C                   PARM                    RSUT
057700011129     C                   PARM                    vtna
057800011129     C                   PARM                    vtnn
057900011129      *                                                    *
058000011129     C     *ENTRY        PLIST                                                  *
058100011129     C                   PARM                    KPJBA                          *
058200011129     C                   Z-ADD     1             CODUT                          *
058300011129     C                   MOVEL     KPJBU         PARAM
058400011129      *
058500011129     C                   CALL      'XPARUT'                                     *
058600011129     C                   PARM                    PARUT                          *
058700011129     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
058800011129     C     CODUT         ifeq      -1                                           *
058900011129     c                   seton                                        lr
059000011129     c                   end
059100011129     C                   MOVEL     RAGUT         RSUT             20            *
059200030113      * Reperimento dati società
059300030113     C                   clear                   SocXsc
059400030113     C                   eval      tipxsc = 'SOC001'
059500030113     C                   exsr      rtvsocieta
059600030113     C                   if        rtnxsc = '1'
059700030113     C                   seton                                        lr
059800030113     C                   else
059900030113     C                   MOVEL     XSOCDS        xsoc001ds
060000030113     C                   endif
060100011129      *
060200011129     c                   endsr
060300030113     C*---------------------------------------------------------------------
060400030113     c*  Reperisco società.
060500030113     C*---------------------------------------------------------------------
060600030113     C     rtvsocieta    begsr
060700030113     C*
060800030113     C                   CALLb     'XSOC'
060900030113   >>C                   Parm                    TipXsc            6
061000030113   >>C                   Parm                    SocXsc            3
061100030113   >>C                   Parm                    CdsXsc            9 0
061200030113   >>C                   Parm                    ModXsc            3
061300030113   >>C                   Parm      *BLANK        RtnXsc            1
061400030113     C                   Parm                    XsocDS
061500030113   >>C                   Parm                    Kpjba
061600030113     C*
061700030113     C                   endsr
061800011129**
061900011129CAMBIO TARGA
062000011129CAMBIO AUTISTA
062100011129CAMBIO FILIALE
062200011129CAMBIO PROPRIETA'
062300021021CAMBIO SOC.DOTAZIONE
062400030113CAMBIO CDC
062500031028CAMBIO TELEPASS
