000001031128     H DFTACTGRP(*NO) BNDDIR('PJXBND') ACTGRP('QILE')
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011121     F*----------------------------------------------------*
000400031128     F*   Esplosione storico automezzi                     *
000500000000     F*----------------------------------------------------*
000600000000     FTABEL00F  IF   E           K DISK
000700011123     Fansif01L  IF   E           K DISK
001000011122     Fanent01l  IF   E           K DISK
001100910215     FAZORG01L  IF   E           K DISK
001200031128     FmzAMh00f  iF   E             DISK
002100031128     Ftrmz02D6  CF   E             WORKSTN
002301031128    >D xsoc001ds     E DS
002307031128     D* DS Interna per dati di output di XSOC
002308031128     D XSOCDS          DS          1000
005200011220     D dsxa          E DS
005201031128     D dsxs          E DS
005600000000     D KPJBA         E DS
005900000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
005901031128     D  dataiso        s               d
005902031128     D  dataeur        s               d   datfmt(*eur)
005903031128     D  nrr            s              9s 0
005904031128     D  parkey         s                   like(tblkey)
011100000000     C*----------------------------------------------------*
011101031128     c                   exsr      leggi
011400011121     C                   do        *hival
012200031128     C                   EXFMT     GA02D002
013200011122     C   Kc              leave
013800011122     c                   enddo
013900000000     C                   SETON                                        LR
024500011123     C**********************************************************************
024600011123     C** LETTURA X CHIAVE N.RIFERIMENTO
024700011123     C**********************************************************************
024800011123     C     LEGGI         BEGSR
025700031128     c     nrr           chain     mzamh00f
025701031128     C                   Z-ADD     AMhNRF        VNRF
025702031128     c                   if        amhdta <> 0
025703031128     C                   MOVE      AMhDta        dataiso
025704031128     C                   move      dataiso       DATAeur
025705031128     C                   move      DATAeur       VDUR
025706031128     c                   else
025707031128     c                   z-add     0             vdur
025708031128     c                   end
025709031128     c                   movel     amhtip        vtip
025710031128     c                   exsr      srvtip
031100031128     C                   MOVEL     AMhds1        Vds1
031101031128     C                   MOVEL     AMhds2        Vds2
031103031128     C* AUTISTA AUTOMEZZO
031104031128     C                   MOVEL     AMhCAU        VCAU
031105031128     C                   EXSR      SRVCAU
031106031128     c                   if        amhdal <> 0
031107031128     C                   move      AMhDAL        DATAIso
031108031128     C                   move      dataiso       dataeur
031109031128     C                   move      DATAeur       dataut
031110031128     c                   else
031111031128     c                   z-add     0             dataut
031112031128     C                   END
031114031128     c* x cambio targa inverto i campi della targa
031115031128     c                   if        amhtnn <> ' '
032000031128     C                   MOVEL     AMhTna        VTGA
032100031128     C                   MOVEL     AMhTnn        VTGN
032101031128     C                   MOVEL     AMhTGA        VTGAo
032102031128     C                   MOVEL     AMhTGN        VTGNo
032103031128     c                   else
032104031128     C                   MOVEL     AMhTGA        VTGA
032105031128     C                   MOVEL     AMhTGN        VTGN
032106031128     C                   clear                   VTGAo
032107031128     C                   clear                   VTGNo
032108031128     c                   end
032110031128     C*CATEGORIA AUTOMEZZO
032200031128     C                   MOVEL     AMhCAT        VCAT
032500011126     c                   exsr      srvCAT
037101031128     C                   MOVEL     AMhTLA        VTLA
037102031128     C                   MOVEl     AMhtel        telev
038800031128     C* FILIALE IN DOTAZIONE
038801031128     C                   MOVEL     AMhFIL        VFIL
038900011126     C                   EXSR      SRVFIL
038901031128     C* società in dotazione
038902031128     C                   movel     AMhsoc        Vsoc
038903031128     c                   exsr      srvsoc
039100031128     C* CDC
039101031128     C                   MOVEL     AMhCDC        VCDC
039200011126     C                   EXSR      SRVCDC
041900031128     C* TIPO PROPRIETA'
042000031128     C                   MOVEL     AMhCSP        Vpro
042100031128     C                   EXSR      SRVpro
050600011123     C                   ENDSR
050601031128     c**********************************************************************
050602031128     C** tipo variazione
050603031128     c**********************************************************************
050604031128     C     srvtip        BEGSR
050605031128     c*
050606031128     c                   movel     'XS'          parcod            2
050607031128     c                   movel(P)  Vtip          parkey
050608031128     c                   exsr      srtab
050609031128     c                   movel     parkey        vtip
050610031128     c                   if        *in99
050611031128     C                   MOVEL     *blanks       dsxs
050612031128     C                   MOVEL     *blanks       VDtip
050613031128     C                   else
050614031128     C                   MOVEL     TBLUNI        dsxs
050615031128     C                   MOVEL     §xsdes        VDtip
050616031128     C                   END
050617031128     c*
050618031128     C                   ENDSR
110900011126     c**********************************************************************
111000011126     C** FILIALE
111100011126     c**********************************************************************
111200011126     C     srvfil        BEGSR
111300011126     c*
111400011126     C                   MOVE      VFIL          FILV              3 0
111500011126     C     FILV          CHAIN     AZORG
111600011126     C                   if        not %found(azorg01l)
111700031128     C                   clear                   vdfil
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
114500031128     C                   IF        %FOUND(ANENT01L)
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
118200011126     C                   else
118300011126     C                   MOVEL     TBLUNI        VDAU
118400011126     C                   END
118500011126     c*
118600011126     C                   ENDSR
122300011126     c**********************************************************************
122400011126     C** tipo proprietà
122500011126     c**********************************************************************
122600031128     C     srvpro        BEGSR
122700011126     c*
122800011126     c                   movel     'XR'          parcod
122900031128     c                   movel(P)  Vpro          parkey
123000011126     c                   exsr      srtab
123100031128     c                   movel     parkey        vpro
123200011126     c                   if        *in99
123300031128     C                   MOVEL     *blanks       Vdpro
123500011126     C                   else
123600031128     C                   MOVEL     TBLUNI        VDpro
123700011126     C                   END
123800011126     c*
123900011126     C                   ENDSR
134400011123     C**********************************************************************
134500011123     C** Controlli e decodifiche campi con tabel00f
134600011123     C**********************************************************************
134700011123     C     srtab         BEGSR
136200011123     C                   MOVEL     *blanks       tbluni
136300011123     C                   MOVEL     parcod        COD
136400011123     C                   MOVEL(p)  parkey        KEY
136500011123     C     KTAB          CHAIN     TABEL                              99
136600011123     c*
136700011123     C                   ENDSR
159100011121     C*---------------------------------------------------------------*
159200011121     C     *inzsr        BEGSR
159300011121     C*---------------------------------------------------------------*
159400011121     C     *ENTRY        PLIST
159500011121     C                   PARM                    KPJBA
159501031128     c                   movel     kpjbu         nrr
159700011121     C                   Z-ADD     1             CODUT
159800011121     C                   CALL      'XPARUT'
159900011121     C                   PARM                    UTEDSE
160000011122     C     CODUT         ifeq      -1
160100011122     c                   seton                                        lr
160200031128     c                   return
160300011122     c                   end
160400011121     C                   MOVEL     RAGUT         RSUT             20
160401031128      * Reperimento dati società
160402031128     C                   clear                   SocXsc
160403031128     C                   eval      tipxsc = 'SOC001'
160404031128     C                   exsr      rtvsocieta
160405031128     C                   if        rtnxsc = '1'
160406031128     C                   seton                                        lr
160407031128     c                   return
160408031128     C                   else
160409031128     C                   MOVEL     XSOCDS        xsoc001ds
160410031128     C                   endif
161900011121     C* POSIZIONAMENTO TABELLE                             *
162000011121     C     KTAB          KLIST                                                  *
162100011121     C                   KFLD                    CODUT                          *
162200011121     C                   KFLD                    COD               2            *
162300011121     C                   KFLD                    KEY               8            *
162301031128     C* POSIZIONAMENTO SU ANENT                            *
162302031128     C     Kent          KLIST
162303031128     C                   KFLD                    xscsoc
162304031128     C                   KFLD                    enttprifan
162305031128     C                   KFLD                    ententita
162306031128     C                   move      'CDC'         enttprifan
165000011122     c*
165100011121     C                   ENDSR
165200031128     C*---------------------------------------------------------------------
165300031128     c*  Reperisco società.
165400031128     C*---------------------------------------------------------------------
165500031128     C     rtvsocieta    begsr
165600031128     C*
165700031128     C                   CALLb     'XSOC'
165800031128   >>C                   Parm                    TipXsc            6
165900031128   >>C                   Parm                    SocXsc            3
166000031128   >>C                   Parm                    CdsXsc            9 0
166100031128   >>C                   Parm                    ModXsc            3
166200031128   >>C                   Parm      *BLANK        RtnXsc            1
166300031128     C                   Parm                    XsocDS
166400031128   >>C                   Parm                    Kpjba
166500031128     C*
166600031128     C                   endsr
