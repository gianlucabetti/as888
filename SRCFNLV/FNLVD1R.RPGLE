000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200160722     H* FNLVD1R *----------------------------------------------------*
000300160722     H* - Driver di Verifica instradabilità da EEX a DPD
000301110610     h*
000302160822     h* Il pgm non ricenve in input la kpjba. Se dovesse diventare
000303160822     h* necessario averla sarà da impostare in questo pgm in quanto il
000304170505     h* chiamante (di Fabrizio) lavora senza
000400000000     H*--------------------------------------------------------------*
000500160722     Ftabel00f  IF   E           K DISK
000501160722     Ftntbe01l  IF   E           K DISK
001200030722     F**
001600070313     d
001700140205      * ds parametri input/output
001800160722     D fnlvd1ds      E DS
001801160722     D tisi95ds      E DS
001802180108     d trulflbds     e ds
001803160725
001804160725     D kpjba         E DS
001805160722
001806160725     D* DS PER TISIE3R - CAPPARIO DPD  - NUOVO
001807160725     D TISIe3ds      E DS
001808160725
001809160722     d tisi97ds      e ds
001810160725     d dcppflo       e ds
001811160722     d fnlvdpds      e ds
001812160722     d TRULVPODS     e ds
001813160722     d   sk_VPODPD            16    765    inz   dim(250)
001814160722     d ds3idp        e ds
001815160722     d ds15          e ds
001816160722     d ds16          e ds
001817160722     d codut           s              1  0 inz(1)
001818160722     d cod             s                   like(tblcod)
001819160722     d key             s                   like(tblkey)
001820170505     d*w_lad           s                   like(elvd1lnad)
001821160725     d w_dsp           s                   like(i95dat)
001822160725     d dataiso_cor     s               d   datfmt(*iso)
002000070313     d
002004140206
003800900518     C***************************************************************
003900000000     C     *ENTRY        PLIST
004000160722     C                   PARM                    fnlvd1ds
004001160722     C                   PARM                    tisi95ds
004300940223     C*---------------------------------------------------------------*
004400160722     C* TIPO LANCIO, ilvd1TLA "C" -           CHIUSO CON LR
004500160722     C* TIPO LANCIO, ilvd1TLA "L" - ELABORO E CHIUDO CON LR
004600160722     C* TIPO LANCIO, ilvd1TLA " " - ELABORO E CHIUDO IN RETRN
004700941216     C*
004800160722    1C     ilvd1TLA      IFNE      'C'
004900160722     c                   eval      OLVd1ESI='S'
005000160722     c                   clear                   OLVd1MAXP
005001160722     c                   clear                   OLVd1MAXV
005002170505     c                   clear                   OLVd1ZNCD
005003160722     c                   clear                   OLVd1msg
005400070412     c
005401160727     c* controllo e reperimento parametri
005402160725     c                   exsr      sr_parm
005403160727     c                   if        olvd1esi='S'
005422160725     c* Solo se non richiesto il SOLO controllo con cappario dpd (TISIE3R)
005423160725     c                   if        ILVD1TPE=*blanks or ilvd1tpe='N'
005424160722     c                   exsr      sr_elabora
005429160725     c                   endif
005430160725
005431160725     c                   if        OLVD1ESI='S'
005432160725     c* Controllo cap su cappario DPD, se richiesto
005502160725     c                   if        ILVD1TPE=*blanks or ilvd1tpe='S'
005503160725     c                   exsr      sr_tisie3
005504160725     c                   endif
005505160725
005506160725     c                   endif
005507160727     c                   endif
005508170505     c*
005509170505     c                   if        olvd1esi='S'
005510170505     C* PER DPD IMPOSTO ZONA 00 PER RESI ZONA 80
005511170505     C     ilvd1fbr      IFEQ      'S'
005512170505     C                   MOVEL     80            olvd1zncd
005513170505     C                   ELSE
005514170505     C                   MOVEL     00            olvd1zncd
005515170505     C                   ENDIF
005517170505     c                   endif
005518160725
012001140205    1c                   endif
012100051123     c
012200160722    1C     Ilvd1tla      IFNE      ' '
012201160725     c                   exsr      sr_chiudi
012400051123     C                   SETON                                        LR
012500051123     C                   ELSE
012600051123     C                   SETON                                        RT
012700140205    1C                   ENDIF
012701160725     C**************************************************************************
012702160725     C     sr_parm       BEGSR
012703160725     C**************************************************************************
012704160727     c* Verifico la presenza dei campi obbligatori
012705160727     c                   select
012706160727     c* lna
012707160727     c                   when      o95lna = 0
012708160727     c                   eval      OLVd1ESI='E'
012709160727     c                   eval      olvd1msg='Manca parametro o95lna        '
012710160727     c* località
012711160727     c                   when      i95loc = *blanks
012712160727     c                   eval      OLVd1ESI='E'
012713160727     c                   eval      olvd1msg='Manca parametro i95loc        '
012714160727     c* cap
012715160727     c                   when      i95cap = *blanks
012716160727     c                   eval      OLVd1ESI='E'
012717160727     c                   eval      olvd1msg='Manca parametro i95cap        '
012718160727     c* nazione
012719160727     c                   when      i95nar = *blanks
012720160727     c                   eval      OLVd1ESI='E'
012721160727     c                   eval      olvd1msg='Manca parametro i95nar        '
012722160727     c* Porto
012723160727     c                   when      i95tpo = *blanks
012724160727     c                   eval      OLVd1ESI='E'
012725160727     c                   eval      olvd1msg='Manca parametro i95tpo        '
012726160727     c* lnp
012727160727     c                   when      ilvd1lnp = 0
012728160727     c                   eval      OLVd1ESI='E'
012729160727     c                   eval      olvd1msg='Manca parametro ilvd1lnp      '
012730160727     c* numero colli
012731160727     c                   when      ilvd1ncl = 0
012732160727     c                   eval      OLVd1ESI='E'
012733160727     c                   eval      olvd1msg='Manca parametro ilvd1ncl      '
012734160727     c* peso
012735160727     c                   when      ilvd1pkg = 0
012736160727     c                   eval      OLVd1ESI='E'
012737160727     c                   eval      olvd1msg='Manca parametro ilvd1pkg      '
012738160727     c* volume
012739170503     c*                  when      ilvd1vlu = 0
012740170503     c*                  eval      OLVd1ESI='E'
012741170503     c*                  eval      olvd1msg='Manca parametro ilvd1vlu      '
012742160727     c* codice cliente mittente
012743160727     c                   when      ilvd1ccm = 0
012744160727     c                   eval      OLVd1ESI='E'
012745160727     c                   eval      olvd1msg='Manca parametro ilvd1ccm      '
012746160727     c* Se orm non fittizio, Network del po emittente
012747160727     c                   when      ilvd1ormf=*blanks and ilvd1npoe=*blanks
012748160727     c                   eval      OLVd1ESI='E'
012749160727     c                   eval      olvd1msg='Manca parametro ilvd1npoe     '
012750160727     c                   endsl
012751160727
012752160727     c                   if        olvd1esi='E'
012753160727     c                   leavesr
012754160727     c                   endif
012755160727
012756160725     c* Linea di arrivo DPD: se non passata la reperisco da tabella Nazioni
012757170505     c***                clear                   w_lad
012758170505     c                   if        ELVD1LNAD=0
012759160725     c                   movel     '15'          cod
012760160725     C                   MOVEL(P)  i95nar        KEY
012761160725     c                   clear                   ds15
012762160725     c     ktab          chain     tabel00f
012763160725     c                   if        %found(tabel00f) and tblflg=' '
012764160725     c                   eval      ds15=tbluni
012765170505     c****               z-add     §15lad        w_lad
012766170505     c                   z-add     §15lad        ELVD1LNAD
012767160725     c                   endif
012768160725     c                   else
012769170505     c*****              z-add     ILVD1LNAD     w_lad
012770160725     c                   endif
012771160725     c* Data spedizione: se non passata assume data odierna
012772160725     c                   clear                   w_dsp
012773160725     c                   if        i95dat=0
012774160725     c                   eval      dataiso_cor=(%date())
012775160725     c                   eval      w_dsp=%dec(dataiso_cor)
012776160725     c                   else
012777160725     c                   eval      w_dsp=i95dat
012778160725     c                   endif
012779160725     c                   endsr
012780160722     C**************************************************************************
012781160722     C     sr_elabora    BEGSR
012782160725     C**************************************************************************
012783160725     c* Gli assegnati non sono mai abilitati alla forzatura da EEX a DPD
012784160725     c                   if        i95tpo='A'
012785160725     c                   eval      OLVd1ESI='N'
012786160726     c                   eval      olvd1msg='Bolla in Assegnato'
012787160725     c                   leavesr
012788160725     c                   endif
012789160725     c* Verifica se Linea di partenza attiva per la forzatura
012790160722     c                   if        %lookup('999':sk_VPODPD)=0 and
012791160722     c                             %lookup(%editc(ilvd1lnp:'X'):sk_VPODPD)=0
012792160722     c                   eval      OLVd1ESI='N'
012793160725     c                   eval      olvd1msg='Linea di partenza non abilitata'
012794160722     c                   leavesr
012795160722     c                   endif
012796160725     c* Verifica se nazione servita da DPD e verifica i limiti di colli e peso
012797160725     c* previsti da DPD
012798170505    1c****               if        w_lad <=0 or
012799170505    1c                   if        ELVD1LNAD <=0 or
012800160722     c                             ilvd1ncl<>1 or
012801160722     c                             ilvd1pkg>§3ipkd
012802160722     c                   eval      OLVd1ESI='N'
012803160725     c                   select
012804170505     c****               when      w_lad<=0
012805170505     c                   when      ELVD1LNAD <=0
012806160725     c                   eval      olvd1msg='Nazione non servita da DPD'
012807160727     c                   when      ilvd1ncl<>1
012808160727     c                   eval      olvd1msg='Numero colli<>1'
012809160725     c                   when      ilvd1pkg>§3ipkd
012810160725     c                   eval      olvd1msg='Peso kg supera kg'
012811160725     c                   eval      olvd1msg=%trim(olvd1msg)+' '+
012812160725     c                             %editc(§3ipkd:'X')
012813160725     c                   endsl
012814160722     c                   leavesr
012815160722    1c                   endif
012816160722     c* recupero eventuale limite di peso e volume per nazione su tabella "16"
012817160722     C                   MOVEL     '16'          COD
012818160725     C                   MOVEL(P)  i95nar        KEY
012819160722     c                   clear                   ds16
012820160725     C                   z-add     *hival        §16pkd
012821160722     C                   z-add     *hival        §16vld
012822160722     c     ktab          chain     tabel00f
012823160725     c                   if        %found(tabel00f) and tblflg=' '
012824160722     C                   MOVEL     TBLUNI        DS16
012825160722     c                   endif
012826160725     c                   eval      OLVd1MAXP=§16PKD
012827160725     c                   eval      OLVd1MAXV=§16VLD
012828160725     c                   if        ilvd1pkg> §16pkd  or
012829160722     c                             ilvd1vlu > §16vld
012830160722     c                   eval      OLVd1ESI='N'
012831160725     c                   eval      olvd1msg='superati limiti di peso/volume-
012832160725     c                              previsti per la nazione richiesta'
012833160722     c                   leavesr
012834160722     c                   endif
012835180108     c* se presente una forzatura FLB non si instrada a DPD
012836180108     c                   clear                   trulflbds
012837180108     c                   movel     o95lna        iflblna
012838180108     c                   move      ILVD1LNP      Iflblnp
012840180108     c                   move      ilvd1ccm      iflbccm
012847180108     c                   call      'TRULFLBR'
012848180108     c                   parm                    trulflbds
012849180108    2c                   if        Oflblfz>*zeros
012851180108     c                   eval      OLVd1ESI='N'
012852180108     c                   eval      olvd1msg='Presente forzatura per il clie-
012853180108     c                             nte su tabella FLB'
012854180108     c                   leavesr
012855180108     c                   endif
012856160722     c* verifico eventuali blocchi per instradare a dpd  - TAB "IED"
012857160722     c                   clear                   fnlvdpds
012858160722     c                   eval      idplna=o95lna
012859160725     c                   eval      idpnaz=i95nar
012860160725     c                   eval      idpccm=ilvd1ccm
012861160722     c                   movel     fnlvdpds      kpjbu
012862160722     c                   call      'FNLVDPR'
012863160722     c                   parm                    kpjba
012864160722     c                   movel     kpjbu         fnlvdpds
012865160722     c                   if        odpesito='0'
012866160722     c                   eval      OLVd1ESI='N'
012867160725     c                   eval      olvd1msg='Presenti blocchi su tabella "IED"'
012868160722     c                   leavesr
012869160722     c                   endif
012870160722     c
012871160722     c* verifico eventuali blocchi cappario utilizzi particolari
012872160725     c                   eval      i97ntw='DPD'
012873160722     c                   call      'TISI97R'
012874160725     c                   parm                    tisi95ds
012875160725     c                   parm                    tisi97ds
012876160722    1c                   select
012877160725     c                   when      o97err<>*blanks
012878160722     c                   eval      OLVd1ESI='N'
012879160725     c                   eval      olvd1msg=o97msg
012880160722     c                   leavesr
012881160725     c                   when      o97uti='N'
012882160725     c                   movel     o97flo        dcppflo
012883160722    2c                   if        §cppall='S'  or
012884160722     c                             (§cppcas='S' and i95fca='S') or
012885160722     c                             (§cpppas='S' and i95tpo='A') or
012886160722     c                             (§cpppas='O' and i95tpo='A' and
012887160725     c                              (ilvd1ormf='S' or i97ntw<>ilvd1npoe))
012888160722     c                   eval      OLVd1ESI='N'
012889160725     c                   eval      olvd1msg='Presenti blocchi su AZCPP'
012890160722     c                   leavesr
012891160722    2c                   endif
012892160722    1c                   endsl
012893160722     c                   endsr
012894160725     C**************************************************************************
012895160725     C* Richiamo cappario dpd -----------------------------------
012896160725     C**************************************************************************
012897160725     c     sr_tisie3     begsr
012898160725     c                   clear                   tisie3ds
012899160725     c                   eval      isie3tla='E'
012900160725     c                   z-add     w_dsp         isie3dri
012901160725     C                   z-add     w_dsp         Isie3dsp
012902160725     c                   time                    isie3hsp
012903160725     C                   MOVEL     i95Nar        isie3nzd
012904160725     C                   MOVEL     i95CAp        Isie3cad
012905160725     c                   movel     i95loc        isie3cty
012906160725     c                   z-add     ilvd1lnp      isie3lnp
012907160725     C     ilvd1fbr      IFEQ      'S'
012908160725     c                   z-add     300           isie3srv
012909160725     c                   else
012910160725     c                   if        ilvd1pkg>§3ilmp
012911160725     c                   z-add     101           isie3srv
012912160725     c                   else
012913160725     c                   z-add     136           isie3srv
012914160725     c                   endif
012915160725     c                   endif
012916160725     c                   movel     'N'           isie3exc
012917160725     C                   CALL      'TISIE3R'
012918160725     C                   PARM                    tisie3ds
012919160725    3C                   IF        Osie3err <> ' '
012920160725     c                   eval      OLVD1esi='N'
012921160726     c                   eval      olvd1msg='Errore da TISIE3R'
012922160725     c                   endif
012923160725     c                   endsr
012924160725     C**************************************************************************
012925160725     C     sr_chiudi     BEGSR
012926160725     C**************************************************************************
012927160725     c                   clear                   fnlvdpds
012928160725     c                   movel     'C'           idptla
012929160725     c                   movel     fnlvdpds      kpjbu
012930160725     C                   CALL      'FNLVDPR'
012931160725     C                   PARM                    kpjba
012932160725
012933160725     c                   movel     'C'           i95tla
012934160725     c                   call      'TISI97R'
012935160725     c                   parm                    tisi95ds
012936160725     c                   parm                    tisi97ds
012937160725
012938160725     c                   eval      isie3tla='C'
012939160725     c                   call      'TISIE3R'
012940160725     C                   PARM                    TISIE3DS
012941180108
012942180108     c                   clear                   trulflbds
012943180108     c                   eval      iflbtla='C'
012944180108     c                   call      'TRULFLBR'
012945180108     c                   parm                    TRULFLBDS
012946160725
012947160725     c                   endsr
012948991206     C**************************************************************************
012949051123     C     *INZSR        BEGSR
012950160725     C**************************************************************************
012951160722     c* ricerca P.O. abilitati da tabella VPO per progetto
012952160722     c* forzatura instradamento da eex a dpd
012953160722     c                   clear                   trulvpods
012954160722     c                   eval      ivpoke1 = 'DECOFIDPD'
012955160722     c                   call      'TRULVPOR'
012956160722     c                   parm                    trulvpods
012957160722     c* Recupero Max peso bollettabile per DPD
012958160722     c                   eval      cod='3I'
012959160722     c                   eval      key='DPD'
012960160722     c                   clear                   DS3IDP
012961160722     c     ktab          chain     tabel00f
012962160725     c                   if        %found(tabel00f) and tblflg=' '
012963160722     c                   eval      DS3IDP=tbluni
012964160722     c                   endif
013000051123     c
013407140206     c
013408160722     C* ACCESSO   TABEL
013409160722     C     KTAB          KLIST
013410160722     C                   KFLD                    CODUT
013411160722     C                   KFLD                    COD
013412160722     C                   KFLD                    KEY
015300070313     c                   endsr
