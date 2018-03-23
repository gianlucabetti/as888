000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500161222      *  Aggiorna sped. Import dpd a fronte di ORM commissionato                *
000501161222      *  Abbina la sped. all'orm e cambia il porto da franco ad assegnato
000800060612      **?************************************************************************
000900161223     Ffnblp01l  uF   E           k DISK
000902161222     Ffiar403l  if   E           k disk
000903161223     Ffiar401l  if   E           k disk    rename(fiar4000:fiar4001)
000904161222     Ftabel00f  if   E           k disk
000905161223     Ffnorm01l  if   E           k disk
000906161223     Ffncls01l  IF   E           K DISK
000907161222
000908161222     d fieu39ds      e ds
000909161223     d fnlv19ds      e ds
000910161222     d ds3a          e ds
000911161223     D DSBL4M        E DS
000912161223     D DSBL4A        E DS
000913161223     D Dorm01        E DS
000914161223     d Ktrc            s                   like(Ar4trc)
000915161222     d codut           s              1  0 inz(1)
000916161222     d cod             s                   like(tblcod)
000917161222     d Key             s                   like(tblkey)
000918161223     d wtrov           s              1
000919161223     d wpart           s              1
000920161223     d wcliksc         s              7  0
000921161223     d wclictr         s              3
000922161223     D wclsflo8        S              1
001000060809      *
012500060612      * ?================================================================== */
012600060612      *   Ciclo principale
012700060413      * ?********************************************************************/
012701161222     c     *entry        plist
012702161222     c                   parm                    fieu39ds
012703161228     C*---------------------------------------------------------------*
012704161228     C* TIPO LANCIO, D19TLA "C" -           CHIUSO CON LR
012705161228     C* TIPO LANCIO, D19TLA "L" - ELABORO E CHIUDO CON LR
012706161228     C* TIPO LANCIO, D18TLA " " - ELABORO E CHIUDO IN RETRN
012707161228    1C     I39TLA        IFNE      'C'
012708161222
012709161222
012710161223     c                   eval      o39esito='0'
012711161228
012712161228     c                   if        i39tla='L'
012713161223     c                   eval      *inlr=*on
012714161228     c                   endif
012715161222
012716161223     c* Controllo i parametri ricevuti
012717161223     c* Errore se non passato il numero parcel
012718161222     c                   if        i39npr=*blanks
012719161223     c                   eval      o39esito='E'
012720161222     c                   exsr      routend
012721161222     c                   endif
012722161223     c     korm          chain     fnorm01l
012723161223     c                   if        not %found(fnorm01l) or ormfcs<>'F'
012724161223     c                   eval      o39esito='E'
012725161223     c                   exsr      routend
012726161223     c                   endif
012727161223     c                   eval      dorm01=ormflo
012728161223     c                   if        §orpfb<>'A'
012729161223     c                   eval      o39esito='E'
012730161223     c                   exsr      routend
012731161223     c                   endif
012732161223
012733161223     c* Col Numero Parcel recupero la spedizione
012734161223     c                   clear                   wtrov
012735161223     c                   clear                   wpart
012736161222     c                   eval      ar4trc='I'
012737161222     c                   movel(p)  i39npr        ar4n30
012738161223     c     kar43         setll     fiar403l
012739161223     c     ar4trc        reade     fiar403l
012740161223     c                   dow       not %eof(fiar403l) and wtrov=' '
012741161223     c                   if        %subst(ar4n30:1:14)=i39npr
012742161222     c     kblp          chain     fnblp01l
012743161222     c                   if        %found(fnblp01l)
012744161222     c* se bolla già partita non faccio niente
012745161223     c                   if        blpft2<>*blanks
012746161223     c* se partita memorizzo che è già partita
012747161223     c                   if        blpft2<>'N'
012748161223     c                   eval      wpart='S'
012749161223     c                   endif
012750161223     c                   else
012751161223     c                   eval      wtrov='S'
012752161222     c                   endif
012753161223     c                   endif
012754161223     c                   endif
012755161223     c                   if        %subst(ar4n30:1:14)>i39npr
012756161223     c     ar4trc        setgt     fiar403l
012757161223     c                   endif
012758161223     c     ar4trc        reade     fiar403l
012759161223     c                   enddo
012760161223
012761161223     c                   if        wtrov=' '
012762161223     c                   if        wpart='S'
012763161223     c* Bolla partita
012764161223     c                   eval      o39esito='P'
012765161223     c                   else
012766161223     c* Bolla non trovata
012767161223     c                   eval      o39esito='N'
012768161223     c                   endif
012769161223     c                   exsr      routend
012770161223     c                   endif
012771161223
012772161222     c* Escludo i recuperi e gli assegnati
012773161222     c                   movel     '3A'          COD
012774161222     c                   movel     BLPCBO        KEY
012775161222     c     ktab          chain     TABEL00F
012776161222     C                   IF        %FOUND(TABEL00F)
012777161222     C                   EVAL      DS3A=TBLUNI
012778161222     c                   if        §3arbl='R' or %subst(§3atb1:1:1)='A'
012779161223     c* Codice bolla di recupero o già in assegnato
012780161223     c                   eval      o39esito='B'
012781161222     c                   exsr      routend
012782161222     c                   endif
012783161222     C                   ENDIF
012784161223
012785161222     c                   eval      ktrc='M'
012786161222     c     kar4          chain     fiar401l
012787161222     c* se bolla già abbinata a orm non faccio niente
012788161222     c                   if        %found(fiar401l)
012789161223     c                   eval      o39esito='B'
012790161222     c                   exsr      routend
012791161222     c                   endif
012792161222     c* Se superati tutti i controlli posso aggiornare la bolla
012793161222     c                   exsr      sr_aggbolla
012794161228
012795161228     c                   endif
012796161228
012797161228     c                   if        i39tla='C'
012798161228     c                   seton                                        lr
012799161228     c                   endif
012800161228
012801161223     c                   exsr      routend
012802161222     c
012803161222
018401161223      *----------------------------------------------------------------
018402161223      *   Aggiornamento spedizione                                    *
018403161223      *----------------------------------------------------------------
018404161223     C     sr_aggbolla   BEGSR
018405161223     c* cambio il porto da Franco ad assegnato
018406161223     c                   eval      blpcbo='2 '
018407161223     c* creo il fiar4 record "M" per abbinare all'ORM
018408161223     c                   clear                   fnlv19ds
018414161223     c                   clear                   dsbl4m
018415161223     C                   MOVEL     'T'           D19ftr
018416161223     C                   MOVEL     BLPAAS        D19AAS
018417161223     C                   MOVEL     BLPLNP        D19LNP
018418161223     C                   MOVEL     BLPNRS        D19NRS
018419161223     C                   MOVEL     BLPNSP        D19NSP
018420161223     C                   Z-ADD     i39poe        §B4POE
018421161223     C                   Z-ADD     i39nsr        §B4NSR
018422161223     C                   Z-ADD     i39nor        §B4NOR
018423161223     C                   Z-ADD     i39nrv        §B4NRV
018426161223     C                   MOVEL     'M'           D19TRC
018427161223     C                   MOVEL     DSBL4M        D19NT1
018428161223     C                   CALL      'FNLV19R'
018429161223     C                   PARM                    fnlv19ds
018430161223     c* memorizzo ksc/ctr per tassazione assegnato
018432161223     c                   Z-add     OrmKsc        wcliksc
018434161223     c                   if        ormctr<>*blanks
018435161223     c                   Move      OrmCtr        wclictr
018437161223     c                   endif
018458161223
018459161223     c                   Clear                   dsbl4a
018460161223     c                   Clear                   fnlv19ds
018461161223     C                   MOVEL     'T'           D19ftr
018462161223     C                   MOVEL     BLPAAS        D19AAS
018463161223     C                   MOVEL     BLPLNP        D19LNP
018464161223     C                   MOVEL     BLPNRS        D19NRS
018465161223     C                   MOVEL     BLPNSP        D19NSP
018466161223     c                   Movel     'A'           d19trc
018467161223     c                   if        wcliksc>0
018468161223     c                   z-add     wcliksc       §b4aks
018469161223     C                   IF        wclictr<>*blanks
018470161223     c                   move      wclictr       §b4act
018471161223     c                   endif
018472161223     c                   exsr      sr_fncls
018473161223     c                   move      wclsflo8      §b4af1
018474161223     c                   Movel     dsbl4a        d19nt1
018475161223     c                   Call      'FNLV19R'
018476161223     c                   Parm                    fnlv19ds
018477161223     c                   endif
018479161223     c                   update    fnblp000
018480161223     c                   endsr
018481161223      *----------------------------------------------------------------
018482161223      *   operazioni finali                                *
018483161223      *----------------------------------------------------------------
018484161223     C     routend       BEGSR
018485161228
018486161223     c                   unlock(E) fnblp01l
018487161228
018488161228     c                   eval      d19tla  =  'C'
018489161228     c                   call      'FNLV19R'
018490161228     c                   parm                    fnlv19ds
018491161228
018492161223     c                   return
018493161228
018494161223     c                   endsr
018495161223      *--------------------------------------------------------------*
018496161223      * Chaino fncls per codice ass. per impostazione §b4af1
018497161223      *--------------------------------------------------------------*
018498161223     c     Sr_fncls      BegSr
018499161223     c                   clear                   WCLSflo8
018500161223     c     §b4aks        chain     fncls01l
018501161223     c                   if        %found(fncls01l)
018502161223     c                   eval      wclsflo8 = %SUBST(clsflo:8:1)
018503161223     c                   endif
018504161223     c                   endsr
018505161222      *----------------------------------------------------------------
018600161222      *   DEFINIZIONE CHIAVI                               *
018700161222      *----------------------------------------------------------------
018800161222     C     *INZSR        BEGSR
019200161222      **
019900161222     C     Kar43         KLIST
020000161222     C                   KFLD                    ar4trc
020100161222     C                   KFLD                    ar4n30
020200161222     C     Kblp          KLIST
020300161222     C                   KFLD                    ar4aas
020400161222     C                   KFLD                    ar4lnp
020500161222     C                   KFLD                    ar4nrs
020600161222     C                   KFLD                    ar4nsp
020700161222     C     Kar4          KLIST
020800161222     C                   KFLD                    blpaas
020900161222     C                   KFLD                    blplnp
021000161222     C                   KFLD                    blpnrs
021100161222     C                   KFLD                    blpnsp
021200161222     C                   KFLD                    ktrc
021300161222     C     Ktab          KLIST
021400161222     C                   KFLD                    codut
021500161222     C                   KFLD                    cod
021501161222     C                   KFLD                    key
022000161223     C     Korm          KLIST
022100161223     C                   KFLD                    i39poe
022200161223     C                   KFLD                    i39nsr
022300161223     C                   KFLD                    i39nor
022400161223     C                   KFLD                    i39nrv
022500161223     c                   endsr
