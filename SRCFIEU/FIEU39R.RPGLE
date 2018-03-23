000100050414     H DECEDIT('0,') DATEDIT(*YMD/)
000200171107      ** ************************************************************************
000300161222      *  Aggiorna sped. Import dpd a fronte di ORM commissionato                *
000400161222      *  Abbina la sped. all'orm e cambia il porto da franco ad assegnato
000500171107      ** ************************************************************************
000600161223     Ffnblp01l  uF   E           k DISK
000700171107      *---
000800171107     Ffipnd02l  if   E           k disk
000900171107     F*****fiar403l  if   E           k disk
001000171107      *---
001100171107     Ffiar401l  if   E           k disk    rename(fiar4000:fiar4001)
001200161222     Ftabel00f  if   E           k disk
001300161223     Ffnorm01l  if   E           k disk
001400161223     Ffncls01l  IF   E           K DISK
001500161222
001600161222     d fieu39ds      e ds
001700161223     d fnlv19ds      e ds
001800161222     d ds3a          e ds
001900161223     D DSBL4M        E DS
002000161223     D DSBL4A        E DS
002100161223     D Dorm01        E DS
002200161223     d Ktrc            s                   like(Ar4trc)
002300161222     d codut           s              1  0 inz(1)
002400161222     d cod             s                   like(tblcod)
002500161222     d Key             s                   like(tblkey)
002600161223     d wtrov           s              1
002700161223     d wpart           s              1
002800161223     d wcliksc         s              7  0
002900161223     d wclictr         s              3
003000161223     D wclsflo8        S              1
003100060809      *
003200171107      *  ================================================================== */
003300060612      *   Ciclo principale
003400171107      *  ********************************************************************/
003500161222     c     *entry        plist
003600161222     c                   parm                    fieu39ds
003700161228     C*---------------------------------------------------------------*
003800161228     C* TIPO LANCIO, D19TLA "C" -           CHIUSO CON LR
003900161228     C* TIPO LANCIO, D19TLA "L" - ELABORO E CHIUDO CON LR
004000161228     C* TIPO LANCIO, D18TLA " " - ELABORO E CHIUDO IN RETRN
004100161228    1C     I39TLA        IFNE      'C'
004200161222
004300161222
004400161223     c                   eval      o39esito='0'
004500161228
004600161228     c                   if        i39tla='L'
004700161223     c                   eval      *inlr=*on
004800161228     c                   endif
004900161222
005000161223     c* Controllo i parametri ricevuti
005100161223     c* Errore se non passato il numero parcel
005200161222     c                   if        i39npr=*blanks
005300161223     c                   eval      o39esito='E'
005400161222     c                   exsr      routend
005500161222     c                   endif
005600161223     c     korm          chain     fnorm01l
005700161223     c                   if        not %found(fnorm01l) or ormfcs<>'F'
005800161223     c                   eval      o39esito='E'
005900161223     c                   exsr      routend
006000161223     c                   endif
006100161223     c                   eval      dorm01=ormflo
006200161223     c                   if        §orpfb<>'A'
006300161223     c                   eval      o39esito='E'
006400161223     c                   exsr      routend
006500161223     c                   endif
006600161223
006700161223     c* Col Numero Parcel recupero la spedizione
006800161223     c                   clear                   wtrov
006900161223     c                   clear                   wpart
007000171107      *--
007100171107     c*******            eval      ar4trc='I'
007200171107     c*******            movel(p)  i39npr        ar4n30
007300171107     c**** kar43         setll     fiar403l
007400171107     c**** ar4trc        reade     fiar403l
007500171107     c*******            dow       not %eof(fiar403l) and wtrov=' '
007600171107     c*******            if        %subst(ar4n30:1:14)=i39npr
007700171107     c**** kblp          chain     fnblp01l
007800171107     c*******            if        %found(fnblp01l)
007900161222     c* se bolla già partita non faccio niente
008000171107     c*******            if        blpft2<>*blanks
008100161223     c* se partita memorizzo che è già partita
008200171107     c*******            if        blpft2<>'N'
008300171107     c*******            eval      wpart='S'
008400171107     c*******            endif
008500171107     c*******            else
008600171107     c*******            eval      wtrov='S'
008700171107     c*******            endif
008800171107     c*******            endif
008900171107     c*******            endif
009000171107     c*******            if        %subst(ar4n30:1:14)>i39npr
009100171107     c**** ar4trc        setgt     fiar403l
009200171107     c*******            endif
009300171107     c**** ar4trc        reade     fiar403l
009400171107     c*******            enddo
009500171107      **
009600171107      **  SOSTITUITO con il nuovo FIPND quello che era il FIAR4 "I"
009700171107     c     i39npr        setll     fipnd02l
009800171107     c     i39npr        reade     fipnd02l
009900171107     c                   dow       not %eof(fipnd02l) and wtrov=' '
010000171107     c     kblp          chain     fnblp01l
010100171107     c                   if        %found(fnblp01l)
010200171107     c* se bolla già partita non faccio niente
010300171107     c                   if        blpft2<>*blanks
010400171107     c* se partita memorizzo che è già partita
010500171107     c                   if        blpft2<>'N'
010600171107     c                   eval      wpart='S'
010700171107     c                   endif
010800171107     c                   else
010900171107     c                   eval      wtrov='S'
011000171107     c                   endif
011100171107     c                   endif
011200171107     c     i39npr        reade     fipnd02l
011300171107     c                   enddo
011400171107      *******
011500161223     c                   if        wtrov=' '
011600161223     c                   if        wpart='S'
011700161223     c* Bolla partita
011800161223     c                   eval      o39esito='P'
011900161223     c                   else
012000161223     c* Bolla non trovata
012100161223     c                   eval      o39esito='N'
012200161223     c                   endif
012300161223     c                   exsr      routend
012400161223     c                   endif
012500161223
012600161222     c* Escludo i recuperi e gli assegnati
012700161222     c                   movel     '3A'          COD
012800161222     c                   movel     BLPCBO        KEY
012900161222     c     ktab          chain     TABEL00F
013000161222     C                   IF        %FOUND(TABEL00F)
013100161222     C                   EVAL      DS3A=TBLUNI
013200161222     c                   if        §3arbl='R' or %subst(§3atb1:1:1)='A'
013300161223     c* Codice bolla di recupero o già in assegnato
013400161223     c                   eval      o39esito='B'
013500161222     c                   exsr      routend
013600161222     c                   endif
013700161222     C                   ENDIF
013800161223
013900161222     c                   eval      ktrc='M'
014000161222     c     kar4          chain     fiar401l
014100161222     c* se bolla già abbinata a orm non faccio niente
014200161222     c                   if        %found(fiar401l)
014300161223     c                   eval      o39esito='B'
014400161222     c                   exsr      routend
014500161222     c                   endif
014600161222     c* Se superati tutti i controlli posso aggiornare la bolla
014700161222     c                   exsr      sr_aggbolla
014800161228
014900161228     c                   endif
015000161228
015100161228     c                   if        i39tla='C'
015200161228     c                   seton                                        lr
015300161228     c                   endif
015400161228
015500161223     c                   exsr      routend
015600161222     c
015700161222
015800161223      *----------------------------------------------------------------
015900161223      *   Aggiornamento spedizione                                    *
016000161223      *----------------------------------------------------------------
016100161223     C     sr_aggbolla   BEGSR
016200161223     c* cambio il porto da Franco ad assegnato
016300161223     c                   eval      blpcbo='2 '
016400161223     c* creo il fiar4 record "M" per abbinare all'ORM
016500161223     c                   clear                   fnlv19ds
016600161223     c                   clear                   dsbl4m
016700161223     C                   MOVEL     'T'           D19ftr
016800161223     C                   MOVEL     BLPAAS        D19AAS
016900161223     C                   MOVEL     BLPLNP        D19LNP
017000161223     C                   MOVEL     BLPNRS        D19NRS
017100161223     C                   MOVEL     BLPNSP        D19NSP
017200161223     C                   Z-ADD     i39poe        §B4POE
017300161223     C                   Z-ADD     i39nsr        §B4NSR
017400161223     C                   Z-ADD     i39nor        §B4NOR
017500161223     C                   Z-ADD     i39nrv        §B4NRV
017600161223     C                   MOVEL     'M'           D19TRC
017700161223     C                   MOVEL     DSBL4M        D19NT1
017800161223     C                   CALL      'FNLV19R'
017900161223     C                   PARM                    fnlv19ds
018000161223     c* memorizzo ksc/ctr per tassazione assegnato
018100161223     c                   Z-add     OrmKsc        wcliksc
018200161223     c                   if        ormctr<>*blanks
018300161223     c                   Move      OrmCtr        wclictr
018400161223     c                   endif
018500161223
018600161223     c                   Clear                   dsbl4a
018700161223     c                   Clear                   fnlv19ds
018800161223     C                   MOVEL     'T'           D19ftr
018900161223     C                   MOVEL     BLPAAS        D19AAS
019000161223     C                   MOVEL     BLPLNP        D19LNP
019100161223     C                   MOVEL     BLPNRS        D19NRS
019200161223     C                   MOVEL     BLPNSP        D19NSP
019300161223     c                   Movel     'A'           d19trc
019400161223     c                   if        wcliksc>0
019500161223     c                   z-add     wcliksc       §b4aks
019600161223     C                   IF        wclictr<>*blanks
019700161223     c                   move      wclictr       §b4act
019800161223     c                   endif
019900161223     c                   exsr      sr_fncls
020000161223     c                   move      wclsflo8      §b4af1
020100161223     c                   Movel     dsbl4a        d19nt1
020200161223     c                   Call      'FNLV19R'
020300161223     c                   Parm                    fnlv19ds
020400161223     c                   endif
020500161223     c                   update    fnblp000
020600161223     c                   endsr
020700161223      *----------------------------------------------------------------
020800161223      *   operazioni finali                                *
020900161223      *----------------------------------------------------------------
021000161223     C     routend       BEGSR
021100161228
021200161223     c                   unlock(E) fnblp01l
021300161228
021400161228     c                   eval      d19tla  =  'C'
021500161228     c                   call      'FNLV19R'
021600161228     c                   parm                    fnlv19ds
021700161228
021800161223     c                   return
021900161228
022000161223     c                   endsr
022100161223      *--------------------------------------------------------------*
022200161223      * Chaino fncls per codice ass. per impostazione §b4af1
022300161223      *--------------------------------------------------------------*
022400161223     c     Sr_fncls      BegSr
022500161223     c                   clear                   WCLSflo8
022600161223     c     §b4aks        chain     fncls01l
022700161223     c                   if        %found(fncls01l)
022800161223     c                   eval      wclsflo8 = %SUBST(clsflo:8:1)
022900161223     c                   endif
023000161223     c                   endsr
023100161222      *----------------------------------------------------------------
023200161222      *   DEFINIZIONE CHIAVI                               *
023300161222      *----------------------------------------------------------------
023400161222     C     *INZSR        BEGSR
023500171107      ****
023600171107     C**** Kar43         KLIST
023700171107     C****               KFLD                    ar4trc
023800171107     C****               KFLD                    ar4n30
023900171107     C**** Kblp          KLIST
024000171107     C****               KFLD                    ar4aas
024100171107     C****               KFLD                    ar4lnp
024200171107     C****               KFLD                    ar4nrs
024300171107     C****               KFLD                    ar4nsp
024400171107     C     Kblp          KLIST
024500171107     C                   KFLD                    PNDaas
024600171107     C                   KFLD                    PNDlnp
024700171107     C                   KFLD                    PNDnrs
024800171107     C                   KFLD                    PNDnsp
024900161222     C     Kar4          KLIST
025000161222     C                   KFLD                    blpaas
025100161222     C                   KFLD                    blplnp
025200161222     C                   KFLD                    blpnrs
025300161222     C                   KFLD                    blpnsp
025400161222     C                   KFLD                    ktrc
025500161222     C     Ktab          KLIST
025600161222     C                   KFLD                    codut
025700161222     C                   KFLD                    cod
025800161222     C                   KFLD                    key
025900161223     C     Korm          KLIST
026000161223     C                   KFLD                    i39poe
026100161223     C                   KFLD                    i39nsr
026200161223     C                   KFLD                    i39nor
026300161223     C                   KFLD                    i39nrv
026400161223     c                   endsr
