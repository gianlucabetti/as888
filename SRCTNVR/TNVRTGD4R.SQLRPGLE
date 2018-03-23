000100041230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200130415      *----------------------------------------------------------------*
000300150917      * Statistica reclami bolle
000400130415      *----------------------------------------------------------------*
000500150917     ftabel00f  if   e           k disk
000600150917     fazorg01l  if   e           k disk
000700150921     fFITGD04l  if   e           k disk
000800150930     fFITGn01l  if   e           k disk
000900150930     ffiarbf2c  if   e           k disk
001000150917     fwfTGt00F  uf a e             disk
001100150923     ffnevb01l  if   e           k disk
001200150918     ftigcp01l  if   e           k disk
001300150917     ffnarb01l  if   e           k disk
001400150917     ffnblp01l  if   e           k disk    prefix(arb:3)
001500150923     ffnblp31l  if   e           k disk    rename(fnblp000:fnblp031)
001600150917     ffiar501l  if   e           k disk
001700150917     fTitas30c  if   e           k Disk    UsrOpn extfile(wtitas)
001800150917     f
001900150917     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002000150917
002100150917     d manc            s              3    dim(100)
002200150917     d pri             s              7  0 dim(24)
002300150917     d disp            s              7  0 dim(24)
002400150917     d reso            s              7  0 dim(24)
002500150917     d dir             s              7  0 dim(24)
002600150917     d tot             s              7  0 dim(24)
002700150918     d gia             s              7  0 dim(24)
002800150930     d tvar            s              2    dim(31)
002900150930     d oraTGD          s               t   timfmt(*eur)
003000150930     d oraVAR          s               t   timfmt(*eur)
003100150921
003200150921     D UDAT            DS                  OCCURS(16)  INZ
003300150930     D  var                    1     93
003400150921     D                                     DIM(31)
003500150921     D  totra                 94    217P 0
003600150921     D                                     DIM(31)
003700041230      *
003800041231      * - Parametri x Controllo profilo utenti
003900041231     d TIBS34DS      e ds                  inz
004000041231      * - Ds di riferimento al file esterno AZUTE00F
004100041231     d AZUTEDS       e ds                  extname(AZUTE00F)
004200041231      * - Ds per dati organigramma
004300041231     d DDatiUte      e ds
004400041230      *
004500041231     d KPJBA         e ds
004600150917     d dtasflo       e ds
004700150917     d dar5emd       e ds
004800150917     d ds2a          e ds
004900150917     d
005000150917     D param           DS
005100150917     D  datada                 1      8  0
005200150917     D  dataa                  9     16  0
005300150917     D  fil                   17     19  0
005400150923     D  priv                  20     20
005500150929     D  alert                 21     21
005600161122     d* mic="S"  al posto del primo tentativo consegna controllo evento MIC con la r.a.
005700161122     d* MIC= Y   elaboro solo r.a. preima del primo MIC
005800161122     D  MIC                   22     22
005900161122     D  fasciasn              23     23
006000150917     D                 DS
006100150917     D  wlnp                   1      3  0
006200150917     D  wnrs                   4      5  0
006300150917     D  wnsp                   6     12  0
006400150917     D  wann                  13     16  0
006500150917     D  wbolla                 1     16
006600150917     d
006700150918     d wdata           s              8  0
006800150918     d tal             s              1
006900150917     d primo           s              1
007000161122     d dopomic         s              1
007100150930     d variaz          s              1
007200150930     d wcvb            s                   like(arbcvb)
007300150930     d nummin          s              7  0
007400150918     d wtitas          s             21    inz('GAITRAGRU /TITAS30C')
007500041230      *
007600150917     c     *entry        plist
007700150917     c                   parm                    kpjba
007800150917     c                   movel     kpjbu         param
007900150918     c* pulisco il file
008000150918      /FREE
008100150918       exec sql delete from WFTGT00F    ;
008200150918      /END-FREE
008300150917     c* Apertura file
008400150917     c                   if        %subst( knsif : 7 : 1) = 'P'
008500150917     c                   eval      %subst( wtitas : 7 : 4 ) = 'GRPS'
008600150917     c                   endif
008700150917     c
008800150917     c                   open      titas30c
008900150917     c
009000150917     c                   if        fil>0
009100150917     c                   eval      orgfil=fil
009200150917     c                   exsr      leggiTGD
009300150917     c                   else
009400150917     c     *loval        setll     azorg01l
009500150917     c                   read      azorg01l
009600150917     c                   dow       not %eof(azorg01l)
009700150917     c                   if        orgfag='F' or orgfag='A'
009800150917     c                   if        orgfva=' '
009900150917     c                   exsr      leggiTGD
010000150917     c                   endif
010100150917     c                   endif
010200150917     c                   read      azorg01l
010300150917     c                   enddo
010400150917     c                   endif
010500150917     c
010600150917     c*   riempo il file
010700150917     c                   exsr      scrivi
010800150923     c* conto le bolle che  hanno un tentativo di consegna
010900161122     c                   if        Mic=' '
011000150923     c                   exsr      contacons
011100161122     c                   endif
011200130415      *
011300150917     c                   except    err
011400041231     c                   seton                                        lr
011500041230      *
011600041231      *---------------------------------------------------------------*
011700041231      * Routine Iniziale                                              *
011800041231      *---------------------------------------------------------------*
011900041231     c     *InzSR        BegSR
012000041231      *
012100041231      * Reperisco dati job
012200041231     c                   exsr      DatiJob
012300150917     c
012400150917     c     ktgd          klist
012500150917     c                   kfld                    orgfil
012600150917     c                   kfld                    kdac              8 0
012700150923     c     kblp          klist
012800150923     c                   kfld                    orgfil
012900150923     c                   kfld                    kaas              4 0
013000150923     c                   kfld                    kmgs              4 0
013100150917     c     kbolla        klist
013200150917     c                   kfld                    wann
013300150917     c                   kfld                    wlnp
013400150917     c                   kfld                    wnrs
013500150917     c                   kfld                    wnsp
013600150917     c     kar5          klist
013700150917     c                   kfld                    wann
013800150917     c                   kfld                    wlnp
013900150917     c                   kfld                    wnrs
014000150917     c                   kfld                    wnsp
014100150917     c                   kfld                    ktrc              3
014200150917     c                   eval      ktrc='EMD'
014300150917     c     ktgd32        klist
014400150917     c                   kfld                    tgdtor
014500150917     c                   kfld                    tgdogg
014600150917     c     ktab          klist
014700150917     c                   kfld                    codut             1 0
014800150917     c                   kfld                    kcod              2
014900150917     c                   z-add     1             codut
015000150930     c     kTGN          klist
015100150930     c                   kfld                    tgdant
015200150930     c                   kfld                    tgdnut
015300150917     c
015400150917     c* Carico gli eventi di 1a mancata consegna
015500150917     c                   eval      kcod='2A'
015600150917     c                   eval      xx=1
015700150917     c     ktab          setll     tabel00f
015800150917     c     ktab          reade     tabel00f
015900150917     c                   dow       not %eof(tabel00f)
016000150917     c                   if        tblflg=' '
016100150917     c                   movel     tbluni        ds2a
016200150917     c                   if        §2aftc<>' '
016300150917     c                   movel     tblkey        manc(xx)
016400150917     c                   add       1             xx                3 0
016500150917     c                   endif
016600150917     c                   endif
016700150917     c
016800150917     c     ktab          reade     tabel00f
016900150917     c                   enddo
017000150917     c
017100041231     C                   ENDSR
017200041231      *
017300041231      *---------------------------------------------------------------*
017400041231      * Reperimento Dati del job (Utente/Operativi)                   *
017500041231      *---------------------------------------------------------------*
017600041231     c     DatiJob       BegSr
017700041231      *
017800041231     c     *dtaara       define    §azute        azuteds
017900041231     c     *dtaara       define    §datiute      ddatiute
018000041231      *
018100041231     c                   in(E)     *dtaara
018200041231     c                   IF        %ERROR or RSUT = *blanks
018300041231     c                   clear                   Tibs34Ds
018400041231     c                   call      'TIBS34R'
018500041231     c                   parm                    Tibs34Ds
018600041231     c                   in        *dtaara
018700041231     c                   ENDIF
018800041231     C                   ENDSR
018900150917      *---------------------------------------------------------------*
019000150917      * lettura TGD
019100150917      *---------------------------------------------------------------*
019200150917     c     LeggiTGD      BegSr
019300150917     c                   eval      kdac=datada
019400150921     c     ktgd          setll     fitgd04l
019500150921     c     orgfil        reade     fitgd04l
019600150921     c                   dow       not %eof(fitgd04l)
019700161124     c                   if        tgddac>dataa
019800150917     c                   leave
019900150917     c                   endif
020000150917     c* elaboro solo M D C   e telefonate per  spedizione
020100150917     c                   if        tgdtco='T' and tgdtor='S' and tgdogg<>*blanks
020200161122     c                             and %subst(tgdflo:1:1)<>'R' and
020300161122     c                             %subst(tgdflo:1:1)<>'C'
020400150921     c***                if        tgdtch='D' or tgdtch='M' or tgdtch='C'
020500150921     c                   if        tgdtch='D' or tgdtch='M'
020600150917     c                   movel     tgdtch        ch                1
020700150917     c* fascia oraria
020800150917     c                   clear                   fascia            4 0
020900161122     c                   if        fasciasn=' '
021000161124     c                   if        tgdorc<140000
021100150917     c                   eval      fascia=0800
021200150917     c                   else
021300150917     c                   eval      fascia=1400
021400150917     c                   endif
021500161122     c                   endif
021600150917     c
021700150917     c                   exsr      elabolla
021800150917     c                   endif
021900150917     c                   endif
022000150917     c
022100150921     c     orgfil        reade     fitgd04l
022200150917     c                   enddo
022300150917     c
022400150917     c                   endsr
022500150917      *---------------------------------------------------------------*
022600150917      * lettura bolle
022700150917      *---------------------------------------------------------------*
022800150917     c     elabolla      BegSr
022900161124     c                   movel     tgdorc        wora              4 0
023000150917     c                   movel     tgdogg        wbolla
023100150917     c                   movel     'F'           tipob
023200150930     c                   eval      variaz='N'
023300150930     c                   clear                   note            256
023400150930     c                   clear                   dirot             1
023500150930     c                   clear                   post              1
023600150917     c
023700150917     c     kbolla        chain     fnarb01l
023800150917     c                   if        not %found(fnarb01l)
023900150917     c     kbolla        chain     fnblp01l
024000150917     c                   if        not %found(fnblp01l)
024100150917     c     kbolla        chain     titas30c
024200150917     c                   if        not %found(titas30c)
024300150917     c                   add       1             nobolla           7 0
024400150917     c                   leavesr
024500150917     c                   else
024600150917     c                   movel     'T'           tipob             1
024700150922     c                   movel     tasgva        arbgva
024800150917     c                   movel     tascca        arbcca
024900150917     c                   movel     tashmc        arbhmc
025000150917     c                   endif
025100150917     c                   endif
025200150917     c                   endif
025300150923     c
025400150923     c* se ho chieso solo spedizioni ai rpivati, escluso quelle senza "P"
025500150923     c                   if        priv ='P' and %subst(arbgva:1:1)<>'P'
025600150923     c                   leavesr
025700150923     c                   endif
025800150917     c
025900150917     c* possibilità di alert
026000150917    1c                   if        tipob='F'
026100150917     c     kar5          chain     fiar501l
026200150917    2c                   if        not %found(fiar501l)
026300150917     c                   eval      tal='N'
026400150917   x2c                   else
026500150917     c                   movel     ar5uni        dar5emd
026600150921    3c                   if        §AR5smp='S'         or §ar5mmp='S'
026700150917     c                   eval      tal='S'
026800150917     c                   else
026900150917     c                   eval      tal='N'
027000150917    3c                   endif
027100150917    2c                   endif
027200150917     c
027300150917   x1c                   else
027400150917     c                   movel     tasflo        dtasflo
027500150917    2c                   if        §floemd='S'
027600150917     c                   eval      tal='S'
027700150917     c                   else
027800150917     c                   eval      tal='N'
027900150917    2c                   endif
028000150917    1c                   endif
028100150929     c
028200150929     c* Se voglio solo le bolle con ALERT escludo se manca
028300161122    1c                   if        alert=' ' or (alert='S' and tal='S') or
028400150929     c                             (alert='N' and tal='N')
028500150918     c
028600161122     c* se ho chiesto le r.a. prima del primo MIC
028700161122     c*  NON controllo il primo tentativo di consegna
028800161122    2c                   if        mic<>' '
028900161122
029000161122     c                   eval      dopomic='N'
029100161122     c     kbolla        setll     fnevb01l
029200161122     c     kbolla        reade     fnevb01l
029300161122    3c                   dow       not %eof(fnevb01l)
029400161122    4c                   if        evbcev='MIC'
029500161122     c* Verifico l'orario
029600161124    5c                   if        tgddac>evbdev
029700161122     c                   eval      dopomic='S'
029800161122  x5 c                   else
029900161124    6c                   if        tgddac=evbdev and wora  >evbhev
030000161122     c                   eval      dopomic='S'
030100161122    6c                   endif
030200161122    5c                   endif
030300161122     c
030400161122     c                   leave
030500161122    4c                   endif
030600161122     c
030700161122     c     kbolla        reade     fnevb01l
030800161122    3c                   enddo
030900161122   x2c                   else
031000161122     c
031100150917     c* verifico se la bolla ha evento di consegna prima o dopo questa r.a.
031200150917     c                   eval      primo='N'
031300150923     c     kbolla        setll     fnevb01l
031400150923     c     kbolla        reade     fnevb01l
031500161122    3c                   dow       not %eof(fnevb01l)
031600161122    4c                   if        %lookup(evbcev:manc)>0
031700150917     c* Verifico l'orario
031800161124    5c                   if        tgddac>evbdev
031900150918     c                   eval      primo='S'
032000161122  x5 c                   else
032100161124    6c                   if        tgddac=evbdev and wora  >evbhev
032200150917     c                   eval      primo='S'
032300161122    6c                   endif
032400161122    5c                   endif
032500150918     c
032600150918     c                   leave
032700161122    4c                   endif
032800150917     c
032900150923     c     kbolla        reade     fnevb01l
033000161122    3c                   enddo
033100161122    2c                   endif
033200161122     c*
033300161122     c* se ho chieso solo r.a. prima del primo MIC parzializzo
033400161122   2ac                   if        mic<>'Y' or (mic='Y' and dopomic ='N')
033500150917     c* indice schiera
033600161122     c*
033700150917     c                   clear                   ZZ                3 0
033800150917     c                   if        ch='M'
033900150917     c                   z-add     1             zz
034000150917     c                   endif
034100150917     c                   if        ch='D'
034200150917     c                   z-add     9             zz
034300150917     c                   endif
034400150917     c                   if        ch='C'
034500150917     c                   z-add     17            zz
034600150917     c                   endif
034700150917     c* fascia
034800150917     c                   if        fascia=1400
034900150917     c                   add       4             zz
035000150917     c                   endif
035100150917     c* presenza alert
035200150917     c                   if        tal='N'
035300150917     c                   add       2             zz
035400150917     c                   endif
035500150917     c* prima o dopo 1°tent.consegna
035600161122     c                   if        mic=' '
035700150917     c                   if        primo='S'
035800150917     c                   add       1             zz
035900150917     c                   endif
036000161122     c                   else
036100161122     c                   if        dopomic='S'
036200161122     c                   add       1             zz
036300161122     c                   endif
036400161122     c                   endif
036500150917     c* totale
036600150917     c                   add       1             tot(ZZ)
036700150917     c* privato
036800150922     c                   if        %subst(arbgva:1:1)='P'
036900150917     c                   add       1             pri(ZZ)
037000150917     c                   endif
037100150918     c* con giacenza
037200150918     c     kbolla        chain     tigcp01l
037300150918     c                   if        %found(tigcp01l)
037400150918     c                   z-add     gcpmgc        wdata
037500150918     c                   movel     gcpagc        wdata
037600150918     c                   if        tgddac> wdata
037700150918     c                   add       1             gia(zz)
037800150918     c                   endif
037900150918     c                   endif
038000150930     c
038100150930     c* Verifico se ha prodotto una variazione
038200150930     c     kbolla        setll     fiarbf2c
038300150930     c     kbolla        reade     fiarbf2c
038400150930     c                   dow       not %eof(fiarbf2c)
038500150930     c                   if        arbcvb='I0' or arbcvb='I7' or
038600150930     c                             arbcvb='NT' or
038700150930     c                             %subst(arbcvb:1:1)='C' or
038800150930     c                             %subst(arbcvb:1:1)='K' or
038900150930     c                             %subst(arbcvb:1:1)='T'
039000150930     c                   if        arbdtv=tgddac
039100150930     c     *eur          movel     arborv        oravar
039200150930     c     *eur          movel     tgdorc        oratgd
039300150930     c                   if        arborv>tgdorc
039400150930     c                   eval      nummin=%diff(oravar:oratgd :*mn)
039500150930     c                   else
039600150930     c                   eval      nummin=%diff(oratgd:oravar :*mn)
039700150930     c                   endif
039800150930     c                   if        nummin<=5
039900150930     c                   eval      variaz='S'
040000150930     c                   leave
040100150930     c                   endif
040200150930     c                   endif
040300150930     c                   endif
040400150930     c
040500150930     c     kbolla        reade     fiarbf2c
040600150930     c                   enddo
040700150930     c
040800150930     c* Se non ha variazione verifico alcune note fisse
040900150930     c                   if        variaz='N'
041000150930     c                   z-add     1             conta             2 0
041100150930     c     ktgn          setll     fitgn01l
041200150930     c     ktgn          reade     fitgn01l
041300150930     c                   dow       not %eof(fitgn01l) and conta<=7
041400150930     c                   eval      note=%trim(note)+tgnnot
041500150930     c                   add       1             conta
041600150930     c
041700150930     c     ktgn          reade     fitgn01l
041800150930     c                   enddo
041900150930     c
042000150930     c                   select
042100150930     c                   when      %scan('Indirizzo Alternativo':note)>0
042200150930     c                   eval      dirot ='S'
042300150930     c                   when      %scan('DataConsegnaRichiesta':note)>0
042400150930     c                   eval      post  ='S'
042500150930     c                   endsl
042600150930
042700150930     c                   else
042800150930     c* Raggruppo le causali variazioni
042900150930     c                   eval      wcvb=arbcvb
043000150930     c                   if        arbcvb='I7'
043100150930     c                   eval      wcvb='I0'
043200150930     c                   endif
043300150930     c                   if        %subst(wcvb:1:1)='C' or
043400150930     c                             %subst(wcvb:1:1)='K' or
043500150930     c                             %subst(wcvb:1:1)='T'
043600150930     c                   eval      %subst(wcvb:2:1)=' '
043700150930     c                   endif
043800150930     c                   endif
043900150921     c
044000150930     c                   if        variaz='N' and dirot=' ' and post=' '
044100150930     c                   z-add     1             ra
044200150930     c                   eval      tvar(1)='NO'
044300150930     c                   eval      wcvb   ='NO'
044400150930     c                   endif
044500150930     c                   if        variaz='N' and dirot='S' and post=' '
044600150930     c                   z-add     2             ra
044700150930     c                   eval      tvar(2)='DI'
044800150930     c                   eval      wcvb   ='DI'
044900150930     c                   endif
045000150930     c                   if        variaz='N' and dirot=' ' and post='S'
045100150930     c                   z-add     3             ra
045200150930     c                   eval      tvar(3)='PO'
045300150930     c                   eval      wcvb   ='PO'
045400150930     c                   endif
045500150930     c
045600150930     c
045700150930     c                   if        variaz='S'
045800150930     c                   z-add     4             ra                3 0
045900150930     c                   eval      ra=%lookup(wcvb:tvar:4)
046000150921     c                   if        ra=0
046100150930     c                   eval      ra=%lookup('   ':tvar:4)
046200150921     c                   if        ra>0 and ra<=30
046300150930     c                   eval      tvar(ra)=wcvb
046400150921     c                   else
046500150921     c                   eval      ra=31
046600150930     c                   eval      tvar(31)='OL'
046700150921     c                   endif
046800150921     c                   endif
046900150930
047000150930     c                   endif
047100150921     c
047200161124     c                   except    detf
047300161124     c
047400150921     C     ZZ            OCCUR     UDAT                                         *POSIZIONAMENTO
047500150921    1c                   if        ra=31
047600150930    2c                   if        var(31)=*blanks
047700150930     c                   eval      var(31)='OL '
047800150921     c                   z-add     1             totra(31)
047900150921     c                   else
048000150921     c                   add       1             totra(31)
048100150921    2c                   endif
048200150921     c
048300150921   x1c                   else
048400150930    2c                   if        var(ra)=*blanks
048500150930     c                   eval      var(ra)=wcvb
048600150921     c                   z-add     1             totra(ra)
048700150921     c                   else
048800150921     c                   add       1             totra(ra)
048900161122   2ac                   endif
049000161122    2c                   endif
049100161122    1c                   endif
049200150929     c
049300150929    1c                   endif
049400150917     c
049500150917     c                   endsr
049600150917      *---------------------------------------------------------------*
049700150917      * scrittura file
049800150917      *---------------------------------------------------------------*
049900150917     c     scrivi        BegSr
050000150917     c*
050100150917     c                   clear                   wftgt000
050200150917     c                   eval      WATDTADA=datada
050300150917     c                   eval      WATDTAa =dataa
050400150917     c                   eval      WTGTTCH ='M'
050500161122     c                   if        fasciasn=' '
050600150917     c                   eval      WTGTFORA=0800
050700161122     c                   endif
050800161122     c
050900150917     c                   z-add     1             zz
051000150917     c                   exsr      Wrifascia
051100150917     c
051200161122     c                   if        fasciasn=' '
051300150917     c                   eval      WTGTFORA=1400
051400150917     c                   add       1             zz
051500150917     c                   exsr      Wrifascia
051600161122     c                   endif
051700150917     c
051800150917     c*
051900150917     c                   clear                   wftgt000
052000150917     c                   eval      WATDTADA=datada
052100150917     c                   eval      WATDTAa =dataa
052200150917     c                   eval      WTGTTCH ='D'
052300161122     c                   if        fasciasn=' '
052400150917     c                   eval      WTGTFORA=0800
052500161122     c                   endif
052600150917     c                   z-add     9             zz
052700150917     c                   exsr      Wrifascia
052800150917     c
052900161122     c                   if        fasciasn=' '
053000150917     c                   eval      WTGTFORA=1400
053100150917     c                   add       1             zz
053200161122     c                   exsr      Wrifascia
053300161122     c                   endif
053400150917     c*
053500150917     c                   endsr
053600150917      *---------------------------------------------------------------*
053700150917     c     Wrifile       BEGSR
053800150917     c                   eval      WTGTTOT =tot(zz)
053900150917     c                   eval      WTGTbp  =pri(zz)
054000150921     c**                 eval      WTGT86  =disp(zz)
054100150921     c**                 eval      WTGTdir =dir(zz)
054200150921     c**                 eval      WTGTreso=reso(zz)
054300150918     c                   eval      WTGTgia =gia(zz)
054400150921     c*
054500150921     c     zz            occur     udat
054600150930     c                   eval      wtgtma1=  var(1)
054700150921     c                   eval      wtgtto1=totra(1)
054800150930     c                   eval      wtgtma2=  var(2)
054900150921     c                   eval      wtgtto2=totra(2)
055000150930     c                   eval      wtgtma3=  var(3)
055100150921     c                   eval      wtgtto3=totra(3)
055200150930     c                   eval      wtgtma4=  var(4)
055300150921     c                   eval      wtgtto4=totra(4)
055400150930     c                   eval      wtgtma5=  var(5)
055500150921     c                   eval      wtgtto5=totra(5)
055600150930     c                   eval      wtgtma6=  var(6)
055700150921     c                   eval      wtgtto6=totra(6)
055800150930     c                   eval      wtgtma7=  var(7)
055900150921     c                   eval      wtgtto7=totra(7)
056000150930     c                   eval      wtgtma8=  var(8)
056100150921     c                   eval      wtgtto8=totra(8)
056200150930     c                   eval      wtgtma9=  var(9)
056300150921     c                   eval      wtgtto9=totra(9)
056400150930     c                   eval      wtgtma10=  var(10)
056500150921     c                   eval      wtgtto10=totra(10)
056600150930     c                   eval      wtgtma11=  var(11)
056700150921     c                   eval      wtgtto11=totra(11)
056800150930     c                   eval      wtgtma12=  var(12)
056900150921     c                   eval      wtgtto12=totra(12)
057000150930     c                   eval      wtgtma13=  var(13)
057100150921     c                   eval      wtgtto13=totra(13)
057200150930     c                   eval      wtgtma14=  var(14)
057300150921     c                   eval      wtgtto14=totra(14)
057400150930     c                   eval      wtgtma15=  var(15)
057500150921     c                   eval      wtgtto15=totra(15)
057600150930     c                   eval      wtgtma16=  var(16)
057700150921     c                   eval      wtgtto16=totra(16)
057800150930     c                   eval      wtgtma17=  var(17)
057900150921     c                   eval      wtgtto17=totra(17)
058000150930     c                   eval      wtgtma18=  var(18)
058100150921     c                   eval      wtgtto18=totra(18)
058200150930     c                   eval      wtgtma19=  var(19)
058300150921     c                   eval      wtgtto19=totra(19)
058400150930     c                   eval      wtgtma20=  var(20)
058500150921     c                   eval      wtgtto20=totra(20)
058600150930     c                   eval      wtgtma21=  var(21)
058700150921     c                   eval      wtgtto21=totra(21)
058800150930     c                   eval      wtgtma22=  var(22)
058900150921     c                   eval      wtgtto22=totra(22)
059000150930     c                   eval      wtgtma23=  var(23)
059100150921     c                   eval      wtgtto23=totra(23)
059200150930     c                   eval      wtgtma24=  var(24)
059300150921     c                   eval      wtgtto24=totra(24)
059400150930     c                   eval      wtgtma25=  var(25)
059500150921     c                   eval      wtgtto25=totra(25)
059600150930     c                   eval      wtgtma26=  var(26)
059700150921     c                   eval      wtgtto26=totra(26)
059800150930     c                   eval      wtgtma27=  var(27)
059900150921     c                   eval      wtgtto27=totra(27)
060000150930     c                   eval      wtgtma28=  var(28)
060100150921     c                   eval      wtgtto28=totra(28)
060200150930     c                   eval      wtgtma29=  var(29)
060300150921     c                   eval      wtgtto29=totra(29)
060400150930     c                   eval      wtgtma30=  var(30)
060500150921     c                   eval      wtgtto30=totra(30)
060600150930     c                   eval      wtgtma31=  var(31)
060700150921     c                   eval      wtgtto31=totra(31)
060800150917     c                   write     wftgt000
060900150917     c                   ENDSR
061000150917      *---------------------------------------------------------------*
061100150917     c     Wrifascia     BEGSR
061200150917     c                   eval      WTGTFASN='S'
061300161122     c                   if        mic=' '
061400150917     c                   eval      WTGTTCON='N'
061500161122     c                   else
061600161122     c                   eval      WTGTTmic='N'
061700161122     c                   endif
061800150917     c                   exsr      wrifile
061900150917     c
062000161122     c                   if        mic=' '
062100150917     c                   eval      WTGTTCON='S'
062200161122     c                   else
062300161122     c                   eval      WTGTTmic='S'
062400161122     c                   endif
062500150917     c                   add       1             ZZ
062600161122     c
062700161122     c                   if        mic<>'Y'
062800150917     c                   exsr      wrifile
062900161122     c                   endif
063000150917     c
063100150917     c                   eval      WTGTFASN='N'
063200161122     c                   if        mic=' '
063300161122     c                   eval      WTGTTCON='N'
063400161122     c                   else
063500161122     c                   eval      WTGTTmic='N'
063600161122     c                   endif
063700150917     c                   add       1             ZZ
063800150917     c                   exsr      wrifile
063900150917     c
064000150917
064100161122     c                   if        mic=' '
064200161122     c                   eval      WTGTTCON='S'
064300161122     c                   else
064400161122     c                   eval      WTGTTmic='S'
064500161122     c                   endif
064600150917     c                   add       1             ZZ
064700161122
064800161122     c                   if        mic<>'Y'
064900150917     c                   exsr      wrifile
065000161122     c                   endif
065100150917     c                   ENDSR
065200150923      *---------------------------------------------------------------*
065300150923     c     contacons     BEGSR
065400150923     c                   if        fil>0
065500150923     c                   eval      orgfil=fil
065600150923     c                   exsr      leggiblp
065700150923     c                   else
065800150923     c     *loval        setll     azorg01l
065900150923     c                   read      azorg01l
066000150923     c                   dow       not %eof(azorg01l)
066100150923     c                   if        orgfag='F' or orgfag='A'
066200150923     c                   if        orgfva=' '
066300150923     c                   exsr      leggiblp
066400150923     c                   endif
066500150923     c                   endif
066600150923     c                   read      azorg01l
066700150923     c                   enddo
066800150923     c                   endif
066900150923     c
067000150923     c
067100150923     c* scrivo i dati nel file
067200150923     c                   clear                   wftgt000
067300150923     c                   eval      WATDTADA=datada
067400150923     c                   eval      WATDTAa =dataa
067500150923     c                   eval      WTGTFASN='N'
067600150923     c                   eval      WTGTTCON='N'
067700150923     c                   eval      wtgttot=nono
067800150923     c                   write     wftgt000
067900150923     c
068000150923     c                   clear                   wftgt000
068100150923     c                   eval      WATDTADA=datada
068200150923     c                   eval      WATDTAa =dataa
068300150923     c                   eval      WTGTFASN='N'
068400150923     c                   eval      WTGTTCON='S'
068500150923     c                   eval      wtgttot=nosi
068600150923     c                   write     wftgt000
068700150923     c
068800150923     c                   clear                   wftgt000
068900150923     c                   eval      WATDTADA=datada
069000150923     c                   eval      WATDTAa =dataa
069100150923     c                   eval      WTGTFASN='S'
069200150923     c                   eval      WTGTTCON='N'
069300150923     c                   eval      wtgttot=sino
069400150923     c                   write     wftgt000
069500150923
069600150923     c                   clear                   wftgt000
069700150923     c                   eval      WATDTADA=datada
069800150923     c                   eval      WATDTAa =dataa
069900150923     c                   eval      WTGTFASN='S'
070000150923     c                   eval      WTGTTCON='S'
070100150923     c                   eval      wtgttot=sisi
070200150923     c                   write     wftgt000
070300150923
070400150923     c                   ENDSR
070500150923      *---------------------------------------------------------------*
070600150923     c     Leggiblp      BEGSR
070700150923     c                   movel     datada        kaas
070800150923     c                   move      datada        kmgs
070900150923     c     kblp          setll     fnblp31l
071000150923     c     orgfil        reade     fnblp31l
071100150923    1c                   dow       not %eof(fnblp31l)
071200150923     c                   z-add     blpmgs        wdata
071300150923     c                   movel     blpaas        wdata
071400150923    2c                   if        wdata>dataa
071500150923     c                   leave
071600150923    2c                   endif
071700150923     c                   if        blpnzd='  ' and blpctm<>'4 ' and blpctm<>'4F'
071800150923     c                   if        priv =' ' or  %subst(blpgva:1:1)= 'P'
071900150923     c                   if        blpcbo='1 ' or blpcbo='2 ' or blpcbo='4 '
072000150923     c                             or blpcbo='5 ' or blpcbo='6 '
072100150923     c* Verifico se ha alert
072200150923     c                   eval      wann=blpaas
072300150923     c                   eval      wlnp=blplnp
072400150923     c                   eval      wnrs=blpnrs
072500150923     c                   eval      wnsp=blpnsp
072600150923     c     kar5          chain     fiar501l
072700150923    2c                   if        not %found(fiar501l)
072800150923     c                   eval      tal='N'
072900150923   x2c                   else
073000150923     c                   movel     ar5uni        dar5emd
073100150923    3c                   if        §AR5smp='S'         or §ar5mmp='S'
073200150923     c                   eval      tal='S'
073300150923     c                   else
073400150923     c                   eval      tal='N'
073500150923    3c                   endif
073600150923    2c                   endif
073700150923     c*
073800150923     c* verifico se ha un tentativo di consegna
073900150923     c                   eval      primo='N'
074000150923     c     kbolla        setll     fnevb01l
074100150923     c     kbolla        reade     fnevb01l
074200150923    2c                   dow       not %eof(fnevb01l)
074300150923    3c                   if        %lookup(evbcev:manc)>0
074400150923     c                   eval      primo='S'
074500150923     c                   leave
074600150923    3c                   endif
074700150923     c
074800150923     c     kbolla        reade     fnevb01l
074900150923    2c                   enddo
075000150923    2c                   select
075100150923     c                   when      tal='N' and primo='N'
075200150923     c                   add       1             nono              7 0
075300150923     c                   when      tal='N' and primo='S'
075400150923     c                   add       1             nosi              7 0
075500150923     c                   when      tal='S' and primo='N'
075600150923     c                   add       1             sino              7 0
075700150923     c                   when      tal='S' and primo='S'
075800150923     c                   add       1             sisi              7 0
075900150923    2c                   endsl
076000150923     c                   endif
076100150923     c                   endif
076200150923     c                   endif
076300150923
076400150923     c     orgfil        reade     fnblp31l
076500150923    1c                   enddo
076600150923     c
076700150923     c                   ENDSR
076800150917     c
076900150917     OQSYSPRT   E            err         1
077000150917     O                                              'R.A. di bolle non trovate'
077100150917     o                       nobolla       2  +   1
077200161124     Oqsysprt   E            detf        1
077300161124     O                       arbaas        z  +   1
077400161124     O                       arblnp        z  +   1
077500161124     O                       arbnrs        z  +   1
077600161124     O                       arbnsp        z  +   1
077700161124     O                       arbdcm        z  +   1
077800161124     O                       tal              +   1
077900161124     O                       dopomic          +   1
078000161124     O                       wcvb             +   1
078100161124     o
