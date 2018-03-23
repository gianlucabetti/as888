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
005600150917     D                 DS
005700150917     D  wlnp                   1      3  0
005800150917     D  wnrs                   4      5  0
005900150917     D  wnsp                   6     12  0
006000150917     D  wann                  13     16  0
006100150917     D  wbolla                 1     16
006200150917     d
006300150918     d wdata           s              8  0
006400150918     d tal             s              1
006500150917     d primo           s              1
006600150930     d variaz          s              1
006700150930     d wcvb            s                   like(arbcvb)
006800150930     d nummin          s              7  0
006900150918     d wtitas          s             21    inz('GAITRAGRU /TITAS30C')
007000041230      *
007100150917     c     *entry        plist
007200150917     c                   parm                    kpjba
007300150917     c                   movel     kpjbu         param
007400150918     c* pulisco il file
007500150918      /FREE
007600150918       exec sql delete from WFTGT00F    ;
007700150918      /END-FREE
007800150917     c* Apertura file
007900150917     c                   if        %subst( knsif : 7 : 1) = 'P'
008000150917     c                   eval      %subst( wtitas : 7 : 4 ) = 'GRPS'
008100150917     c                   endif
008200150917     c
008300150917     c                   open      titas30c
008400150917     c
008500150917     c                   if        fil>0
008600150917     c                   eval      orgfil=fil
008700150917     c                   exsr      leggiTGD
008800150917     c                   else
008900150917     c     *loval        setll     azorg01l
009000150917     c                   read      azorg01l
009100150917     c                   dow       not %eof(azorg01l)
009200150917     c                   if        orgfag='F' or orgfag='A'
009300150917     c                   if        orgfva=' '
009400150917     c                   exsr      leggiTGD
009500150917     c                   endif
009600150917     c                   endif
009700150917     c                   read      azorg01l
009800150917     c                   enddo
009900150917     c                   endif
010000150917     c
010100150917     c*   riempo il file
010200150917     c                   exsr      scrivi
010300150923     c* conto le bolle che  hanno un tentativo di consegna
010400150923     c                   exsr      contacons
010500130415      *
010600150917     c                   except    err
010700041231     c                   seton                                        lr
010800041230      *
010900041231      *---------------------------------------------------------------*
011000041231      * Routine Iniziale                                              *
011100041231      *---------------------------------------------------------------*
011200041231     c     *InzSR        BegSR
011300041231      *
011400041231      * Reperisco dati job
011500041231     c                   exsr      DatiJob
011600150917     c
011700150917     c     ktgd          klist
011800150917     c                   kfld                    orgfil
011900150917     c                   kfld                    kdac              8 0
012000150923     c     kblp          klist
012100150923     c                   kfld                    orgfil
012200150923     c                   kfld                    kaas              4 0
012300150923     c                   kfld                    kmgs              4 0
012400150917     c     kbolla        klist
012500150917     c                   kfld                    wann
012600150917     c                   kfld                    wlnp
012700150917     c                   kfld                    wnrs
012800150917     c                   kfld                    wnsp
012900150917     c     kar5          klist
013000150917     c                   kfld                    wann
013100150917     c                   kfld                    wlnp
013200150917     c                   kfld                    wnrs
013300150917     c                   kfld                    wnsp
013400150917     c                   kfld                    ktrc              3
013500150917     c                   eval      ktrc='EMD'
013600150917     c     ktgd32        klist
013700150917     c                   kfld                    tgdtor
013800150917     c                   kfld                    tgdogg
013900150917     c     ktab          klist
014000150917     c                   kfld                    codut             1 0
014100150917     c                   kfld                    kcod              2
014200150917     c                   z-add     1             codut
014300150930     c     kTGN          klist
014400150930     c                   kfld                    tgdant
014500150930     c                   kfld                    tgdnut
014600150917     c
014700150917     c* Carico gli eventi di 1a mancata consegna
014800150917     c                   eval      kcod='2A'
014900150917     c                   eval      xx=1
015000150917     c     ktab          setll     tabel00f
015100150917     c     ktab          reade     tabel00f
015200150917     c                   dow       not %eof(tabel00f)
015300150917     c                   if        tblflg=' '
015400150917     c                   movel     tbluni        ds2a
015500150917     c                   if        §2aftc<>' '
015600150917     c                   movel     tblkey        manc(xx)
015700150917     c                   add       1             xx                3 0
015800150917     c                   endif
015900150917     c                   endif
016000150917     c
016100150917     c     ktab          reade     tabel00f
016200150917     c                   enddo
016300150917     c
016400041231     C                   ENDSR
016500041231      *
016600041231      *---------------------------------------------------------------*
016700041231      * Reperimento Dati del job (Utente/Operativi)                   *
016800041231      *---------------------------------------------------------------*
016900041231     c     DatiJob       BegSr
017000041231      *
017100041231     c     *dtaara       define    §azute        azuteds
017200041231     c     *dtaara       define    §datiute      ddatiute
017300041231      *
017400041231     c                   in(E)     *dtaara
017500041231     c                   IF        %ERROR or RSUT = *blanks
017600041231     c                   clear                   Tibs34Ds
017700041231     c                   call      'TIBS34R'
017800041231     c                   parm                    Tibs34Ds
017900041231     c                   in        *dtaara
018000041231     c                   ENDIF
018100041231     C                   ENDSR
018200150917      *---------------------------------------------------------------*
018300150917      * lettura TGD
018400150917      *---------------------------------------------------------------*
018500150917     c     LeggiTGD      BegSr
018600150917     c                   eval      kdac=datada
018700150921     c     ktgd          setll     fitgd04l
018800150921     c     orgfil        reade     fitgd04l
018900150921     c                   dow       not %eof(fitgd04l)
019000150921     c                   if        tgddar>dataa
019100150917     c                   leave
019200150917     c                   endif
019300150917     c* elaboro solo M D C   e telefonate per  spedizione
019400150917     c                   if        tgdtco='T' and tgdtor='S' and tgdogg<>*blanks
019500150921     c***                if        tgdtch='D' or tgdtch='M' or tgdtch='C'
019600150921     c                   if        tgdtch='D' or tgdtch='M'
019700150917     c                   movel     tgdtch        ch                1
019800150917     c* fascia oraria
019900150917     c                   clear                   fascia            4 0
020000150921     c                   if        tgdorr<140000
020100150917     c                   eval      fascia=0800
020200150917     c                   else
020300150917     c                   eval      fascia=1400
020400150917     c                   endif
020500150917     c
020600150917     c                   exsr      elabolla
020700150917     c                   endif
020800150917     c                   endif
020900150917     c
021000150921     c     orgfil        reade     fitgd04l
021100150917     c                   enddo
021200150917     c
021300150917     c                   endsr
021400150917      *---------------------------------------------------------------*
021500150917      * lettura bolle
021600150917      *---------------------------------------------------------------*
021700150917     c     elabolla      BegSr
021800150921     c                   movel     tgdorr        wora              4 0
021900150917     c                   movel     tgdogg        wbolla
022000150917     c                   movel     'F'           tipob
022100150930     c                   eval      variaz='N'
022200150930     c                   clear                   note            256
022300150930     c                   clear                   dirot             1
022400150930     c                   clear                   post              1
022500150917     c
022600150917     c     kbolla        chain     fnarb01l
022700150917     c                   if        not %found(fnarb01l)
022800150917     c     kbolla        chain     fnblp01l
022900150917     c                   if        not %found(fnblp01l)
023000150917     c     kbolla        chain     titas30c
023100150917     c                   if        not %found(titas30c)
023200150917     c                   add       1             nobolla           7 0
023300150917     c                   leavesr
023400150917     c                   else
023500150917     c                   movel     'T'           tipob             1
023600150922     c                   movel     tasgva        arbgva
023700150917     c                   movel     tascca        arbcca
023800150917     c                   movel     tashmc        arbhmc
023900150917     c                   endif
024000150917     c                   endif
024100150917     c                   endif
024200150923     c
024300150923     c* se ho chieso solo spedizioni ai rpivati, escluso quelle senza "P"
024400150923     c                   if        priv ='P' and %subst(arbgva:1:1)<>'P'
024500150923     c                   leavesr
024600150923     c                   endif
024700150917     c
024800150917     c* possibilità di alert
024900150917    1c                   if        tipob='F'
025000150917     c     kar5          chain     fiar501l
025100150917    2c                   if        not %found(fiar501l)
025200150917     c                   eval      tal='N'
025300150917   x2c                   else
025400150917     c                   movel     ar5uni        dar5emd
025500150921    3c                   if        §AR5smp='S'         or §ar5mmp='S'
025600150917     c                   eval      tal='S'
025700150917     c                   else
025800150917     c                   eval      tal='N'
025900150917    3c                   endif
026000150917    2c                   endif
026100150917     c
026200150917   x1c                   else
026300150917     c                   movel     tasflo        dtasflo
026400150917    2c                   if        §floemd='S'
026500150917     c                   eval      tal='S'
026600150917     c                   else
026700150917     c                   eval      tal='N'
026800150917    2c                   endif
026900150917    1c                   endif
027000150929     c
027100150929     c* Se voglio solo le bolle con ALERT escludo se manca
027200150929     c                   if        alert=' ' or (alert='S' and tal='S') or
027300150929     c                             (alert='N' and tal='N')
027400150918     c
027500150917     c* verifico se la bolla ha evento di consegna prima o dopo questa r.a.
027600150917     c                   eval      primo='N'
027700150923     c     kbolla        setll     fnevb01l
027800150923     c     kbolla        reade     fnevb01l
027900150923     c                   dow       not %eof(fnevb01l)
028000150917     c                   if        %lookup(evbcev:manc)>0
028100150917     c* Verifico l'orario
028200150921     c                   if        tgddar>evbdev
028300150918     c                   eval      primo='S'
028400150918     c                   else
028500150921     c                   if        tgddar=evbdev and wora  >evbhev
028600150917     c                   eval      primo='S'
028700150917     c                   endif
028800150917     c                   endif
028900150918     c
029000150918     c                   leave
029100150918     c                   endif
029200150917     c
029300150923     c     kbolla        reade     fnevb01l
029400150917     c                   enddo
029500150917     c* indice schiera
029600150917     c                   clear                   ZZ                3 0
029700150917     c                   if        ch='M'
029800150917     c                   z-add     1             zz
029900150917     c                   endif
030000150917     c                   if        ch='D'
030100150917     c                   z-add     9             zz
030200150917     c                   endif
030300150917     c                   if        ch='C'
030400150917     c                   z-add     17            zz
030500150917     c                   endif
030600150917     c* fascia
030700150917     c                   if        fascia=1400
030800150917     c                   add       4             zz
030900150917     c                   endif
031000150917     c* presenza alert
031100150917     c                   if        tal='N'
031200150917     c                   add       2             zz
031300150917     c                   endif
031400150917     c* prima o dopo 1°tent.consegna
031500150917     c                   if        primo='S'
031600150917     c                   add       1             zz
031700150917     c                   endif
031800150917     c* totale
031900150917     c                   add       1             tot(ZZ)
032000150917     c* privato
032100150922     c                   if        %subst(arbgva:1:1)='P'
032200150917     c                   add       1             pri(ZZ)
032300150917     c                   endif
032400150918     c* con giacenza
032500150918     c     kbolla        chain     tigcp01l
032600150918     c                   if        %found(tigcp01l)
032700150918     c                   z-add     gcpmgc        wdata
032800150918     c                   movel     gcpagc        wdata
032900150918     c                   if        tgddac> wdata
033000150918     c                   add       1             gia(zz)
033100150918     c                   endif
033200150918     c                   endif
033300150930     c
033400150930     c* Verifico se ha prodotto una variazione
033500150930     c     kbolla        setll     fiarbf2c
033600150930     c     kbolla        reade     fiarbf2c
033700150930     c                   dow       not %eof(fiarbf2c)
033800150930     c                   if        arbcvb='I0' or arbcvb='I7' or
033900150930     c                             arbcvb='NT' or
034000150930     c                             %subst(arbcvb:1:1)='C' or
034100150930     c                             %subst(arbcvb:1:1)='K' or
034200150930     c                             %subst(arbcvb:1:1)='T'
034300150930     c                   if        arbdtv=tgddac
034400150930     c     *eur          movel     arborv        oravar
034500150930     c     *eur          movel     tgdorc        oratgd
034600150930     c                   if        arborv>tgdorc
034700150930     c                   eval      nummin=%diff(oravar:oratgd :*mn)
034800150930     c                   else
034900150930     c                   eval      nummin=%diff(oratgd:oravar :*mn)
035000150930     c                   endif
035100150930     c                   if        nummin<=5
035200150930     c                   eval      variaz='S'
035300150930     c                   leave
035400150930     c                   endif
035500150930     c                   endif
035600150930     c                   endif
035700150930     c
035800150930     c     kbolla        reade     fiarbf2c
035900150930     c                   enddo
036000150930     c
036100150930     c* Se non ha variazione verifico alcune note fisse
036200150930     c                   if        variaz='N'
036300150930     c                   z-add     1             conta             2 0
036400150930     c     ktgn          setll     fitgn01l
036500150930     c     ktgn          reade     fitgn01l
036600150930     c                   dow       not %eof(fitgn01l) and conta<=7
036700150930     c                   eval      note=%trim(note)+tgnnot
036800150930     c                   add       1             conta
036900150930     c
037000150930     c     ktgn          reade     fitgn01l
037100150930     c                   enddo
037200150930     c
037300150930     c                   select
037400150930     c                   when      %scan('Indirizzo Alternativo':note)>0
037500150930     c                   eval      dirot ='S'
037600150930     c                   when      %scan('DataConsegnaRichiesta':note)>0
037700150930     c                   eval      post  ='S'
037800150930     c                   endsl
037900150930
038000150930     c                   else
038100150930     c* Raggruppo le causali variazioni
038200150930     c                   eval      wcvb=arbcvb
038300150930     c                   if        arbcvb='I7'
038400150930     c                   eval      wcvb='I0'
038500150930     c                   endif
038600150930     c                   if        %subst(wcvb:1:1)='C' or
038700150930     c                             %subst(wcvb:1:1)='K' or
038800150930     c                             %subst(wcvb:1:1)='T'
038900150930     c                   eval      %subst(wcvb:2:1)=' '
039000150930     c                   endif
039100150930     c                   endif
039200150921     c
039300150930     c                   if        variaz='N' and dirot=' ' and post=' '
039400150930     c                   z-add     1             ra
039500150930     c                   eval      tvar(1)='NO'
039600150930     c                   eval      wcvb   ='NO'
039700150930     c                   endif
039800150930     c                   if        variaz='N' and dirot='S' and post=' '
039900150930     c                   z-add     2             ra
040000150930     c                   eval      tvar(2)='DI'
040100150930     c                   eval      wcvb   ='DI'
040200150930     c                   endif
040300150930     c                   if        variaz='N' and dirot=' ' and post='S'
040400150930     c                   z-add     3             ra
040500150930     c                   eval      tvar(3)='PO'
040600150930     c                   eval      wcvb   ='PO'
040700150930     c                   endif
040800150930     c
040900150930     c
041000150930     c                   if        variaz='S'
041100150930     c                   z-add     4             ra                3 0
041200150930     c                   eval      ra=%lookup(wcvb:tvar:4)
041300150921     c                   if        ra=0
041400150930     c                   eval      ra=%lookup('   ':tvar:4)
041500150921     c                   if        ra>0 and ra<=30
041600150930     c                   eval      tvar(ra)=wcvb
041700150921     c                   else
041800150921     c                   eval      ra=31
041900150930     c                   eval      tvar(31)='OL'
042000150921     c                   endif
042100150921     c                   endif
042200150930
042300150930     c                   endif
042400150921     c
042500150921     C     ZZ            OCCUR     UDAT
042600150921    1c                   if        ra=31
042700150930    2c                   if        var(31)=*blanks
042800150930     c                   eval      var(31)='OL '
042900150921     c                   z-add     1             totra(31)
043000150921     c                   else
043100150921     c                   add       1             totra(31)
043200150921    2c                   endif
043300150921     c
043400150921   x1c                   else
043500150930    2c                   if        var(ra)=*blanks
043600150930     c                   eval      var(ra)=wcvb
043700150921     c                   z-add     1             totra(ra)
043800150921     c                   else
043900150921     c                   add       1             totra(ra)
044000150921    2c                   endif
044100150921    1c                   endif
044200150929     c
044300150929    1c                   endif
044400150917     c
044500150917     c                   endsr
044600150917      *---------------------------------------------------------------*
044700150917      * scrittura file
044800150917      *---------------------------------------------------------------*
044900150917     c     scrivi        BegSr
045000150917     c*
045100150917     c                   clear                   wftgt000
045200150917     c                   eval      WATDTADA=datada
045300150917     c                   eval      WATDTAa =dataa
045400150917     c                   eval      WTGTTCH ='M'
045500150917     c                   eval      WTGTFORA=0800
045600150917     c                   z-add     1             zz
045700150917     c                   exsr      Wrifascia
045800150917     c
045900150917     c                   eval      WTGTFORA=1400
046000150917     c                   add       1             zz
046100150917     c                   exsr      Wrifascia
046200150917     c
046300150917     c*
046400150917     c                   clear                   wftgt000
046500150917     c                   eval      WATDTADA=datada
046600150917     c                   eval      WATDTAa =dataa
046700150917     c                   eval      WTGTTCH ='D'
046800150917     c                   eval      WTGTFORA=0800
046900150917     c                   z-add     9             zz
047000150917     c                   exsr      Wrifascia
047100150917     c
047200150917     c                   eval      WTGTFORA=1400
047300150917     c                   add       1             zz
047400150917     c                   exsr      Wrifascia
047500150917     c*
047600150921     c**                 clear                   wftgt000
047700150921     c**                 eval      WATDTADA=datada
047800150921     c**                 eval      WATDTAa =dataa
047900150921     c**                 eval      WTGTTCH ='C'
048000150921     c**                 eval      WTGTFORA=0800
048100150921     c**                 z-add     17            zz
048200150921     c**                 exsr      Wrifascia
048300150917     c
048400150921     c**                 eval      WTGTFORA=1400
048500150921     c**                 add       1             zz
048600150921     c**                 exsr      Wrifascia
048700150917     c
048800150917     c                   endsr
048900150917      *---------------------------------------------------------------*
049000150917     c     Wrifile       BEGSR
049100150917     c                   eval      WTGTTOT =tot(zz)
049200150917     c                   eval      WTGTbp  =pri(zz)
049300150921     c**                 eval      WTGT86  =disp(zz)
049400150921     c**                 eval      WTGTdir =dir(zz)
049500150921     c**                 eval      WTGTreso=reso(zz)
049600150918     c                   eval      WTGTgia =gia(zz)
049700150921     c*
049800150921     c     zz            occur     udat
049900150930     c                   eval      wtgtma1=  var(1)
050000150921     c                   eval      wtgtto1=totra(1)
050100150930     c                   eval      wtgtma2=  var(2)
050200150921     c                   eval      wtgtto2=totra(2)
050300150930     c                   eval      wtgtma3=  var(3)
050400150921     c                   eval      wtgtto3=totra(3)
050500150930     c                   eval      wtgtma4=  var(4)
050600150921     c                   eval      wtgtto4=totra(4)
050700150930     c                   eval      wtgtma5=  var(5)
050800150921     c                   eval      wtgtto5=totra(5)
050900150930     c                   eval      wtgtma6=  var(6)
051000150921     c                   eval      wtgtto6=totra(6)
051100150930     c                   eval      wtgtma7=  var(7)
051200150921     c                   eval      wtgtto7=totra(7)
051300150930     c                   eval      wtgtma8=  var(8)
051400150921     c                   eval      wtgtto8=totra(8)
051500150930     c                   eval      wtgtma9=  var(9)
051600150921     c                   eval      wtgtto9=totra(9)
051700150930     c                   eval      wtgtma10=  var(10)
051800150921     c                   eval      wtgtto10=totra(10)
051900150930     c                   eval      wtgtma11=  var(11)
052000150921     c                   eval      wtgtto11=totra(11)
052100150930     c                   eval      wtgtma12=  var(12)
052200150921     c                   eval      wtgtto12=totra(12)
052300150930     c                   eval      wtgtma13=  var(13)
052400150921     c                   eval      wtgtto13=totra(13)
052500150930     c                   eval      wtgtma14=  var(14)
052600150921     c                   eval      wtgtto14=totra(14)
052700150930     c                   eval      wtgtma15=  var(15)
052800150921     c                   eval      wtgtto15=totra(15)
052900150930     c                   eval      wtgtma16=  var(16)
053000150921     c                   eval      wtgtto16=totra(16)
053100150930     c                   eval      wtgtma17=  var(17)
053200150921     c                   eval      wtgtto17=totra(17)
053300150930     c                   eval      wtgtma18=  var(18)
053400150921     c                   eval      wtgtto18=totra(18)
053500150930     c                   eval      wtgtma19=  var(19)
053600150921     c                   eval      wtgtto19=totra(19)
053700150930     c                   eval      wtgtma20=  var(20)
053800150921     c                   eval      wtgtto20=totra(20)
053900150930     c                   eval      wtgtma21=  var(21)
054000150921     c                   eval      wtgtto21=totra(21)
054100150930     c                   eval      wtgtma22=  var(22)
054200150921     c                   eval      wtgtto22=totra(22)
054300150930     c                   eval      wtgtma23=  var(23)
054400150921     c                   eval      wtgtto23=totra(23)
054500150930     c                   eval      wtgtma24=  var(24)
054600150921     c                   eval      wtgtto24=totra(24)
054700150930     c                   eval      wtgtma25=  var(25)
054800150921     c                   eval      wtgtto25=totra(25)
054900150930     c                   eval      wtgtma26=  var(26)
055000150921     c                   eval      wtgtto26=totra(26)
055100150930     c                   eval      wtgtma27=  var(27)
055200150921     c                   eval      wtgtto27=totra(27)
055300150930     c                   eval      wtgtma28=  var(28)
055400150921     c                   eval      wtgtto28=totra(28)
055500150930     c                   eval      wtgtma29=  var(29)
055600150921     c                   eval      wtgtto29=totra(29)
055700150930     c                   eval      wtgtma30=  var(30)
055800150921     c                   eval      wtgtto30=totra(30)
055900150930     c                   eval      wtgtma31=  var(31)
056000150921     c                   eval      wtgtto31=totra(31)
056100150917     c                   write     wftgt000
056200150917     c                   ENDSR
056300150917      *---------------------------------------------------------------*
056400150917     c     Wrifascia     BEGSR
056500150917     c                   eval      WTGTFASN='S'
056600150917     c                   eval      WTGTTCON='N'
056700150917     c                   exsr      wrifile
056800150917     c
056900150917     c                   eval      WTGTTCON='S'
057000150917     c                   add       1             ZZ
057100150917     c                   exsr      wrifile
057200150917     c
057300150917     c                   eval      WTGTFASN='N'
057400150917     c                   eval      WTGTTCON='N'
057500150917     c                   add       1             ZZ
057600150917     c                   exsr      wrifile
057700150917     c
057800150917
057900150917     c                   eval      WTGTTCON='S'
058000150917     c                   add       1             ZZ
058100150917     c                   exsr      wrifile
058200150917     c                   ENDSR
058300150923      *---------------------------------------------------------------*
058400150923     c     contacons     BEGSR
058500150923     c                   if        fil>0
058600150923     c                   eval      orgfil=fil
058700150923     c                   exsr      leggiblp
058800150923     c                   else
058900150923     c     *loval        setll     azorg01l
059000150923     c                   read      azorg01l
059100150923     c                   dow       not %eof(azorg01l)
059200150923     c                   if        orgfag='F' or orgfag='A'
059300150923     c                   if        orgfva=' '
059400150923     c                   exsr      leggiblp
059500150923     c                   endif
059600150923     c                   endif
059700150923     c                   read      azorg01l
059800150923     c                   enddo
059900150923     c                   endif
060000150923     c
060100150923     c
060200150923     c* scrivo i dati nel file
060300150923     c                   clear                   wftgt000
060400150923     c                   eval      WATDTADA=datada
060500150923     c                   eval      WATDTAa =dataa
060600150923     c                   eval      WTGTFASN='N'
060700150923     c                   eval      WTGTTCON='N'
060800150923     c                   eval      wtgttot=nono
060900150923     c                   write     wftgt000
061000150923     c
061100150923     c                   clear                   wftgt000
061200150923     c                   eval      WATDTADA=datada
061300150923     c                   eval      WATDTAa =dataa
061400150923     c                   eval      WTGTFASN='N'
061500150923     c                   eval      WTGTTCON='S'
061600150923     c                   eval      wtgttot=nosi
061700150923     c                   write     wftgt000
061800150923     c
061900150923     c                   clear                   wftgt000
062000150923     c                   eval      WATDTADA=datada
062100150923     c                   eval      WATDTAa =dataa
062200150923     c                   eval      WTGTFASN='S'
062300150923     c                   eval      WTGTTCON='N'
062400150923     c                   eval      wtgttot=sino
062500150923     c                   write     wftgt000
062600150923
062700150923     c                   clear                   wftgt000
062800150923     c                   eval      WATDTADA=datada
062900150923     c                   eval      WATDTAa =dataa
063000150923     c                   eval      WTGTFASN='S'
063100150923     c                   eval      WTGTTCON='S'
063200150923     c                   eval      wtgttot=sisi
063300150923     c                   write     wftgt000
063400150923
063500150923     c                   ENDSR
063600150923      *---------------------------------------------------------------*
063700150923     c     Leggiblp      BEGSR
063800150923     c                   movel     datada        kaas
063900150923     c                   move      datada        kmgs
064000150923     c     kblp          setll     fnblp31l
064100150923     c     orgfil        reade     fnblp31l
064200150923    1c                   dow       not %eof(fnblp31l)
064300150923     c                   z-add     blpmgs        wdata
064400150923     c                   movel     blpaas        wdata
064500150923    2c                   if        wdata>dataa
064600150923     c                   leave
064700150923    2c                   endif
064800150923     c                   if        blpnzd='  ' and blpctm<>'4 ' and blpctm<>'4F'
064900150923     c                   if        priv =' ' or  %subst(blpgva:1:1)= 'P'
065000150923     c                   if        blpcbo='1 ' or blpcbo='2 ' or blpcbo='4 '
065100150923     c                             or blpcbo='5 ' or blpcbo='6 '
065200150923     c* Verifico se ha alert
065300150923     c                   eval      wann=blpaas
065400150923     c                   eval      wlnp=blplnp
065500150923     c                   eval      wnrs=blpnrs
065600150923     c                   eval      wnsp=blpnsp
065700150923     c     kar5          chain     fiar501l
065800150923    2c                   if        not %found(fiar501l)
065900150923     c                   eval      tal='N'
066000150923   x2c                   else
066100150923     c                   movel     ar5uni        dar5emd
066200150923    3c                   if        §AR5smp='S'         or §ar5mmp='S'
066300150923     c                   eval      tal='S'
066400150923     c                   else
066500150923     c                   eval      tal='N'
066600150923    3c                   endif
066700150923    2c                   endif
066800150923     c*
066900150923     c* verifico se ha un tentativo di consegna
067000150923     c                   eval      primo='N'
067100150923     c     kbolla        setll     fnevb01l
067200150923     c     kbolla        reade     fnevb01l
067300150923    2c                   dow       not %eof(fnevb01l)
067400150923    3c                   if        %lookup(evbcev:manc)>0
067500150923     c                   eval      primo='S'
067600150923     c                   leave
067700150923    3c                   endif
067800150923     c
067900150923     c     kbolla        reade     fnevb01l
068000150923    2c                   enddo
068100150923    2c                   select
068200150923     c                   when      tal='N' and primo='N'
068300150923     c                   add       1             nono              7 0
068400150923     c                   when      tal='N' and primo='S'
068500150923     c                   add       1             nosi              7 0
068600150923     c                   when      tal='S' and primo='N'
068700150923     c                   add       1             sino              7 0
068800150923     c                   when      tal='S' and primo='S'
068900150923     c                   add       1             sisi              7 0
069000150923    2c                   endsl
069100150923     c                   endif
069200150923     c                   endif
069300150923     c                   endif
069400150923
069500150923     c     orgfil        reade     fnblp31l
069600150923    1c                   enddo
069700150923     c
069800150923     c                   ENDSR
069900150917     c
070000150917     OQSYSPRT   E            err         1
070100150917     O                                              'R.A. di bolle non trovate'
070200150917     o                       nobolla       2  +   1
