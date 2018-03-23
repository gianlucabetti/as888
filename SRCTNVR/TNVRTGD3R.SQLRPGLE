000100041230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200130415      *----------------------------------------------------------------*
000300150917      * Statistica reclami bolle
000400130415      *----------------------------------------------------------------*
000500150917     ftabel00f  if   e           k disk
000600150917     fazorg01l  if   e           k disk
000700150921     fFITGD04l  if   e           k disk
000800150917     fFITGD32l  if   e           k disk    rename(fitgd000:fitgd032)
000900150917     f                                     prefix(r_)
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
002800150921     d tmad            s              3    dim(31)
002900150921
003000150921     D UDAT            DS                  OCCURS(16)  INZ
003100150921     D  mad                    1     93
003200150921     D                                     DIM(31)
003300150921     D  totra                 94    217P 0
003400150921     D                                     DIM(31)
003500041230      *
003600041231      * - Parametri x Controllo profilo utenti
003700041231     d TIBS34DS      e ds                  inz
003800041231      * - Ds di riferimento al file esterno AZUTE00F
003900041231     d AZUTEDS       e ds                  extname(AZUTE00F)
004000041231      * - Ds per dati organigramma
004100041231     d DDatiUte      e ds
004200041230      *
004300041231     d KPJBA         e ds
004400150917     d dtasflo       e ds
004500150917     d dar5emd       e ds
004600150917     d ds2a          e ds
004700150917     d
004800150917     D param           DS
004900150917     D  datada                 1      8  0
005000150917     D  dataa                  9     16  0
005100150917     D  fil                   17     19  0
005200150923     D  priv                  20     20
005300150917     D                 DS
005400150917     D  wlnp                   1      3  0
005500150917     D  wnrs                   4      5  0
005600150917     D  wnsp                   6     12  0
005700150917     D  wann                  13     16  0
005800150917     D  wbolla                 1     16
005900150917     d
006000150918     d wdata           s              8  0
006100150918     d tal             s              1
006200150917     d primo           s              1
006300150918     d wtitas          s             21    inz('GAITRAGRU /TITAS30C')
006400041230      *
006500150917     c     *entry        plist
006600150917     c                   parm                    kpjba
006700150917     c                   movel     kpjbu         param
006800150918     c* pulisco il file
006900150918      /FREE
007000150918       exec sql delete from WFTGT00F    ;
007100150918      /END-FREE
007200150917     c* Apertura file
007300150917     c                   if        %subst( knsif : 7 : 1) = 'P'
007400150917     c                   eval      %subst( wtitas : 7 : 4 ) = 'GRPS'
007500150917     c                   endif
007600150917     c
007700150917     c                   open      titas30c
007800150917     c
007900150917     c                   if        fil>0
008000150917     c                   eval      orgfil=fil
008100150917     c                   exsr      leggiTGD
008200150917     c                   else
008300150917     c     *loval        setll     azorg01l
008400150917     c                   read      azorg01l
008500150917     c                   dow       not %eof(azorg01l)
008600150917     c                   if        orgfag='F' or orgfag='A'
008700150917     c                   if        orgfva=' '
008800150917     c                   exsr      leggiTGD
008900150917     c                   endif
009000150917     c                   endif
009100150917     c                   read      azorg01l
009200150917     c                   enddo
009300150917     c                   endif
009400150917     c
009500150917     c*   riempo il file
009600150917     c                   exsr      scrivi
009700150923     c* conto le bolle che  hanno un tentativo di consegna
009800150923     c                   exsr      contacons
009900130415      *
010000150917     c                   except    err
010100041231     c                   seton                                        lr
010200041230      *
010300041231      *---------------------------------------------------------------*
010400041231      * Routine Iniziale                                              *
010500041231      *---------------------------------------------------------------*
010600041231     c     *InzSR        BegSR
010700041231      *
010800041231      * Reperisco dati job
010900041231     c                   exsr      DatiJob
011000150917     c
011100150917     c     ktgd          klist
011200150917     c                   kfld                    orgfil
011300150917     c                   kfld                    kdac              8 0
011400150923     c     kblp          klist
011500150923     c                   kfld                    orgfil
011600150923     c                   kfld                    kaas              4 0
011700150923     c                   kfld                    kmgs              4 0
011800150917     c     kbolla        klist
011900150917     c                   kfld                    wann
012000150917     c                   kfld                    wlnp
012100150917     c                   kfld                    wnrs
012200150917     c                   kfld                    wnsp
012300150917     c     kar5          klist
012400150917     c                   kfld                    wann
012500150917     c                   kfld                    wlnp
012600150917     c                   kfld                    wnrs
012700150917     c                   kfld                    wnsp
012800150917     c                   kfld                    ktrc              3
012900150917     c                   eval      ktrc='EMD'
013000150917     c     ktgd32        klist
013100150917     c                   kfld                    tgdtor
013200150917     c                   kfld                    tgdogg
013300150917     c     ktab          klist
013400150917     c                   kfld                    codut             1 0
013500150917     c                   kfld                    kcod              2
013600150917     c                   z-add     1             codut
013700150917     c
013800150917     c* Carico gli eventi di 1a mancata consegna
013900150917     c                   eval      kcod='2A'
014000150917     c                   eval      xx=1
014100150917     c     ktab          setll     tabel00f
014200150917     c     ktab          reade     tabel00f
014300150917     c                   dow       not %eof(tabel00f)
014400150917     c                   if        tblflg=' '
014500150917     c                   movel     tbluni        ds2a
014600150917     c                   if        §2aftc<>' '
014700150917     c                   movel     tblkey        manc(xx)
014800150917     c                   add       1             xx                3 0
014900150917     c                   endif
015000150917     c                   endif
015100150917     c
015200150917     c     ktab          reade     tabel00f
015300150917     c                   enddo
015400150917     c
015500041231     C                   ENDSR
015600041231      *
015700041231      *---------------------------------------------------------------*
015800041231      * Reperimento Dati del job (Utente/Operativi)                   *
015900041231      *---------------------------------------------------------------*
016000041231     c     DatiJob       BegSr
016100041231      *
016200041231     c     *dtaara       define    §azute        azuteds
016300041231     c     *dtaara       define    §datiute      ddatiute
016400041231      *
016500041231     c                   in(E)     *dtaara
016600041231     c                   IF        %ERROR or RSUT = *blanks
016700041231     c                   clear                   Tibs34Ds
016800041231     c                   call      'TIBS34R'
016900041231     c                   parm                    Tibs34Ds
017000041231     c                   in        *dtaara
017100041231     c                   ENDIF
017200041231     C                   ENDSR
017300150917      *---------------------------------------------------------------*
017400150917      * lettura TGD
017500150917      *---------------------------------------------------------------*
017600150917     c     LeggiTGD      BegSr
017700150917     c                   eval      kdac=datada
017800150921     c     ktgd          setll     fitgd04l
017900150921     c     orgfil        reade     fitgd04l
018000150921     c                   dow       not %eof(fitgd04l)
018100150921     c                   if        tgddar>dataa
018200150917     c                   leave
018300150917     c                   endif
018400150917     c* elaboro solo M D C   e telefonate per  spedizione
018500150917     c                   if        tgdtco='T' and tgdtor='S' and tgdogg<>*blanks
018600150921     c***                if        tgdtch='D' or tgdtch='M' or tgdtch='C'
018700150921     c                   if        tgdtch='D' or tgdtch='M'
018800150917     c                   movel     tgdtch        ch                1
018900150917     c* fascia oraria
019000150917     c                   clear                   fascia            4 0
019100150921     c                   if        tgdorr<140000
019200150917     c                   eval      fascia=0800
019300150917     c                   else
019400150917     c                   eval      fascia=1400
019500150917     c                   endif
019600150917     c
019700150917     c                   exsr      elabolla
019800150917     c                   endif
019900150917     c                   endif
020000150917     c
020100150921     c     orgfil        reade     fitgd04l
020200150917     c                   enddo
020300150917     c
020400150917     c                   endsr
020500150917      *---------------------------------------------------------------*
020600150917      * verifico se ha degli alert
020700150917      *---------------------------------------------------------------*
020800150917     c**   contrAL       BegSr
020900150917     c**   ktgd32        setll     fitgd32l
021000150917     c**   ktgd32        reade     fitgd32l
021100150917     c**                 dow       not %eof(fitgd32l)
021200150917     c**
021300150917     c**                 if        tgdmad='32 '
021400150917     c**                 endif
021500150917     c**                 if        tgdmad='80 '
021600150917     c**                 endif
021700150917     c**                 if        tgdmad='81 '
021800150917     c**                 endif
021900150917     c**
022000150917     c**   ktgd32        reade     fitgd32l
022100150917     c*+                 enddo
022200150917     c**                 endsr
022300150917      *---------------------------------------------------------------*
022400150917      * lettura bolle
022500150917      *---------------------------------------------------------------*
022600150917     c     elabolla      BegSr
022700150921     c                   movel     tgdorr        wora              4 0
022800150917     c                   movel     tgdogg        wbolla
022900150917     c                   movel     'F'           tipob
023000150917     c
023100150917     c     kbolla        chain     fnarb01l
023200150917     c                   if        not %found(fnarb01l)
023300150917     c     kbolla        chain     fnblp01l
023400150917     c                   if        not %found(fnblp01l)
023500150917     c     kbolla        chain     titas30c
023600150917     c                   if        not %found(titas30c)
023700150917     c                   add       1             nobolla           7 0
023800150917     c                   leavesr
023900150917     c                   else
024000150917     c                   movel     'T'           tipob             1
024100150922     c                   movel     tasgva        arbgva
024200150917     c                   movel     tascca        arbcca
024300150917     c                   movel     tashmc        arbhmc
024400150917     c                   endif
024500150917     c                   endif
024600150917     c                   endif
024700150923     c
024800150923     c* se ho chieso solo spedizioni ai rpivati, escluso quelle senza "P"
024900150923     c                   if        priv ='P' and %subst(arbgva:1:1)<>'P'
025000150923     c                   leavesr
025100150923     c                   endif
025200150917     c
025300150917     c* possibilità di alert
025400150917    1c                   if        tipob='F'
025500150917     c     kar5          chain     fiar501l
025600150917    2c                   if        not %found(fiar501l)
025700150917     c                   eval      tal='N'
025800150917   x2c                   else
025900150917     c                   movel     ar5uni        dar5emd
026000150921    3c                   if        §AR5smp='S'         or §ar5mmp='S'
026100150917     c                   eval      tal='S'
026200150917     c                   else
026300150917     c                   eval      tal='N'
026400150917    3c                   endif
026500150917    2c                   endif
026600150917     c
026700150917   x1c                   else
026800150917     c                   movel     tasflo        dtasflo
026900150917    2c                   if        §floemd='S'
027000150917     c                   eval      tal='S'
027100150917     c                   else
027200150917     c                   eval      tal='N'
027300150917    2c                   endif
027400150917    1c                   endif
027500150918     c
027600150917     c* verifico se la bolla ha evento di consegna prima o dopo questa r.a.
027700150917     c                   eval      primo='N'
027800150923     c     kbolla        setll     fnevb01l
027900150923     c     kbolla        reade     fnevb01l
028000150923     c                   dow       not %eof(fnevb01l)
028100150917     c                   if        %lookup(evbcev:manc)>0
028200150917     c* Verifico l'orario
028300150921     c                   if        tgddar>evbdev
028400150918     c                   eval      primo='S'
028500150918     c                   else
028600150921     c                   if        tgddar=evbdev and wora  >evbhev
028700150917     c                   eval      primo='S'
028800150917     c                   endif
028900150917     c                   endif
029000150918     c
029100150918     c                   leave
029200150918     c                   endif
029300150917     c
029400150923     c     kbolla        reade     fnevb01l
029500150917     c                   enddo
029600150917     c* indice schiera
029700150917     c                   clear                   ZZ                3 0
029800150917     c                   if        ch='M'
029900150917     c                   z-add     1             zz
030000150917     c                   endif
030100150917     c                   if        ch='D'
030200150917     c                   z-add     9             zz
030300150917     c                   endif
030400150917     c                   if        ch='C'
030500150917     c                   z-add     17            zz
030600150917     c                   endif
030700150917     c* fascia
030800150917     c                   if        fascia=1400
030900150917     c                   add       4             zz
031000150917     c                   endif
031100150917     c* presenza alert
031200150917     c                   if        tal='N'
031300150917     c                   add       2             zz
031400150917     c                   endif
031500150917     c* prima o dopo 1°tent.consegna
031600150917     c                   if        primo='S'
031700150917     c                   add       1             zz
031800150917     c                   endif
031900150917     c* totale
032000150917     c                   add       1             tot(ZZ)
032100150917     c* privato
032200150922     c                   if        %subst(arbgva:1:1)='P'
032300150917     c                   add       1             pri(ZZ)
032400150917     c                   endif
032500150917     c* dirottata
032600150918     c**                 if        arbcca='1' and arbhmc>wora
032700150918     c**                 add       1             dir(zz)
032800150918     c**                 endif
032900150917     c* resa
033000150918     c**                 if        arbcca='2' or arbcca='6'
033100150918     c**                 if        arbhmc>wora
033200150918     c**                 add       1             reso(ZZ)
033300150918     c**                 endif
033400150918     c**                 endif
033500150917     c* dispo di consegna
033600150921     c**                 if        tgdmad=' 86'
033700150921     c**                 add       1             disp(zz)
033800150921     c**                 endif
033900150918     c* con giacenza
034000150918     c     kbolla        chain     tigcp01l
034100150918     c                   if        %found(tigcp01l)
034200150918     c                   z-add     gcpmgc        wdata
034300150918     c                   movel     gcpagc        wdata
034400150918     c                   if        tgddac> wdata
034500150918     c                   add       1             gia(zz)
034600150918     c                   endif
034700150918     c                   endif
034800150921     c
034900150921     c                   z-add     1             ra                3 0
035000150921     c                   eval      ra=%lookup(tgdmad:tmad)
035100150921     c                   if        ra=0
035200150921     c                   eval      ra=%lookup('   ':tmad)
035300150921     c                   if        ra>0 and ra<=30
035400150921     c                   eval      tmad(ra)=tgdmad
035500150921     c                   else
035600150921     c                   eval      ra=31
035700150921     c                   eval      tmad(31)='OTH'
035800150921     c                   endif
035900150921     c                   endif
036000150921     c
036100150921     C     ZZ            OCCUR     UDAT                                         *POSIZIONAMENTO
036200150921    1c                   if        ra=31
036300150921    2c                   if        mad(31)=*blanks
036400150921     c                   eval      mad(31)='OTH'
036500150921     c                   z-add     1             totra(31)
036600150921     c                   else
036700150921     c                   add       1             totra(31)
036800150921    2c                   endif
036900150921     c
037000150921   x1c                   else
037100150921    2c                   if        mad(ra)=*blanks
037200150921     c                   eval      mad(ra)=tgdmad
037300150921     c                   z-add     1             totra(ra)
037400150921     c                   else
037500150921     c                   add       1             totra(ra)
037600150921    2c                   endif
037700150921    1c                   endif
037800150917     c
037900150917     c                   endsr
038000150917      *---------------------------------------------------------------*
038100150917      * scrittura file
038200150917      *---------------------------------------------------------------*
038300150917     c     scrivi        BegSr
038400150917     c*
038500150917     c                   clear                   wftgt000
038600150917     c                   eval      WATDTADA=datada
038700150917     c                   eval      WATDTAa =dataa
038800150917     c                   eval      WTGTTCH ='M'
038900150917     c                   eval      WTGTFORA=0800
039000150917     c                   z-add     1             zz
039100150917     c                   exsr      Wrifascia
039200150917     c
039300150917     c                   eval      WTGTFORA=1400
039400150917     c                   add       1             zz
039500150917     c                   exsr      Wrifascia
039600150917     c
039700150917     c*
039800150917     c                   clear                   wftgt000
039900150917     c                   eval      WATDTADA=datada
040000150917     c                   eval      WATDTAa =dataa
040100150917     c                   eval      WTGTTCH ='D'
040200150917     c                   eval      WTGTFORA=0800
040300150917     c                   z-add     9             zz
040400150917     c                   exsr      Wrifascia
040500150917     c
040600150917     c                   eval      WTGTFORA=1400
040700150917     c                   add       1             zz
040800150917     c                   exsr      Wrifascia
040900150917     c*
041000150921     c**                 clear                   wftgt000
041100150921     c**                 eval      WATDTADA=datada
041200150921     c**                 eval      WATDTAa =dataa
041300150921     c**                 eval      WTGTTCH ='C'
041400150921     c**                 eval      WTGTFORA=0800
041500150921     c**                 z-add     17            zz
041600150921     c**                 exsr      Wrifascia
041700150917     c
041800150921     c**                 eval      WTGTFORA=1400
041900150921     c**                 add       1             zz
042000150921     c**                 exsr      Wrifascia
042100150917     c
042200150917     c                   endsr
042300150917      *---------------------------------------------------------------*
042400150917     c     Wrifile       BEGSR
042500150917     c                   eval      WTGTTOT =tot(zz)
042600150917     c                   eval      WTGTbp  =pri(zz)
042700150921     c**                 eval      WTGT86  =disp(zz)
042800150921     c**                 eval      WTGTdir =dir(zz)
042900150921     c**                 eval      WTGTreso=reso(zz)
043000150918     c                   eval      WTGTgia =gia(zz)
043100150921     c*
043200150921     c     zz            occur     udat
043300150921     c                   eval      wtgtma1=  mad(1)
043400150921     c                   eval      wtgtto1=totra(1)
043500150921     c                   eval      wtgtma2=  mad(2)
043600150921     c                   eval      wtgtto2=totra(2)
043700150921     c                   eval      wtgtma3=  mad(3)
043800150921     c                   eval      wtgtto3=totra(3)
043900150921     c                   eval      wtgtma4=  mad(4)
044000150921     c                   eval      wtgtto4=totra(4)
044100150921     c                   eval      wtgtma5=  mad(5)
044200150921     c                   eval      wtgtto5=totra(5)
044300150921     c                   eval      wtgtma6=  mad(6)
044400150921     c                   eval      wtgtto6=totra(6)
044500150921     c                   eval      wtgtma7=  mad(7)
044600150921     c                   eval      wtgtto7=totra(7)
044700150921     c                   eval      wtgtma8=  mad(8)
044800150921     c                   eval      wtgtto8=totra(8)
044900150921     c                   eval      wtgtma9=  mad(9)
045000150921     c                   eval      wtgtto9=totra(9)
045100150921     c                   eval      wtgtma10=  mad(10)
045200150921     c                   eval      wtgtto10=totra(10)
045300150921     c                   eval      wtgtma11=  mad(11)
045400150921     c                   eval      wtgtto11=totra(11)
045500150921     c                   eval      wtgtma12=  mad(12)
045600150921     c                   eval      wtgtto12=totra(12)
045700150921     c                   eval      wtgtma13=  mad(13)
045800150921     c                   eval      wtgtto13=totra(13)
045900150921     c                   eval      wtgtma14=  mad(14)
046000150921     c                   eval      wtgtto14=totra(14)
046100150921     c                   eval      wtgtma15=  mad(15)
046200150921     c                   eval      wtgtto15=totra(15)
046300150921     c                   eval      wtgtma16=  mad(16)
046400150921     c                   eval      wtgtto16=totra(16)
046500150921     c                   eval      wtgtma17=  mad(17)
046600150921     c                   eval      wtgtto17=totra(17)
046700150921     c                   eval      wtgtma18=  mad(18)
046800150921     c                   eval      wtgtto18=totra(18)
046900150921     c                   eval      wtgtma19=  mad(19)
047000150921     c                   eval      wtgtto19=totra(19)
047100150921     c                   eval      wtgtma20=  mad(20)
047200150921     c                   eval      wtgtto20=totra(20)
047300150921     c                   eval      wtgtma21=  mad(21)
047400150921     c                   eval      wtgtto21=totra(21)
047500150921     c                   eval      wtgtma22=  mad(22)
047600150921     c                   eval      wtgtto22=totra(22)
047700150921     c                   eval      wtgtma23=  mad(23)
047800150921     c                   eval      wtgtto23=totra(23)
047900150921     c                   eval      wtgtma24=  mad(24)
048000150921     c                   eval      wtgtto24=totra(24)
048100150921     c                   eval      wtgtma25=  mad(25)
048200150921     c                   eval      wtgtto25=totra(25)
048300150921     c                   eval      wtgtma26=  mad(26)
048400150921     c                   eval      wtgtto26=totra(26)
048500150921     c                   eval      wtgtma27=  mad(27)
048600150921     c                   eval      wtgtto27=totra(27)
048700150921     c                   eval      wtgtma28=  mad(28)
048800150921     c                   eval      wtgtto28=totra(28)
048900150921     c                   eval      wtgtma29=  mad(29)
049000150921     c                   eval      wtgtto29=totra(29)
049100150921     c                   eval      wtgtma30=  mad(30)
049200150921     c                   eval      wtgtto30=totra(30)
049300150921     c                   eval      wtgtma31=  mad(31)
049400150921     c                   eval      wtgtto31=totra(31)
049500150917     c                   write     wftgt000
049600150917     c                   ENDSR
049700150917      *---------------------------------------------------------------*
049800150917     c     Wrifascia     BEGSR
049900150917     c                   eval      WTGTFASN='S'
050000150917     c                   eval      WTGTTCON='N'
050100150917     c                   exsr      wrifile
050200150917     c
050300150917     c                   eval      WTGTTCON='S'
050400150917     c                   add       1             ZZ
050500150917     c                   exsr      wrifile
050600150917     c
050700150917     c                   eval      WTGTFASN='N'
050800150917     c                   eval      WTGTTCON='N'
050900150917     c                   add       1             ZZ
051000150917     c                   exsr      wrifile
051100150917     c
051200150917
051300150917     c                   eval      WTGTTCON='S'
051400150917     c                   add       1             ZZ
051500150917     c                   exsr      wrifile
051600150917     c                   ENDSR
051700150923      *---------------------------------------------------------------*
051800150923     c     contacons     BEGSR
051900150923     c                   if        fil>0
052000150923     c                   eval      orgfil=fil
052100150923     c                   exsr      leggiblp
052200150923     c                   else
052300150923     c     *loval        setll     azorg01l
052400150923     c                   read      azorg01l
052500150923     c                   dow       not %eof(azorg01l)
052600150923     c                   if        orgfag='F' or orgfag='A'
052700150923     c                   if        orgfva=' '
052800150923     c                   exsr      leggiblp
052900150923     c                   endif
053000150923     c                   endif
053100150923     c                   read      azorg01l
053200150923     c                   enddo
053300150923     c                   endif
053400150923     c
053500150923     c
053600150923     c* scrivo i dati nel file
053700150923     c                   clear                   wftgt000
053800150923     c                   eval      WATDTADA=datada
053900150923     c                   eval      WATDTAa =dataa
054000150923     c                   eval      WTGTFASN='N'
054100150923     c                   eval      WTGTTCON='N'
054200150923     c                   eval      wtgttot=nono
054300150923     c                   write     wftgt000
054400150923     c
054500150923     c                   clear                   wftgt000
054600150923     c                   eval      WATDTADA=datada
054700150923     c                   eval      WATDTAa =dataa
054800150923     c                   eval      WTGTFASN='N'
054900150923     c                   eval      WTGTTCON='S'
055000150923     c                   eval      wtgttot=nosi
055100150923     c                   write     wftgt000
055200150923     c
055300150923     c                   clear                   wftgt000
055400150923     c                   eval      WATDTADA=datada
055500150923     c                   eval      WATDTAa =dataa
055600150923     c                   eval      WTGTFASN='S'
055700150923     c                   eval      WTGTTCON='N'
055800150923     c                   eval      wtgttot=sino
055900150923     c                   write     wftgt000
056000150923
056100150923     c                   clear                   wftgt000
056200150923     c                   eval      WATDTADA=datada
056300150923     c                   eval      WATDTAa =dataa
056400150923     c                   eval      WTGTFASN='S'
056500150923     c                   eval      WTGTTCON='S'
056600150923     c                   eval      wtgttot=sisi
056700150923     c                   write     wftgt000
056800150923
056900150923     c                   ENDSR
057000150923      *---------------------------------------------------------------*
057100150923     c     Leggiblp      BEGSR
057200150923     c                   movel     datada        kaas
057300150923     c                   move      datada        kmgs
057400150923     c     kblp          setll     fnblp31l
057500150923     c     orgfil        reade     fnblp31l
057600150923    1c                   dow       not %eof(fnblp31l)
057700150923     c                   z-add     blpmgs        wdata
057800150923     c                   movel     blpaas        wdata
057900150923    2c                   if        wdata>dataa
058000150923     c                   leave
058100150923    2c                   endif
058200150923     c                   if        blpnzd='  ' and blpctm<>'4 ' and blpctm<>'4F'
058300150923     c                   if        priv =' ' or  %subst(blpgva:1:1)= 'P'
058400150923     c                   if        blpcbo='1 ' or blpcbo='2 ' or blpcbo='4 '
058500150923     c                             or blpcbo='5 ' or blpcbo='6 '
058600150923     c* Verifico se ha alert
058700150923     c                   eval      wann=blpaas
058800150923     c                   eval      wlnp=blplnp
058900150923     c                   eval      wnrs=blpnrs
059000150923     c                   eval      wnsp=blpnsp
059100150923     c     kar5          chain     fiar501l
059200150923    2c                   if        not %found(fiar501l)
059300150923     c                   eval      tal='N'
059400150923   x2c                   else
059500150923     c                   movel     ar5uni        dar5emd
059600150923    3c                   if        §AR5smp='S'         or §ar5mmp='S'
059700150923     c                   eval      tal='S'
059800150923     c                   else
059900150923     c                   eval      tal='N'
060000150923    3c                   endif
060100150923    2c                   endif
060200150923     c*
060300150923     c* verifico se ha un tentativo di consegna
060400150923     c                   eval      primo='N'
060500150923     c     kbolla        setll     fnevb01l
060600150923     c     kbolla        reade     fnevb01l
060700150923    2c                   dow       not %eof(fnevb01l)
060800150923    3c                   if        %lookup(evbcev:manc)>0
060900150923     c                   eval      primo='S'
061000150923     c                   leave
061100150923    3c                   endif
061200150923     c
061300150923     c     kbolla        reade     fnevb01l
061400150923    2c                   enddo
061500150923    2c                   select
061600150923     c                   when      tal='N' and primo='N'
061700150923     c                   add       1             nono              7 0
061800150923     c                   when      tal='N' and primo='S'
061900150923     c                   add       1             nosi              7 0
062000150923     c                   when      tal='S' and primo='N'
062100150923     c                   add       1             sino              7 0
062200150923     c                   when      tal='S' and primo='S'
062300150923     c                   add       1             sisi              7 0
062400150923    2c                   endsl
062500150923     c                   endif
062600150923     c                   endif
062700150923     c                   endif
062800150923
062900150923     c     orgfil        reade     fnblp31l
063000150923    1c                   enddo
063100150923     c
063200150923     c                   ENDSR
063300150917     c
063400150917     OQSYSPRT   E            err         1
063500150917     O                                              'R.A. di bolle non trovate'
063600150917     o                       nobolla       2  +   1
