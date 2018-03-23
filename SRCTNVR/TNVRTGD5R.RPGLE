000100041230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200130415      *----------------------------------------------------------------*
000300150917      * Statistica reclami bolle
000400130415      *----------------------------------------------------------------*
000500150917     ftabel00f  if   e           k disk
000600150917     fazorg01l  if   e           k disk
000700150923     ffnevb01l  if   e           k disk
000800161122     ffnarb26l  if   e           k disk
000900161122
001000150917     ffiar501l  if   e           k disk
001100150917     f
001200150917     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300150917
001400150917     d manc            s              3    dim(100)
001500150917     d pri             s              7  0 dim(24)
001600150917     d disp            s              7  0 dim(24)
001700150917     d reso            s              7  0 dim(24)
001800150917     d dir             s              7  0 dim(24)
001900150917     d tot             s              7  0 dim(24)
002000150918     d gia             s              7  0 dim(24)
002100150930     d tvar            s              2    dim(31)
002200041230      *
002300041231      * - Parametri x Controllo profilo utenti
002400041231     d TIBS34DS      e ds                  inz
002500041231      * - Ds di riferimento al file esterno AZUTE00F
002600041231     d AZUTEDS       e ds                  extname(AZUTE00F)
002700041231      * - Ds per dati organigramma
002800041231     d DDatiUte      e ds
002900041230      *
003000041231     d KPJBA         e ds
003100150917     d dar5emd       e ds
003200150917     d ds2a          e ds
003300150917     d
003400150917     D param           DS
003500150917     D  datada                 1      8  0
003600150917     D  dataa                  9     16  0
003700150917     D  fil                   17     19  0
003800150917     D                 DS
003900150917     D  wlnp                   1      3  0
004000150917     D  wnrs                   4      5  0
004100150917     D  wnsp                   6     12  0
004200150917     D  wann                  13     16  0
004300150917     D  wbolla                 1     16
004400150917     d
004500150918     d wdata           s              8  0
004600150918     d tal             s              1
004700150917     d primo           s              1
004800150930     d variaz          s              1
004900150930     d nummin          s              7  0
005000041230      *
005100150917     c     *entry        plist
005200150917     c                   parm                    kpjba
005300150917     c                   movel     kpjbu         param
005400161122     c                   setoff                                       01
005500150917     c
005600161122    1c                   if        fil>0
005700150917     c                   eval      orgfil=fil
005800161122     c     orgfil        chain     azorg01l
005900150917     c                   exsr      leggiTGD
006000161122     c* stampo totale e % dei l.avviso
006100161122     c                   clear                   perc              5 2
006200161122     c                   if        totemd>0
006300161122     c                   eval(h)   perc=(totlav*100)/totemd
006400161122     c                   endif
006500161122     c                   except    totf
006600161122
006700161122   x1c                   else
006800161122
006900161122     c     *loval        setll     azorg01l
007000150917     c                   read      azorg01l
007100161122    2c                   dow       not %eof(azorg01l)
007200161122    3c                   if        orgfag='F' or orgfag='A'
007300161122    4c                   if        orgfva=' '
007400150917     c                   exsr      leggiTGD
007500161122
007600161122     c* stampo totale e % dei l.avviso
007700161122     c                   clear                   perc              5 2
007800161122     c                   if        totemd>0
007900161122     c                   eval(h)   perc=(totlav*100)/totemd
008000161122     c                   endif
008100161122     c                   except    totf
008200161122     c                   seton                                        01
008300161122     c                   clear                   totemd
008400161122     c                   clear                   totlav
008500161122    4c                   endif
008600161122    3c                   endif
008700161122     c
008800150917     c                   read      azorg01l
008900161122    2c                   enddo
009000161122    1c                   endif
009100150917     c
009200130415      *
009300041231     c                   seton                                        lr
009400041230      *
009500041231      *---------------------------------------------------------------*
009600041231      * Routine Iniziale                                              *
009700041231      *---------------------------------------------------------------*
009800041231     c     *InzSR        BegSR
009900041231      *
010000041231      * Reperisco dati job
010100041231     c                   exsr      DatiJob
010200150917     c
010300150917     c     ktgd          klist
010400150917     c                   kfld                    orgfil
010500161122     c                   kfld                    kaas              4 0
010600161122     c                   kfld                    kmgs              4 0
010700150917     c     kbolla        klist
010800161122     c                   kfld                    arbaas
010900161122     c                   kfld                    arblnp
011000161122     c                   kfld                    arbnrs
011100161122     c                   kfld                    arbnsp
011200161122     c     kar5          klist
011300161122     c                   kfld                    arbaas
011400161122     c                   kfld                    arblnp
011500161122     c                   kfld                    arbnrs
011600161122     c                   kfld                    arbnsp
011700161122     c                   kfld                    ktrc              3
011800150917     c                   eval      ktrc='EMD'
011900150917     c     ktab          klist
012000150917     c                   kfld                    codut             1 0
012100150917     c                   kfld                    kcod              2
012200150917     c                   z-add     1             codut
012300150917     c
012400161122     c* Carico gli eventi di lasciato avviso
012500150917     c                   eval      kcod='2A'
012600150917     c                   eval      xx=1
012700150917     c     ktab          setll     tabel00f
012800150917     c     ktab          reade     tabel00f
012900150917     c                   dow       not %eof(tabel00f)
013000150917     c                   if        tblflg=' '
013100150917     c                   movel     tbluni        ds2a
013200161122     c                   if        §2aftc= 'A'
013300150917     c                   movel     tblkey        manc(xx)
013400150917     c                   add       1             xx                3 0
013500150917     c                   endif
013600150917     c                   endif
013700150917     c
013800150917     c     ktab          reade     tabel00f
013900150917     c                   enddo
014000150917     c
014100041231     C                   ENDSR
014200041231      *
014300041231      *---------------------------------------------------------------*
014400041231      * Reperimento Dati del job (Utente/Operativi)                   *
014500041231      *---------------------------------------------------------------*
014600041231     c     DatiJob       BegSr
014700041231      *
014800041231     c     *dtaara       define    §azute        azuteds
014900041231     c     *dtaara       define    §datiute      ddatiute
015000041231      *
015100041231     c                   in(E)     *dtaara
015200041231     c                   IF        %ERROR or RSUT = *blanks
015300041231     c                   clear                   Tibs34Ds
015400041231     c                   call      'TIBS34R'
015500041231     c                   parm                    Tibs34Ds
015600041231     c                   in        *dtaara
015700041231     c                   ENDIF
015800041231     C                   ENDSR
015900150917      *---------------------------------------------------------------*
016000161122      * lettura
016100150917      *---------------------------------------------------------------*
016200150917     c     LeggiTGD      BegSr
016300161122     c                   move      datada        kmgs
016400161122     c                   movel     datada        kaas
016500161122     c     ktgd          setll     fnarb26l
016600161122     c     orgfil        reade     fnarb26l
016700161122    1c                   dow       not %eof(fnarb26l)
016800161122    2c                   if        (arbaas*10000)+arbmgs>dataa
016900150917     c                   leave
017000161122    2c                   endif
017100150917     c
017200150917     c                   exsr      elabolla
017300150917     c
017400161122     c     orgfil        reade     fnarb26l
017500150917     c                   enddo
017600150917     c
017700150917     c                   endsr
017800150917      *---------------------------------------------------------------*
017900150917      * lettura bolle
018000150917      *---------------------------------------------------------------*
018100150917     c     elabolla      BegSr
018200161122
018300150917     c* possibilità di alert
018400150917     c     kar5          chain     fiar501l
018500150917    2c                   if        not %found(fiar501l)
018600150917     c                   eval      tal='N'
018700150917   x2c                   else
018800150917     c                   movel     ar5uni        dar5emd
018900150921    3c                   if        §AR5smp='S'         or §ar5mmp='S'
019000150917     c                   eval      tal='S'
019100150917     c                   else
019200150917     c                   eval      tal='N'
019300150917    3c                   endif
019400150917    2c                   endif
019500150917     c
019600150929     c
019700161122     c                   if        tal='S'
019800161122     c                   add       1             totemd           10 0
019900150918     c
020000161122     c* verifico se la bolla ha evento di lasciato avviso
020100150917     c                   eval      primo='N'
020200150923     c     kbolla        setll     fnevb01l
020300150923     c     kbolla        reade     fnevb01l
020400161122    2c                   dow       not %eof(fnevb01l)
020500161122    3c                   if        %lookup(evbcev:manc)>0
020600150918     c                   eval      primo='S'
020700150918     c
020800150918     c                   leave
020900161122    3c                   endif
021000150917     c
021100150923     c     kbolla        reade     fnevb01l
021200161122    2c                   enddo
021300161122
021400161122    2c                   if        primo='S'
021500161122     c                   add       1             totlav            9 0
021600161122    2c                   endif
021700161122    1c                   endif
021800161122     c
021900161122     c                   ENDSR
022000150917     c
022100161122     OQSYSPRT   E            totf        1
022200161122     O              n01                             'Dal'
022300161122     O              n01      datada           +   1 '    /  /  '
022400161122     O              n01                       +   1 'al '
022500161122     O              n01      dataa            +   1 '    /  /  '
022600161122     O              n01                       +   1 'fil.Arrivo'
022700161122     o                       orgfil           +   1
022800161122     o                       orgdes           +   1
022900161122     O                                        +   1 'bolle con alert'
023000161122     o                       totemd        2  +   1
023100161122     O                                        +   1 'di cui con L.AVV'
023200161122     o                       totlav        2  +   1
023300161122     o                       perc          4  +   1
023400161122     O                                        +   0 '%'
