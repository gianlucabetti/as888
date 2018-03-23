000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200031028     F*          passaggio dati da PC ad AS x FAIS         *
000300031029     Fmzame02l  uf   E           K DISK
000400031103     Fmzame05l  if   E           K DISK    rename(mzame000:mzame005)
000500031103     f                                     prefix(£)
000600031029     Fmzamh00f  o    E             DISK
000700031031     Fmzamh02l  if a E           k DISK    rename(mzamh000:mzamh002)
000800031029     Ftelepass  uf   E             DISK    rename(telepass:telef)
000900031104     f                                     prefix(£)
001000031029     d dstele          ds
001100031126     d tg                                  like(£f1)
001200031126     d tel                                 like(£f2)
001300031126     d dta                                 like(£f3)
001400031126     d flg                                 like(£f4)
001500031029     d rrn                            9  0
001600031031     d dsteles         ds
001700031126     d tgs                                 like(£f1)
001800031126     d tels                                like(£f2)
001900031126     d dtas                                like(£f3)
002000031126     d flgs                                like(£f4)
002100031031     d rrns                           9  0
002200031029     ddtaiso           s               d
002300031104     ddta08            s              8  0
002400031031     dmaxdta           s                   like(amhdta)
002500031031     dtelss            s                   like(tels)
002600031029     C     Kame          KLIST                                                  *
002700031029     C                   KFLD                    ametga                         *
002800031029     C                   KFLD                    ametgn                         *
002900031031     C     Kamh          KLIST                                                  *
003000031031     C                   KFLD                    amhtga                         *
003100031031     C                   KFLD                    amhtgn                         *
003200031028     C/EXEC SQL
003300031126     C+ DECLARE A1 CURSOR FOR SELECT f1, f2, f3, f4, rrn(telepass) FROM
003400031126     C+ telepass ORDER BY f2, f3 desc
003500031028     C/END-EXEC
003600031028
003700031028     C/EXEC SQL
003800031028     C+ OPEN A1
003900031028     C/END-EXEC
004000031028
004100031030     C                   do        *hival
004200031028
004300031028     C/EXEC SQL
004400031029     C+ FETCH NEXT FROM A1 INTO :dstele
004500031028     C/END-EXEC
004600031028
004700031028     C                   select
004800031028     C                   when      sqlcod = 100
004900031104     c                   if        tel<>tels and
005000031104     c                             tels<> *blanks
005100031031     c                   movel     tel           tels
005200031031     c                   clear                   tel
005300031031     c                   exsr      srwrite0
005400031031     c                   end
005500031030     C                   leave
005600031028     C                   when      sqlcod = 0
005700031028     c* dettaglio
005800031028     c                   exsr      srwrite
005900031028     C                   endsl
006000031028     C*
006100031028     C                   enddo
006200031028
006300031028     C/EXEC SQL
006400031028     C+ CLOSE A1
006500031028     C/END-EXEC
006600031030     c* leggo i record rimasti e li scrivo nello storico
006700031030     c                   clear                   dsteles
006800031030     C/EXEC SQL
006900031126     C+ DECLARE B1 CURSOR FOR SELECT f1, f2, f3, F4, rrn(telepass) FROM
007000031126     C+ telepass ORDER BY f1, f3
007100031030     C/END-EXEC
007200031030
007300031030     C/EXEC SQL
007400031031     C+ OPEN b1
007500031030     C/END-EXEC
007600031030
007700031030     C                   dou       *inlr
007800031030
007900031030     C/EXEC SQL
008000031031     C+ FETCH NEXT FROM b1 INTO :dstele
008100031030     C/END-EXEC
008200031030
008300031030     C                   select
008400031030     C                   when      sqlcod = 100
008500031030     C                   seton                                        lr
008600040212     c                   if        tgs <> *blanks
008700040212     c                   exsr      srultimo
008900031030     c                   end
009000031030     C                   when      sqlcod = 0
009100031030     c* dettaglio
009200031030     c                   exsr      srwrite1
009300031030     C                   endsl
009400031030     C*
009500031030     C                   enddo
009600031030
009700031030     C/EXEC SQL
009800031031     C+ CLOSE b1
009900031030     C/END-EXEC
010000031028     C**********************************************************************
010100031030     c* scrive record MZAME00F
010200031028     C**********************************************************************
010300031028     c     srwrite       begsr
010400031104     c* a rottura di targa aggiorno l'anagrafico perchè è la data + recente
010500031104     c* (data desc)
010600031104     c                   if        tel<>tels
010700031104     c                   movel     dstele        dsteles
010800031031     c*
010900031029     c                   movel     tg            ametga
011000031029     c                   eval      ametgn = %subst(tg: 3: 6)
011100031029     c     kame          chain     mzame02l
011200040212     c                   if        %found(mzame02l) and ametel = ' '
011203040211     c                   if        %subst(tel: 9: 1) =' '
011300040211     c                   eval      ametel= '00' + tel
011301040211     c                   else
011302040211     c                   eval      ametel= '0' + tel
011303040211     c                   end
011400031103     c* controllo che non ci sia un anagrafico in linea con stesso telepass
011600031103     c     ametel        chain     mzame05l
011601040211     c* se esiste già non lo aggiorno
011700031103     c                   if        not %found(mzame05l)
011701040211     c                   update    mzame000
011702040211     c* ...e poi cancello il file di lavoro
011703040212     c*    rrn           delete    telef
012100040212     c* scrivo il record nello storico con telepass
012102040212     c* salvato se era già presenete un altro cod.telepass
012200040212     c* nell'archivio telepass la data è "a partire da" mentre nello storico
012300040212     c* è "fino a"
012301040212     c*                  clear                   tel
012500040212     c*                  exsr      srwrite0
012600040212     c*                  movel     tels          tel
012700040212     c                   end
012800031104     c                   else
012900031104     c* se non trovo l'anagrafico pulisco i campi salvati in modo da
013000031104     c* riniziare tutto
013100031104     c                   clear                   dsteles
013200031104     c                   end
013300031104     c                   end
013400031031     c*
013500031028     c                   endsr
013600031030     C**********************************************************************
013700031030     c* scrive record MZAMH00F
013800031030     C**********************************************************************
013900031031     c     srwrite0      begsr
014000031031     c                   clear                   mzamh000
014100031031     c                   eval      amhnrf = amenrf
014200031031     c                   eval      amhtga = ametga
014300031031     c                   eval      amhtgn = ametgn
014400031031     c                   eval      amhtla = ametla
014500031104     c                   move      dtas          dta08
014600031104     c                   move      dta08         dtaiso
014700040212     c                   subdur    1:*d          dtaiso
014800031031     c                   move      dtaiso        amhdta
014900031031     c                   eval      amhds1 = 'CAMBIO TELEPASS'
015000031031     c                   eval      amhcau = amecau
015100031031     c                   eval      amhdal = amedal
015200031031     c                   eval      amhcsp = amecsp
015300031031     c                   eval      amhtip = 'TL'
015400031031     c                   eval      amhfil = amefil
015500031031     c                   eval      amhsoc = amesoc
015600031031     c                   eval      amhcat = amecat
015700031031     c                   eval      amhcdc = amecdc
015704040212     c                   eval      amhtel= tel
015900031031     c                   write     mzamh000
016000031031     c                   endsr
016100031031     C**********************************************************************
016200031031     c* scrive record MZAMH00F
016300031031     C**********************************************************************
016400031031     c     srwrite1      begsr
016500031030     c                   if        tg <> tgs
016600031103     c                   if        tgs <> *blanks
016601040212     c                   exsr      srultimo
017401040212     c                   end
017500031031     C*
017501040212     c                   movel     tg            amhtga
017502040212     c                   eval      amhtgn = %subst(tg: 3: 6)
017800031031     c     kamh          chain     mzamh02l
017900031030     c                   if        %found(mzamh02l)
018000031103     c* scrivo il primo record con teleapss a 0 perchè la data
018100031103     c* nell'archivio telepass è "a partire da" mentre nello storico è
018200031103     c* "fino a" e poi lo salvo
018300031103     c                   movel     dstele        dsteles
018400031103     c                   clear                   tels
018500031103     c                   exsr      srwrite2
018600031103     c                   movel     tel           tels
018700031030     c                   else
018800031030     c                   clear                   dsteles
018900031030     c                   end
019000031030     c                   else
019100031103     c                   movel     tels          telss
019200031103     c                   movel     dstele        dsteles
019300031104     c                   movel     telss         tels
019400031030     c                   exsr      srwrite2
019500031104     c                   movel     tel           tels
019600031030     c                   end
019700031030     c                   endsr
019800031030     C**********************************************************************
019900040212     c* scrive ultimo record
020000031030     C**********************************************************************
020100040212     c     srultimo      begsr
020101040212     c* cerco la data di variazione maggiore per scrivere l'ultimo
020102040212     c* record xchè non trovando la macchina nell'anagrafico vuol dire
020103040212     c* che è stata venduta
020104040212     c                   movel     tgs           ametga
020105040212     c                   eval      ametgn = %subst(tgs: 3: 6)
020106040212     c     kame          chain     mzame02l
020107040212     c                   if        not %found(mzame02l)
020108040212     c                   exsr      srdtamag
020110040212     c                   exsr      srwrite2
020111040212     c                   end
020112040212     c                   endsr
020113040212     C**********************************************************************
020114040212     c* scrive record MZAMH00F
020115040212     C**********************************************************************
020116040212     c     srwrite2      begsr
020200031104     c                   move      dtas          dta08
020300031104     c                   move      dta08         dtaiso
020400040212     c                   subdur    1:*d          dtaiso
020500031030     c                   move      dtaiso        amhdta
020600031030     c                   eval      amhds1 = 'CAMBIO TELEPASS'
020700031031     c                   eval      amhtip = 'TL'
020701040211     c                   if        tels <> ' '
020703040211     c                   if        %subst(tels: 9: 1) =' '
020704040211     c                   eval      amhtel= '00' + tels
020705040211     c                   else
020706040211     c                   eval      amhtel= '0' + tels
020707040211     c                   end
020708040211     c                   else
020709040211     c                   eval      amhtel = tels
020710040211     c                   end
020900031031     c                   write     mzamh000
021000031030     c     rrns          delete    telef
021100031030     c                   endsr
021200031030     C**********************************************************************
021300031030     c* cerco l'ultima data variazione maggiore x targa
021400031030     C**********************************************************************
021500031030     c     srdtamag      begsr
021600031104     c* cerco la data di variazione maggiore nello storico e gli aggiungo
021700031104     c* un giorno perchè nella subroutine srwrite toglie un gg
021800031030     C/EXEC SQL
021900031031     C+ SELECT max(amhdta) INTO :maxdta FROM mzamh00f WHERE amhtga =
022000031031     C+ substr(:tgs, 1, 2) and amhtgn = substr(:tgs, 3, 6)
022100031030     C/END-EXEC
022200031031     c                   move      maxdta        dtaiso
022300040212     c                   adddur    1:*d          dtaiso
022400031104     c                   move      dtaiso        dta08
022500031104     c                   movel     dta08         dtas
022600031030     c                   endsr
