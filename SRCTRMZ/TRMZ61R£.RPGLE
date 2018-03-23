000100021219     H DECEDIT('0,') DATEDIT(*YMD.)
000200020606     f********************************************************************
000300021219     f* il pgm reperisce il centro di costo, la società a valere e la
000400021219     f* categoria dell'automezzo passato in una certa data
000500020606     f********************************************************************
000600020605     Fmzame02l  if   E           K DISK
000700021219     Fmzame01l  if   E           K DISK    rename(mzame000:mzame1)
000800021031     Fmzamh02l  if   E           K DISK
000900021219     Fmzamh05l  if   E           K DISK    rename(mzamh000:mzamh5)
000901071129     Fansif01l  if   E           K DISK
001000070129     d SOCIETA_PROPRIETARIA...
001100070129     d                 c                   'P'
001200070129     d AUTOVETTURA...
001300070129     d                 c                   'A'
001400070129     d DATA_INIZIO_EUROBICAR...
001500070129     d                 c                   20070101
001600021219     d trmz61ds      e ds
001601071130     d* società a  valare dell'anagrafico in corso
001602071130     d savsocval       s                   like(d61socval)
001603071130     d oggi            s               d
001700021219     c*
001800021219     c     *entry        plist
001900021219     c                   parm                    trmz61ds
002000021219     c                   clear                   d61cat
002100021219     c                   clear                   d61socval
002200021219     c                   clear                   d61cdc
002300021219     c                   clear                   d61err
002301070730     c                   clear                   d61cauo
002400021219     c*
002500021219     C     Kame          KLIST                                                  *
002600021219     C                   KFLD                    d61tga                         *
002700021219     C                   KFLD                    d61tgn                         *
002800021219     c*
002900021219     C     Kamh          KLIST                                                  *
003000021219     C                   KFLD                    amenrf                         *
003100021219     C                   KFLD                    amhtip                         *
003200021219     C                   KFLD                    d61dta                         *
003300021219     c*
003400021219     C     Kamh1         KLIST                                                  *
003500021219     C                   KFLD                    amenrf                         *
003600021219     C                   KFLD                    amhtip                         *
003601071130     c                   clear                   savsocval
003602071130     c                   move      *date         oggi
003700030115     c* leggo l'anagrafica attuale
003800021219     c                   if        d61tpe <> 'C'
003900020606     c     kame          chain     mzame02l
004000021219     c                   if        %found(mzame02l)
004100021219     c                   eval      d61cat = amecat
004200021219     c                   eval      d61cdc = amecdc
004201070730     c                   eval      d61cauo= amecau
004300070129     c                   if        d61fssoc = SOCIETA_PROPRIETARIA
004400070129     c                             and %subst(d61cat:1:1) = AUTOVETTURA
004500070129     c                             and d61dta >= DATA_INIZIO_EUROBICAR
004600070129     c                   eval      d61socval = amedit
004700070129     c                   else
004800021219     c                   eval      d61socval = amesoc
004900070129     c                   endif
004901071130     c                   eval      savsocval = d61socval
005000021219     c                   else
005100030115     c* se non lo trovo nell'anagrafica cerco nello storico
005200021219     c     kame          chain     mzamh02l
005300030115     c* se nello storico lo trovo vuol dire che c'è stato un cambio targa
005400021219     c                   if        %found(mzamh02l)
005500021219     c     amhnrf        chain     mzame01l
005600021219     c                   if        %found(mzame01l)
005700021219     c                   eval      d61cat = amecat
005800021219     c                   eval      d61cdc = amecdc
005801070730     c                   eval      d61cauo= amecau
005900070129     c                   if        d61fssoc = SOCIETA_PROPRIETARIA
006000070129     c                             and %subst(d61cat:1:1) = AUTOVETTURA
006100070129     c                             and d61dta >= DATA_INIZIO_EUROBICAR
006200070129     c                   eval      d61socval = amedit
006300070129     c                   else
006400021219     c                   eval      d61socval = amesoc
006500070129     c                   endif
006501071130     c                   eval      savsocval = d61socval
006600021219     c                   else
006700030115     c* se nell'anagrafico non lo trovo vuol dire che è stato annullato.
006800030115     c                   eval      amenrf = amhnrf
006900030115     c                   eval      amhtip = 'AN'
007000030115     c     kamh1         chain     mzamh05l
007100030115     c                   if        %found(mzamh05l)
007200030115     c                   eval      d61cat = amhcat
007300030115     c                   if        amhcat = *blanks
007400030131     c                   eval      d61cat = 'A1'
007500030115     c                   move      'A'           d61err
007600030115     c                   end
007700030115     c                   eval      d61cdc = amhcdc
007701070730     c                   eval      d61cauo= amhcau
007800030115     c                   if        amhcdc = *blanks
007900030117     c                   eval      d61cdc = '129'
008000030115     c                   move      'A'           d61err
008100030115     c                   end
008200070129     c* Purtroppo la società proprietaria non è storicizzata.
008300070129     c                   if        d61fssoc = SOCIETA_PROPRIETARIA
008400070129     c                             and %subst(d61cat:1:1) = AUTOVETTURA
008500070129     c                             and d61dta >= DATA_INIZIO_EUROBICAR
008600070129     c                   eval      d61socval = '249'
008700070129     c                   else
008800030115     c                   eval      d61socval = amhsoc
008900070129     c                   endif
009000070129     c*                  if        amhsoc = *blanks
009100070129     c                   if        d61socval = *blanks
009200021220     c                   eval      d61socval = '201'
009300030115     c                   move      'A'           d61err
009400021220     c                   end
009500021219     c                   end
009600030115     c                   end
009700021219     c                   else
009800021219     c* se non lo trovo neanche nello storico segnalo l'errore
009900021219     c                   move      'E'           d61err
010000021219     c                   end
010100021031     c                   end
010200070129
010300021219     c* reperisco la società in dotazione
010400021219     c                   eval      amhtip = 'SC'
010500030205     c     kamh          setll     mzamh05l
010600030205     c                   do        *hival
010700030205     c     kamh1         reade     mzamh05l
010800030205     c                   if        %eof(mzamh05l)
010900030205     c                   leave
011000030205     c                   end
011100040212     c                   if        amhdta >=D61DTA
011200070129     c                   if        d61fssoc = SOCIETA_PROPRIETARIA
011300070129     c                             and %subst(d61cat:1:1) = AUTOVETTURA
011400070129     c                             and d61dta >= DATA_INIZIO_EUROBICAR
011500070129     c*                  La società proprietaria l'ho già reperita da MZAME00F.
011600070129     c                   else
011700021219     c                   eval      d61socval = amhsoc
011800070129     c                   endif
011900030205     c                   leave
012000021219     c                   end
012100030205     c                   enddo
012101071130     c* controllo se la società a valere è ancora attiva. nel caso sia
012102071130     c* cessata imposto quella dell'anagrafico se presente, altrimenti
012103071130     c* forzo 201 con anomalia
012104071130     c                   if        d61socval <> ' ' and d61err = ' '
012105071130     c     d61socval     chain     ansif01l
012107071130     c                   if        %found(ansif01l) and SIFDTFIVL <= oggi
012108071130     c                             and SIFDTFIVL <> *loval
012109071130     c                   if        savsocval <> ' '
012110071130     c                   eval      d61socval = savsocval
012111071130     c                   else
012112071130     c                   eval      d61socval = '201'
012113071130     c                   move      'A'           d61err
012114071130     c                   end
012115071130     c                   end
012116071130     c                   end
012200070129
012300030205     c* reperisco il cdc
012400030205     c                   eval      amhtip = 'CD'
012500030205     c     kamh          setll     mzamh05l
012600030205     c                   do        *hival
012700030205     c     kamh1         reade     mzamh05l
012800030205     c                   if        %eof(mzamh05l)
012900030205     c                   leave
013000030205     c                   end
013100040212     c                   if        amhdta >=D61DTA
013200030205     c                   eval      d61cdc = amhcdc
013300030205     c                   leave
013400030205     c                   end
013500030205     c                   enddo
013501070730     c* reperisco l'autista
013502070730     c                   eval      amhtip = 'AU'
013503070730     c     kamh          setll     mzamh05l
013504070730     c                   do        *hival
013505070730     c     kamh1         reade     mzamh05l
013506070730     c                   if        %eof(mzamh05l)
013507070730     c                   leave
013508070730     c                   end
013509070730     c                   if        amhdta >=D61DTA
013510070730     c                   eval      d61cauo= amhcau
013511070730     c                   leave
013512070730     c                   end
013513070730     c                   enddo
013600021219     c                   end
013700021219     c*
013800021219     c                   if        d61tpe = ' '
013900021219     c                   seton                                        rt
014000021219     c                   else
014100021219     c                   seton                                        lr
014200021219     c                   end
