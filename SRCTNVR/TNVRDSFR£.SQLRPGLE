000302080104     fannmr01l  if   e           k disk
000303080104     fyusoc01l  if   e           k disk
000304080104     fyusoc02l  if   e           k disk    rename(yusoc000:yusoc002)
000305080104     fyupdc01l  if   e           k disk
000306080104     ffiatt06l  if   e           k disk
000307080104     fanfrn01l  if   e           k disk
000400050222     FQSysPrt   O    F  198        PRINTER
000500050222     F                                     OFLIND(*INOA)
000501080104     dentit3           s              8
000502080104     dsoc              s                   like(pdcsocold)
000503080104     dkcc              s                   like(pdckccold)
000504080104     dksc              s                   like(pdckscold)
000505080104     ddoc              s              9  0
000700050221
002700050222
002800050222     C                   EVAL      *INLR = *ON
002900080104     c                   seton                                        oa
002901080104     c     knmr          klist
002902080104     c                   kfld                    NMRSOCIETA
002903080104     c                   kfld                    NMRUNITA
002904080104     c                   kfld                    NMRCONTAB
002906080104     c                   kfld                    NMRCHIAVE
002907080104     c                   kfld                    NMRSERIENM
002908080104     c                   kfld                    NMRTEMPO
002909080104     c     kpdc          klist
002910080104     c                   kfld                    soc
002911080104     c                   kfld                    kcc
002912080104     c                   kfld                    ksc
002913080104     c     katt          klist
002914080104     c                   kfld                    pdcsocnew
002915080104     c                   kfld                    pdckscnew
002916080104     c                   kfld                    attflg
002918080104     c                   eval      attflg = 'C'
002919080104     c     kfrn          klist
002920080104     c                   kfld                    pdcsocnew
002921080104     c                   kfld                    pdckscnew
004000050222
004100050221     C/EXEC SQL
004200080104     C+ DECLARE CONDPAG CURSOR FOR SELECT REGSOCIETA, REGentit3,
004300080104     C+ REGNRDOC, REGKCCCPA, REGKSCCPA FROM NDREG00F WHERE REGCAUSTES =
004400080104     C+ 'VFAD' and REGDTDOC ='2007-11-30'
004700050221     C/END-EXEC
004800050221
004900050221     C/EXEC SQL
005000050221     C+ OPEN CONDPAG
005100050221     C/END-EXEC
005200050221
005300050221     C                   DO        *HIVAL
005400050221
005500050221     C/EXEC SQL
005600080104     C+ FETCH NEXT FROM CONDPAG INTO :soc, :entit3, :doc, :kcc, :ksc
005700050221     C/END-EXEC
005800050221
005900050221     C                   SELECT
006000050221     C
006100050221     C                   WHEN      SQLCod = 100
006200050222     C                   LEAVE
006300050221     C
006400050221     C                   WHEN      SQLCod < 0
006500050222     C                   EVAL      *INH1 = *ON
006600050222     C                   LEAVE
006700050221     C
006800050221     C                   OTHER
006801080104     c                   if        entit3 <> ' '
006802080104     c                   movel     entit3        soc
006803080104     c                   end
006804080104     c     soc           chain     yusoc01l
006805080104     c                   if        not %found(yusoc01l)
006806080104     c     soc           chain     yusoc02l
006807080104     c                   if        not %found(yusoc02l)
006808080104     c                   iter
006809080104     c                   end
006810080104     c                   end
006811080104     c                   clear                   com9
006812080104     c                   clear                   frnsocieta
006813080104     c                   clear                   frnksc
006814080104     c                   clear                   attcdf
006815080104     c                   clear                   attsoc
006816080104     c                   clear                   savnumero
006817080104     c* prendo nuovo codice fornitore
006900080104     C     kpdc          chain     yupdc01l
006901080104     c                   if        not %found(yupdc01l)
006902080104     c                   eval      pdcsocnew = soc
006903080104     c                   eval      pdckscnew = ksc
006904080104     c                   end
006905080104     c* vedo se ho da autofatturare
006906080104     c     katt          setll     fiatt06l
006907080104     c                   do        *hival
006908080104     c     katt          reade     fiatt06l
006909080104     c                   if        %eof(fiatt06l)
006910080104     c                   leave
006911080104     c                   end
006912080104     c                   if        attnff <> 0
006913080104     c                   iter
006914080104     c                   end
006915080104     c* prendo il fornitore di fatturazione
006916080104     c     kfrn          chain     anfrn01l
006917080104     c                   if        %found(anfrn01l)
006918080104     c                   clear                   nmrunita
006919080104     c                   clear                   nmrcontab
006920080104     c                   movel     pdcsocnew     nmrsocieta
006921080104     c                   movel     'YPDR'        nmrserienm
006928080104     c                   if        frnforfatt = ' '
006929080104     c                   movel     frnksc        nmrchiave
006930080104     c                   else
006931080104     c                   movel     frnforfatt    nmrchiave
006932080104     c                   end
006933080104     c                   clear                   nmrtempo
006934080104     c                   move      '2007'        nmrtempo
006935080104     c     knmr          chain     annmr01l
006936080104     c                   if        %found(annmr01l)
006937080104     c                   move      nmrnumero     savnumero         9
006938080104     c                   eval      %subst(savnumero: 3: 3) = pdcsocnew
006939080104     c                   move      savnumero     savnumerop        9 0
006940080104     c                   add       1             nmrnumero
006941080104     c                   else
006942080104     c                   z-add     1             nmrnumero
006945080104     c                   end
006946080104     c                   move      nmrnumero     com9              9
006947080104     c                   eval      %subst(com9: 3: 3) = pdcsocnew
006948080104     c                   leave
006949080104     c                   end
006950080104     c                   enddo
008100080104     C* stampo
008101080104     c* cambio societa, fornitore fatturazione se stesso, numero = 1
008102080104     c                   if        soc <> frnsocieta and ksc = frnksc
008103080104     c                             and frnforfatt = ' ' and nmrnumero = 1
008104080104     c                   iter
008105080104     c                   end
008106080104     c* stessa societa, fornitore fatturazione se stesso, numero =
008107080104     c                   if        soc = frnsocieta and ksc = frnksc
008108080104     c                             and frnforfatt = ' ' and doc = savnumerop
008109080104     c                   iter
008110080104     c                   end
008111080104     c* non ci sono valorizzazioni
008112080104     c                   if        frnksc = ' '
008113080104     c                   iter
008114080104     c                   end
008115080104     c                   if        *inoa
008116080104     C                   EXCEPT    testa
008117080104     c                   setoff                                       oa
008118080104     c                   end
008119080104     C                   EXCEPT    Log010
008200050221     C                   ENDSL
008300050221
008400050221     C                   ENDDO
008500050224
008800050221
008900050221     C/EXEC SQL
009000050221     C+ CLOSE CONDPAG
009100050221     C/END-EXEC
009200050221
009300050222     C                   RETURN
009301080104     C     ErroreSQL     TAG
009302080104     C                   EVAL      *INH1 = *ON
009303080104     C                   RETURN
009700050222     **
009800050222     **
009900050222     ***********************************************************************************************
010300050222
010400050222     ***********************************************************************************************
010500050222     **
010600050222     **
010700050222     **
010800050222     ***********************************************************************************************
010900080104     OQSysPrt   E            testa             1
011000080104     O                                              'Controllo numetazione a -
011100080104     O                                              utofatturazione AFF/DEF.'
011101080104     O          E            testa       1
011201080104     O                                              'OLD: Fornitore Numero'
011202080104     O                                           +2 'NEW: Fornitore Numero'
011203080104     O                                           +2 'Fornitore fat.'
011300050222     O          E            Log010      1
011400080104     o                       soc
011500080104     o                       ksc                 +1
011600080104     o                       doc                 +1
011700080104     o                       frnsocieta          +1
011800080104     o                       frnksc              +1
011900080104     o                       com9                +1
012000080104     o                       frnforfatt          +1
