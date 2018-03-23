000100030124     hDECEDIT('0,') DATEDIT(*DMY.)
000200030117      *****************************************************************
000300030220      *      statistica DIST.APERTE SENZA MOTIVO
000400030117      *****************************************************************
000500041021     Ftitas26c  IF   E           K DISK
000600030220     Ffnevb04L  IF   E           K DISK
000700030220     Fwfdis01l  uf a E           k disk
000800030220     Fazorg01l  IF   E           K DISK
000900030220     Ftncsb03l  IF   E           K DISK
001000030220     D kpjba         e DS
001100030220     D tist60ds      e DS
001200030220     D OG143         E DS
001300030220     D FIP             S              3  0 DIM(50)                              FILIALI POSTE
001400030221     D FIe             S              3  0 DIM(100)                             FILIALI POSTE
001500030220     D x               S              3  0 inz                                  FILIALI POSTE
001600030220     D trovato         S              1    inz                                  FILIALI POSTE
001700030117      *****************************************************************
001800030220     c* leggo tutte le distinte con data consegna = 0 e che hanno data
001900030220     c* distinta = a quella scelta a video
002000041021     c     00000000      setll     titas26c
002100030220     c                   do        *hival
002200041021     c     00000000      reade     titas26c
002300030220     c* fine file
002400041021     c                   if        %eof(titas26c)
002500030220     c                   leave
002600030220     c                   end
002700030220     c* scarto quei record che hanno data distinta <> da quella scelta
002800030310     c                   if        tasddc < t60dti or tasddc > t60dtf
002900030220     c                   iter
003000030220     c                   end
003100030221     c* scarto filiale d'arrivo estera
003200030221     c                   z-add     1             x
003300030221     C     taslna        LOOKUP    FIe(x)                                 65
003400030221     c                   if        *in65
003500030221     c                   iter
003600030221     c                   end
003700030220     c* scarto consegne anomale
003800030220     c                   if        tascca = '1' or tascca = '2'
003900030220     c                   iter
004000030220     c                   end
004100030220     c* controllo se esistono eventi che giustificano la mancanza della consegna
004200030220     c                   exsr      srevb
004300030220     c* se trovo un evento diverso da MIC scarto la spedizione
004400030220     c                   if        trovato = 'N'
004500030220     c                   exsr      aggio
004600030220     c                   end
004700030218     c                   EndDO
004800030220     c                   seton                                        lr
004900030220     c**********************************************************************
005000030220     c     srevb         begsr
005100030220     c**********************************************************************
005200030220     c                   eval      trovato = 'N'
005300030220     c*
005400030220     c     kspe          setll     fnevb04l
005500030220     c                   do        *hival
005600030220     c     kspe          reade     fnevb04l
005700030220     c                   if        %eof(fnevb04l)
005800030220     c                   leave
005900030220     c                   end
006000030220     c* controllo codice evento
006100030220     c                   if        evbcev = 'MIC'
006200030220     c                   iter
006300030220     c                   end
006400030317     c* controllo che la data evento sia successiva alla data della distinta
006500030317     c                   if        evbdev < tasddc
006600030317     c                   iter
006700030317     c                   end
006800030220     C                   eval      trovato = 'S'
006900030220     c                   EndDO
007000030220     c*
007100030220     c                   endsr
007200030220     c**********************************************************************
007300030220     c     aggio         begsr
007400030220     c**********************************************************************
007500030220     c                   clear                   wfdis
007600030220     c     kdis          chain     wfdis01l
007700030220     c                   if        not %found(wfdis01l)
007800030220     c                   eval      wfdddc = tasddc
007900030220     c                   eval      wfdndc = tasndc
008000030220     c                   eval      wfdfla = taslna
008100030220     c* leggo azorg per distretto area
008200030220     c                   exsr      srorg
008300030220     c                   z-add     1             wfdcnd
008400030220     c                   end
008500030220     c* leggo tncsb
008600030220     c                   clear                   csbcas
008700030220     c                   if        tasfca = 'S'
008800030220     c     kspe          chain     tncsb03l
008900030220     c                   end
009000030319     c* poste ?
009100030319     c                   z-add     1             x
009200030319     C     taslnp        LOOKUP    FIP(x)                                 65
009300030319     c* SCELTA totale
009400030319     c                   if        t60det = 'N'
009500030220     c                   if        *in65
009600030319     c                   add       1             wfdnspo
009700030220     c                   add       csbcas        wfdcap
009800030220     c                   add       tasimv        wfdasp
009900030220     c                   else
010000030220     c* Bartolini
010100030220     c                   add       1             wfdnsb
010200030220     c                   add       csbcas        wfdcab
010300030220     c                   add       tasimv        wfdasb
010400030220     c                   end
010500040913     C* se la bolla è di recupero o legata la evidenzio xchè potrebbe acca-
010600040913     c* dere:
010700040913     c* arriva l'evento e la data consegna dalla chiusura operatore ma non
010800040913     c* c'è la bolla, quindi rimangono negli archivi delle trasmissioni.
010900040913     c* Poi è arriva la bolla dalla partenza con la chiusura fogli viaggio.
011000040913     c* Gira il pgm (bolla senza evento e data consegna) e segnala l'errore.
011100040913     c* Successivamente girano le trasmissioni che aggiornano la data
011200040913     c* e gli eventi (la bolla non è + in condizioni d'errore).
011300040910     c                   if        tascbo = '£' or
011400040910     c                             (tasfa1 <> ' ' and taslnp = taslna)
011500040910     c                   add       1             wfdnspa
011600040910     c                   end
011700030319     c                   else
011800030319     c                   if        *in65
011900030319     c                   z-add     1             wfdnspo
012000030319     c                   z-add     0             wfdnsb
012100030319     c                   z-add     csbcas        wfdcap
012200030319     c                   z-add     tasimv        wfdasp
012300030319     c                   else
012400030319     c                   z-add     1             wfdnsb
012500030319     c                   z-add     0             wfdnspo
012600030319     c                   z-add     csbcas        wfdcab
012700030319     c                   z-add     tasimv        wfdasb
012800030319     c                   end
012900030319     c                   eval      wfdaas = tasaas
013000030319     c                   eval      wfdlnp = taslnp
013100030319     c                   eval      wfdnrs = tasnrs
013200030319     c                   eval      wfdnsp = tasnsp
013300040910     c                   if        tascbo = '£' or
013400040910     c                             (tasfa1 <> ' ' and taslnp = taslna)
013500040910     c                   z-add     1             wfdnspa
013600040910     c                   end
013700040913     c                   end
013800040913     c*
013900030220     c* scrivo/aggiorno
014000030220     c                   if        not %found(wfdis01l)
014100030220     c                   write     wfdis
014200030220     c                   else
014300030319     c* totale
014400030319     c                   if        t60det = 'N'
014500030220     c                   update    wfdis
014600030319     c                   else
014700030319     c* dettaglio
014800030319     c                   write     wfdis
014900030319     c                   end
015000030220     c                   end
015100030220     c                   endsr
015200030319     c**********************************************************************
015300030319     c     srorg         begsr
015400030319     c**********************************************************************
015500030220     c* aggancio organigramma per vedere se spedizione posta e per reperire
015600030220     c* dati distretto e area
015700030220     c     taslna        chain     azorg01l
015800030220     c                   if        %found(azorg01l)
015900030220     c                   eval      wfddiv = orgfl3
016000030220     c                   eval      wfdare = orgcar
016100030220     c                   end
016200030220     c*
016300030220     c                   endsr
016400030220     c**********************************************************************
016500030220     c     *inzsr        begsr
016600030220     c**********************************************************************
016700030220     C     *ENTRY        PLIST
016800030220     C                   PARM                    KPJBA
016900030220     C                   movel     KPJBU         tist60ds
017000030220     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
017100030220     C                   CLEAR                   FIP
017200030221     C                   CLEAR                   FIe
017300030220     C     *LOVAL        SETLL     AZORG01L
017400030220     C                   DO        *HIVAL
017500030220     C                   READ      AZORG01L                               95
017600030220     C   95              LEAVE
017700030220     C                   MOVEL     ORGDE3        OG143
017800030220     C                   Z-ADD     1             X
017900030221     C     §OgNTW        IFEQ      'PPT'
018000030220     C     *ZEROS        LOOKUP    FIP(X)                                 23
018100030220     C   23              Z-ADD     ORGFIL        FIP(X)
018200030220     C                   END
018300030221     C     §OgNTW        IFEQ      'EUP'
018400030221     C     §OgNTW        orEQ      'FED'
018500030221     C     §OgNTW        orEQ      'EEX'
018600030221     C     §OgNTW        orEQ      'DPD'
018700030221     C     *ZEROS        LOOKUP    FIe(X)                                 23
018800030221     C   23              Z-ADD     ORGFIL        FIe(X)
018900030221     C                   END
019000030220     C                   END
019100030220     c*
019200030220     c     kspe          klist
019300030220     c                   kfld                    tasaas
019400030220     c                   kfld                    taslnp
019500030220     c                   kfld                    tasnrs
019600030220     c                   kfld                    tasnsp
019700030220     c*
019800030220     c     kdis          klist
019900030220     c                   kfld                    taslna
020000030220     c                   kfld                    tasddc
020100030220     c                   kfld                    tasndc
020200030220     c                   endsr
