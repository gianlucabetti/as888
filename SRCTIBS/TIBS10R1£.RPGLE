000100090803     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200090803     H datedit(*DMY)
000300000000     f*--------------------------------------------------------------------------------------------*
000400000000     f* Data base
000500000000     f*--------------------------------------------------------------------------------------------*
000600090831     ftivss01l  IF   E           K DISK
000700091019     ftivss02l  IF   E           K DISK    rename(tivss000:tivss002)
000800091020     ftntbe01l  IF   E           K DISK
000900051110
001000051110     ***********************************************************************************************
001100090831     d ksssun          S                   LIKE(vsssun)                         *lettura tivss00f
001200000000     d kssisv          S                   LIKE(vssisv)
001300091019     d kssksu          S                   LIKE(vssksu)
001400091020     d sktle           S              2    DIM(20)
001500091020     d jtle            S              5  0 INZ(*zeros)
001600091020     d rtvFamily       S               N   INZ(*off)
001700091020     d jCurCof         S              3  0 INZ(*zeros)
001800091020     d jMaxCof         S              3  0 INZ(*zeros)
001900091020     d jMaxCop         S              3  0 INZ(*zeros)
002000091020     d jMaxTle         S              3  0 INZ(*zeros)
002100000000     d*---
002200000000     d* Ds
002300000000     d*---
002400000000      * reperimento clienti da cliente unificante
002500091020     d dtle          E DS
002600090803
002700090803      * controllo data
002800090803     d wlbda8          DS                  INZ                                  *controlla data
002900090803     d  g08dat                        8  0                                       -data dritta
003000090803     d  g08inv                        8  0                                       -data invertita
003100090803     d  g08err                        1                                          -errore
003200090803     d  g08tgi                        5  0                                       -giorni fra date
003300090803
003400091020     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
003500091020     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
003600090803
003700000000     c*--------------------------------------------------------------------------------------------*
003800000000     c* Main lines
003900000000     c*--------------------------------------------------------------------------------------------*
004000091020     c                   clear                   rtvFamily
004100091020     c                   clear                   jCurCof
004200091020     c                   clear                   jMaxCof
004300091020     c                   clear                   jMaxCop
004400091020     c                   clear                   jMaxTle
004500090803     c*
004600090803     c* Reperimento data corrente
004700090803     C                   TIME                    n14              14 0          *ora (6)+ data (8)
004800090803     C                   MOVEL     n14           oracor            6 0          *ora  (6) in h:m:s
004900090803     C                   MOVE      n14           n8                8 0          *data (8) in g/m/a
005000090803     C                   Z-ADD     n8            g08dat
005100090803     C                   Z-ADD     *zeros        g08inv
005200090803     C                   MOVEL     '0'           g08err
005300090803     c                   CALL      'XSRDA8'
005400090803     c                   PARM                    wlbda8
005500090803     C                   Z-ADD     g08inv        datcor            8 0          *Data corrente a/m/g
005600090803     c*
005700090803     c* Se data riferimento nn ricevuta in input => considero la data corrente
005800091021     c                   if        TIBS10R1DS.tibs10ds.d10drf = *zeros
005900091021     c                   eval      TIBS10R1DS.tibs10ds.d10drf = datcor
006000090803     c                   endif
006100000000     c*
006200091020     c* Se tipo legame nn ricevuto in input => effettuo considerazioni particolari
006300091021     c                   if        TIBS10R1DS.tibs10ds.d10tle = *blanks
006400091019     c*
006500091019     c                   select
006600091021     c                   when      TIBS10R1DS.D10ISUN <> *blanks AND
006700091021     c                             TIBS10R1DS.D10ISUN  <> *zeros
006800091021     c                   eval      ksssun = TIBS10R1DS.D10ISUN
006900000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
007000090831     c     keyvss01_C    SETLL     tivss01l
007100090831     c     keyvss01_C    READE     tivss01l                               99
007200090803     c                   DOW       NOT *in99
007300090803     c                   IF        vssvat = *blanks                             *attivo
007400091021     c                   IF        TIBS10R1DS.tibs10ds.d10drf >= vssdde AND     *in decorrenza
007500091021     c                             TIBS10R1DS.tibs10ds.d10drf <= vssdsc
007600091021     c                   EVAL      TIBS10R1DS.tibs10ds.d10tle = vsstle          *legame
007700090831     c                   LEAVE                                                  *uscita ciclo
007800090803     c                   ENDIF
007900090803     c                   ENDIF
008000090831     c     keyvss01_C    READE     tivss01l                               99
008100090803     c                   ENDDO                                                  *fine cl.abil.TT
008200091019     c*
008300091021     c                   when      TIBS10R1DS.D10IKSU <> *blanks AND
008400091021     c                             TIBS10R1DS.D10IKSU <> *zeros
008500091021     c                   eval      kssksu = TIBS10R1DS.D10IKSU
008600091019     c                   EVAL      kssisv = 'TT'                                *tipo servizio
008700091019     c     keyvss02_P    SETLL     tivss02l
008800091019     c     keyvss02_P    READE     tivss02l                               99
008900091019     c                   DOW       NOT *in99
009000091019     c                   IF        vssvat = *blanks                             *attivo
009100091021     c                   IF        TIBS10R1DS.tibs10ds.d10drf >= vssdde AND     *in decorrenza
009200091021     c                             TIBS10R1DS.tibs10ds.d10drf <= vssdsc
009300091021     c                   EVAL      TIBS10R1DS.tibs10ds.d10tle = vsstle          *legame
009400091019     c                   LEAVE                                                  *uscita ciclo
009500091019     c                   ENDIF
009600091019     c                   ENDIF
009700091019     c     keyvss02_P    READE     tivss02l                               99
009800091019     c                   ENDDO                                                  *fine cl.abil.TT
009900091019     c*
010000091021     c                   when      (TIBS10R1DS.D10ISUN = *blanks OR
010100091021     c                              TIBS10R1DS.D10ISUN = *zeros) AND
010200091021     c                             (TIBS10R1DS.D10IKSU = *blanks OR
010300091021     c                              TIBS10R1DS.D10IKSU = *zeros)
010400091020     c                   if        jtle = *zeros
010500091020     c                   exsr      cartab
010600091020     c                   endif
010700091020     c                   eval      rtvFamily = *on
010800091020     c*
010900091019     c                   endsl
011000091019     c*
011100091019     c                   endif
011200091020     c*
011300091020     c* se richiesta chiamata x "familia"
011400091020     c                   if        rtvFamily = *on
011500091020     c*
011600091020     c* ciclo x tutti i legami del tipo richiesto
011700091020     c                   z-add     1             jtle
011800091020     c                   dow       jtle <= %elem(sktle)
011900091020     c                   if        sktle(jtle) <> *blanks
012000091026     c                   eval      TIBS10R1DS.tibs10ds.d10tle = sktle(jtle)
012100091021     c                   eval      tibs10ds = TIBS10R1DS.tibs10ds
012200091020     c                   CALL      'TIBS10R'
012300091020     c                   PARM                    tibs10ds
012400091020     c                   PARM      'E'           tipExe            1
012500091020     c                   if        d10err = ' '
012600091021     c*
012700091021     c* Se richiesto di reperire l'intera familia per l'intero gruppo tipi legame...
012800091021     c                   if        TIBS10R1DS.D10IFAMILY  = 'S'
012900091020     c* Aggiungo i codici padri alla schiera preposta
013000091020     c                   add       1             jMaxCop
013100091021     c                   eval      TIBS10R1DS.D10SKCOP(jMaxCop) = d10cop
013200091020     c* Aggiungo i tipo legame  alla schiera preposta
013300091020     c                   add       1             jMaxTle
013400091021     c                   eval      TIBS10R1DS.D10SKTLE(jMaxTle) = sktle(jtle)
013500091020     c* Aggiungo i codici figli alla schiera preposta
013600091020     c                   z-add     *zeros        jCurCof
013700091020     c                   dow       jCurCof < %elem(D10SKCLI)
013800091026     c                   add       1             jCurCof
013900091026     c                   if        D10SKCLI(jCurCof) = *zeros
014000091026     c                   leave
014100091026     c                   else
014200091026     c                   if        %lookup(D10SKCLI(jCurCof):
014300091021     c                                     TIBS10R1DS.D10SKCOF) > *zeros
014400091020     c                   else
014500091020     c                   add       1             jMaxCof
014600091021     c                   eval      TIBS10R1DS.D10SKCOF(jMaxCof) =
014700091020     c                                     D10SKCLI(jCurCof)
014800091020     c                   endif
014900091026     c                   endif
015000091020     c                   enddo
015100091021     c*
015200091021     c                   else
015300091026     c                   eval      TIBS10R1DS.tibs10ds = tibs10ds
015400091026     c                   leave
015500091021     c                   endif
015600091020     c*
015700091020     c                   endif
015800091020     c*
015900091020     c                   else
016000091020     c                   leave
016100091020     c                   endif
016200091020     c*
016300091020     c                   add       1             jtle
016400091020     c                   enddo
016500091020     c*
016600091020     c* se richiesta chiamata "singola"
016700091020     c                   else
016800091021     c                   eval      tibs10ds = TIBS10R1DS.tibs10ds
016900091020     c                   CALL      'TIBS10R'
017000090803     c                   PARM                    tibs10ds
017100091020     c                   PARM      'E'           tipExe            1
017200091026     c                   eval      TIBS10R1DS.tibs10ds = tibs10ds
017300091020     c                   endif
017400000000     c*
017500090831     c                   return
017600091020
017700091020
017800091020     c*--------------------------------------------------------------------------------------------*
017900091020     c* caricamento tabelle occorrenti
018000091020     c*--------------------------------------------------------------------------------------------*
018100091020     c     cartab        BEGSR
018200091020     c*---
018300091020     c* tipi legami "internet"
018400091020     c*---
018500091021     c                   z-add     *zeros        Giro              1 0
018600091021     c                   z-add     *zeros        jtle
018700091021     c                   for       Giro=1 to 2
018800091021     c*
018900091020     c                   movel     'TLE'         tbecod
019000091020     c     tbecod        setll     tntbe01l
019100091020     c     tbecod        reade     tntbe01l
019200091020     c                   dow       not %eof(tntbe01l)
019300091021     c                   setoff                                       50
019400091020     c                   if        tbeatb = *blanks
019500091020     c                   eval      dtle = tbeuni
019600091020     c* solo legami d tipo "internet"
019700091021     c                   if        §TLEGRP = TIBS10R1DS.D10IGRPTLE
019800091021     c*
019900091021     c                   if        Giro=1
020000091021     c                   select
020100091021     c                   when      §TLEGRP = 'S' and tbeke1 = 'ST'
020200091021     c                   seton                                        50
020300091021     c                   when      §TLEGRP = 'W' and tbeke1 = 'WW'
020400091021     c                   seton                                        50
020500091021     c                   endsl
020600091021     c                   endif
020700091021     c*
020800091021     c                   if        Giro=2
020900091021     c                   select
021000091021     c                   when      §TLEGRP = 'S' and tbeke1 <> 'ST'
021100091021     c                   seton                                        50
021200091021     c                   when      §TLEGRP = 'W' and tbeke1 <> 'WW'
021300091021     c                   seton                                        50
021400091021     c                   endsl
021500091021     c                   endif
021600091021     c*
021700091021     c                   if        *in50
021800091020     c                   add       1             jtle
021900091021     c                   eval      sktle(jtle) = tbeke1
022000091021     c                   endif
022100091021     c*
022200091020     c                   endif
022300091020     c                   endif
022400091020     c     tbecod        reade     tntbe01l
022500091020     c                   enddo
022600091021     c*
022700091021     c                   endfor
022800091020     c*
022900091020     c                   ENDSR
023000040330
023100060608
023200000000     c*--------------------------------------------------------------------------------------------*
023300000000     c* operazioni iniziali
023400000000     c*--------------------------------------------------------------------------------------------*
023500000000     c     *inzsr        BEGSR
023600000000     c*
023700000000     c* ricevimento parametri
023800000000     c     *ENTRY        PLIST
023900091021     c                   PARM                    TIBS10R1DS
024000000000     c*
024100000000     c* chiavi di lettura
024200090831     c     keyvss01_C    KLIST                                                  *tivss01l
024300091019     c                   KFLD                    ksssun                          -utente Strategi
024400000000     c                   KFLD                    kssisv                          -tipo servizio
024500091019     c*
024600091019     c     keyvss02_P    KLIST                                                  *tivss01l
024700091019     c                   KFLD                    kssksu                          -cl.unificante
024800091019     c                   KFLD                    kssisv                          -tipo servizio
024900090803     c*
025000090803     c                   ENDSR
