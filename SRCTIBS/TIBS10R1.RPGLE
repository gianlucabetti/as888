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
002700091020     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
002800091020     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
002900090803
003000000000     c*--------------------------------------------------------------------------------------------*
003100000000     c* Main lines
003200000000     c*--------------------------------------------------------------------------------------------*
003300091020     c                   clear                   rtvFamily
003400091020     c                   clear                   jCurCof
003500091020     c                   clear                   jMaxCof
003600091020     c                   clear                   jMaxCop
003700091020     c                   clear                   jMaxTle
003800091203     c*
003900091203     c* Controllo parametri d input
004000091203     c                   movel     'OK'          wProcedi          2
004100091203     c                   if        wProcedi = 'OK'
004200091203     c                   if        TIBS10R1DS.tibs10ds.d10paf <> 'P' AND
004300091203     c                             TIBS10R1DS.tibs10ds.d10paf <> 'F'
004400091203     c                   eval      wProcedi = 'KO'
004500091203     c                   endif
004600091203     c                   endif
004700091203     c*
004800091203     c                   if        wProcedi = 'OK'
004900091203     c                   if        TIBS10R1DS.tibs10ds.d10cod = *zeros
005000091203     c                   eval      wProcedi = 'KO'
005100091203     c                   endif
005200091203     c                   endif
005300091203     c*
005400091203     c                   if        wProcedi = 'OK'
005500091203     c                   if        TIBS10R1DS.D10IGRPTLE <> *blanks AND
005600091203     c                             TIBS10R1DS.D10IGRPTLE <> 'W'     AND
005700091203     c                             TIBS10R1DS.D10IGRPTLE <> 'S'
005800091203     c                   eval      wProcedi = 'KO'
005900091203     c                   endif
006000091203     c                   endif
006100091203     c*
006200091203     c                   if        wProcedi = 'OK'
006300091203     c                   if        TIBS10R1DS.D10IFAMILY <> *blanks AND
006400091203     c                             TIBS10R1DS.D10IFAMILY <> 'S'     AND
006500091203     c                             TIBS10R1DS.D10IFAMILY <> 'N'
006600091203     c                   eval      wProcedi = 'KO'
006700091203     c                   endif
006800091203     c                   endif
006900091203     c*
007000091203     c                   if        wProcedi <> 'OK'
007100091203     c                   eval      TIBS10R1DS.tibs10ds.d10err = '1'
007200091203     c                   eval      TIBS10R1DS.tibs10ds.d10msg =
007300091203     c                                 'Parametri obbligatori non validi o '   +
007400091203     c                                 'non passati.'
007500091203     c                   else
007600090803     c*
007700090803     c* Se data riferimento nn ricevuta in input => considero la data corrente
007800091021     c                   if        TIBS10R1DS.tibs10ds.d10drf = *zeros
007900091203     c                   eval      TIBS10R1DS.tibs10ds.d10drf =
008000091203     c                                        %dec(%date() : *ISO)
008100090803     c                   endif
008200000000     c*
008300091020     c* Se tipo legame nn ricevuto in input => effettuo considerazioni particolari
008400091021     c                   if        TIBS10R1DS.tibs10ds.d10tle = *blanks
008500091203     c*
008600091203     c* Se nn specidficato nemmeno il "tipo gruppo legame" per assunto imposto "internet"
008700091203     c                   if        TIBS10R1DS.D10IGRPTLE = *blanks
008800091203     c                   eval      TIBS10R1DS.D10IGRPTLE = 'W'
008900091203     c                   endif
009000091019     c*
009100091019     c                   select
009200091021     c                   when      TIBS10R1DS.D10ISUN <> *blanks AND
009300091021     c                             TIBS10R1DS.D10ISUN  <> *zeros
009400091021     c                   eval      ksssun = TIBS10R1DS.D10ISUN
009500000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
009600090831     c     keyvss01_C    SETLL     tivss01l
009700090831     c     keyvss01_C    READE     tivss01l                               99
009800090803     c                   DOW       NOT *in99
009900090803     c                   IF        vssvat = *blanks                             *attivo
010000091021     c                   IF        TIBS10R1DS.tibs10ds.d10drf >= vssdde AND     *in decorrenza
010100091021     c                             TIBS10R1DS.tibs10ds.d10drf <= vssdsc
010200091021     c                   EVAL      TIBS10R1DS.tibs10ds.d10tle = vsstle          *legame
010300090831     c                   LEAVE                                                  *uscita ciclo
010400090803     c                   ENDIF
010500090803     c                   ENDIF
010600090831     c     keyvss01_C    READE     tivss01l                               99
010700090803     c                   ENDDO                                                  *fine cl.abil.TT
010800091019     c*
010900091021     c                   when      TIBS10R1DS.D10IKSU <> *blanks AND
011000091021     c                             TIBS10R1DS.D10IKSU <> *zeros
011100091021     c                   eval      kssksu = TIBS10R1DS.D10IKSU
011200091019     c                   EVAL      kssisv = 'TT'                                *tipo servizio
011300091019     c     keyvss02_P    SETLL     tivss02l
011400091019     c     keyvss02_P    READE     tivss02l                               99
011500091019     c                   DOW       NOT *in99
011600091019     c                   IF        vssvat = *blanks                             *attivo
011700091021     c                   IF        TIBS10R1DS.tibs10ds.d10drf >= vssdde AND     *in decorrenza
011800091021     c                             TIBS10R1DS.tibs10ds.d10drf <= vssdsc
011900091021     c                   EVAL      TIBS10R1DS.tibs10ds.d10tle = vsstle          *legame
012000091019     c                   LEAVE                                                  *uscita ciclo
012100091019     c                   ENDIF
012200091019     c                   ENDIF
012300091019     c     keyvss02_P    READE     tivss02l                               99
012400091019     c                   ENDDO                                                  *fine cl.abil.TT
012500091019     c*
012600091021     c                   when      (TIBS10R1DS.D10ISUN = *blanks OR
012700091021     c                              TIBS10R1DS.D10ISUN = *zeros) AND
012800091021     c                             (TIBS10R1DS.D10IKSU = *blanks OR
012900091021     c                              TIBS10R1DS.D10IKSU = *zeros)
013000091203     c                   if        §TLEGRP = TIBS10R1DS.D10IGRPTLE AND
013100091203     c                             jtle <> *zeros
013200091203     c                   else
013300091020     c                   exsr      cartab
013400091203     c                   endif
013500091020     c                   eval      rtvFamily = *on
013600091020     c*
013700091019     c                   endsl
013800091019     c*
013900091019     c                   endif
014000091020     c*
014100091020     c* se richiesta chiamata x "familia"
014200091020     c                   if        rtvFamily = *on
014300091020     c*
014400091020     c* ciclo x tutti i legami del tipo richiesto
014500091020     c                   z-add     1             jtle
014600091020     c                   dow       jtle <= %elem(sktle)
014700091020     c                   if        sktle(jtle) <> *blanks
014800091026     c                   eval      TIBS10R1DS.tibs10ds.d10tle = sktle(jtle)
014900091021     c                   eval      tibs10ds = TIBS10R1DS.tibs10ds
015000091203     c                   CALL      'TIBS10R'
015100091020     c                   PARM                    tibs10ds
015200091020     c                   PARM      'E'           tipExe            1
015300091203     c                   eval      TIBS10R1DS.tibs10ds.d10err = d10err
015400091203     c                   eval      TIBS10R1DS.tibs10ds.d10msg = d10msg
015500091020     c                   if        d10err = ' '
015600091021     c*
015700091021     c* Se richiesto di reperire l'intera familia per l'intero gruppo tipi legame...
015800091021     c                   if        TIBS10R1DS.D10IFAMILY  = 'S'
015900091020     c* Aggiungo i codici padri alla schiera preposta
016000091020     c                   add       1             jMaxCop
016100091021     c                   eval      TIBS10R1DS.D10SKCOP(jMaxCop) = d10cop
016200091020     c* Aggiungo i tipo legame  alla schiera preposta
016300091020     c                   add       1             jMaxTle
016400091021     c                   eval      TIBS10R1DS.D10SKTLE(jMaxTle) = sktle(jtle)
016500091020     c* Aggiungo i codici figli alla schiera preposta
016600091020     c                   z-add     *zeros        jCurCof
016700091020     c                   dow       jCurCof < %elem(D10SKCLI)
016800091026     c                   add       1             jCurCof
016900091026     c                   if        D10SKCLI(jCurCof) = *zeros
017000091026     c                   leave
017100091026     c                   else
017200091026     c                   if        %lookup(D10SKCLI(jCurCof):
017300091021     c                                     TIBS10R1DS.D10SKCOF) > *zeros
017400091020     c                   else
017500091020     c                   add       1             jMaxCof
017600091021     c                   eval      TIBS10R1DS.D10SKCOF(jMaxCof) =
017700091020     c                                     D10SKCLI(jCurCof)
017800091020     c                   endif
017900091026     c                   endif
018000091020     c                   enddo
018100091021     c*
018200091021     c                   else
018300091026     c                   eval      TIBS10R1DS.tibs10ds = tibs10ds
018400091026     c                   leave
018500091021     c                   endif
018600091020     c*
018700091020     c                   endif
018800091020     c*
018900091020     c                   else
019000091020     c                   leave
019100091020     c                   endif
019200091020     c*
019300091020     c                   add       1             jtle
019400091020     c                   enddo
019500091020     c*
019600091020     c* se richiesta chiamata "singola"
019700091020     c                   else
019800091021     c                   eval      tibs10ds = TIBS10R1DS.tibs10ds
019900091020     c                   CALL      'TIBS10R'
020000090803     c                   PARM                    tibs10ds
020100091020     c                   PARM      'E'           tipExe            1
020200091026     c                   eval      TIBS10R1DS.tibs10ds = tibs10ds
020300091020     c                   endif
020400091203     c*
020500091203     c                   endif
020600000000     c*
020700090831     c                   return
020800091020
020900091020
021000091020     c*--------------------------------------------------------------------------------------------*
021100091020     c* caricamento tabelle occorrenti
021200091020     c*--------------------------------------------------------------------------------------------*
021300091020     c     cartab        BEGSR
021400091020     c*---
021500091020     c* tipi legami "internet"
021600091020     c*---
021700091021     c                   z-add     *zeros        Giro              1 0
021800091021     c                   z-add     *zeros        jtle
021900091021     c                   for       Giro=1 to 2
022000091021     c*
022100091020     c                   movel     'TLE'         tbecod
022200091020     c     tbecod        setll     tntbe01l
022300091020     c     tbecod        reade     tntbe01l
022400091020     c                   dow       not %eof(tntbe01l)
022500091021     c                   setoff                                       50
022600091020     c                   if        tbeatb = *blanks
022700091020     c                   eval      dtle = tbeuni
022800091203     c* solo legami richiesti
022900091021     c                   if        §TLEGRP = TIBS10R1DS.D10IGRPTLE
023000091021     c*
023100091021     c                   if        Giro=1
023200091021     c                   select
023300091021     c                   when      §TLEGRP = 'S' and tbeke1 = 'ST'
023400091021     c                   seton                                        50
023500091021     c                   when      §TLEGRP = 'W' and tbeke1 = 'WW'
023600091021     c                   seton                                        50
023700091021     c                   endsl
023800091021     c                   endif
023900091021     c*
024000091021     c                   if        Giro=2
024100091021     c                   select
024200091021     c                   when      §TLEGRP = 'S' and tbeke1 <> 'ST'
024300091021     c                   seton                                        50
024400091021     c                   when      §TLEGRP = 'W' and tbeke1 <> 'WW'
024500091021     c                   seton                                        50
024600091021     c                   endsl
024700091021     c                   endif
024800091021     c*
024900091021     c                   if        *in50
025000091020     c                   add       1             jtle
025100091021     c                   eval      sktle(jtle) = tbeke1
025200091021     c                   endif
025300091021     c*
025400091020     c                   endif
025500091020     c                   endif
025600091020     c     tbecod        reade     tntbe01l
025700091020     c                   enddo
025800091021     c*
025900091021     c                   endfor
026000091020     c*
026100091020     c                   ENDSR
026200040330
026300060608
026400000000     c*--------------------------------------------------------------------------------------------*
026500000000     c* operazioni iniziali
026600000000     c*--------------------------------------------------------------------------------------------*
026700000000     c     *inzsr        BEGSR
026800000000     c*
026900000000     c* ricevimento parametri
027000000000     c     *ENTRY        PLIST
027100091021     c                   PARM                    TIBS10R1DS
027200000000     c*
027300000000     c* chiavi di lettura
027400090831     c     keyvss01_C    KLIST                                                  *tivss01l
027500091019     c                   KFLD                    ksssun                          -utente Strategi
027600000000     c                   KFLD                    kssisv                          -tipo servizio
027700091019     c*
027800091019     c     keyvss02_P    KLIST                                                  *tivss01l
027900091019     c                   KFLD                    kssksu                          -cl.unificante
028000091019     c                   KFLD                    kssisv                          -tipo servizio
028100090803     c*
028200090803     c                   ENDSR
