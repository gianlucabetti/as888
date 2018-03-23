000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200131024     H* TISI6OR *----------------------------------------------------*
000300131024     H*         - Cap/località per filiale con OrariServ.- Filtro    *
000500000000     H*--------------------------------------------------------------*
000600131024     Ftisi6oD   CF   E             WORKSTN
001100900509     FAZORG01L  IF   E           K DISK
001200140109     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
001300931130     D****
001400130703     D* PASSAGGIO DATI A pgm batch
001500131024     D tisi6ods      e DS
001501140109     D tisio9ds      e DS
012600900511     D KPJBA         E DS
012601130703     d Tibs34ds      e ds                  inz
012602130703     d §azute        e ds                  Extname(Azute00f)
012603130703     d                                     dtaara
012604130703     d §Datiute      e ds                  EXTname(ddatiute)
012605130703     d                                     dtaara
013500020813     D OG143         e ds                  inz
013800020813      *
013900020813      * Variabili
014000020813      *
014202130808     d codut           s              1  0 inz(1)
014203131025
014204131025     d fel             s              3    dim(10)
014205131025     d dfel            s             20    dim(10)
014206140109     d sgl             s              1    dim(10)
014300130612      //---------------------------------------------------------------
014401130703      /copy gaitrasrc/srcprotopr,tibs34r
014500130612
014501131025     Isi6od01
014502131025     I              vidfel                      fel(1)
014503131025     I              vidfel2                     fel(2)
014504131025     I              vidfel3                     fel(3)
014505131025     I              vidfel4                     fel(4)
014506131025     I              vidfel5                     fel(5)
014507131025     I              vidfel6                     fel(6)
014508131025     I              vidfel7                     fel(7)
014509131025     I              vidfel8                     fel(8)
014510131025     I              vidfel9                     fel(9)
014511131025     I              vidfel0                     fel(10)
014512131025     I              viddfel                     dfel(1)
014513131025     I              viddfel2                    dfel(2)
014514131025     I              viddfel3                    dfel(3)
014515131025     I              viddfel4                    dfel(4)
014516131025     I              viddfel5                    dfel(5)
014517131025     I              viddfel6                    dfel(6)
014518131025     I              viddfel7                    dfel(7)
014519131025     I              viddfel8                    dfel(8)
014520131025     I              viddfel9                    dfel(9)
014521131025     I              viddfel0                    dfel(10)
014522140109     I              vidsgl                      sgl(1)
014523140109     I              vidsgl2                     sgl(2)
014524140109     I              vidsgl3                     sgl(3)
014525140109     I              vidsgl4                     sgl(4)
014526140109     I              vidsgl5                     sgl(5)
014527140109     I              vidsgl6                     sgl(6)
014528140109     I              vidsgl7                     sgl(7)
014529140109     I              vidsgl8                     sgl(8)
014530140109     I              vidsgl9                     sgl(9)
014531140109     I              vidsgl0                     sgl(10)
014600900521     C****************************************************************
014700900521     C*  RIEPILOGO INDICATORI
014800900521     C***************************************************************
015100140109     C* 21    - TESTN SUL NUMERO FOGLIO VIAGGIO
015200900521     C* 40/45 - ERRORE
015300900521     C* 31/32 - DI COMODO
015600900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
015800900521     C*
015900900521     C*****************************************************************
016000000000     C     *ENTRY        PLIST
016100000000     C                   PARM                    KPJBA
016101130703     c                   exsr      DatiJob
018700931130     C*---------------------------------------------------------------*
018703131025     c                   clear                   fel
018704131025     c                   clear                   dfel
018705140109     c                   clear                   sgl
018706131024     c                   do        *hival
018707131024     c                   write     si6ot01
018708131024     c                   exfmt     si6od01
018709131024     c                   setoff                                       28
018710131024     c                   clear                   vidmsg
018711131024     c                   if        *inkc =*on
018712131024     c                   leave
018713131024     c                   endif
018714131024     c                   exsr      ctrd01
018715131025     c* f6=conferma e no errori --> sottomissione azione batch
018716131024     c                   if        not*in90 and *inkf
018717131025     c                   do        10            ii
018718131025     c                   if        fel(ii) <> *blanks and fel(ii)<>*zeros
018719131024     c                   clear                   tisi6ods
018720131025     c                   move      fel(ii)       si6ofel
018721140109     c                   move      sgl(ii)       si6osgl
018722131024     c                   movel     tisi6ods      kpjbu
018723131024     c                   movel     'SI6O'        kcoaz
018724131024     c                   call      'BCH10'
018725131024     c                   parm                    kpjba
018726131025     c                   endif
018727131025     c                   enddo
018728131024     c                   leave
018729131024     c                   endif
018730131024     c                   enddo
036100931130     C*
036300000000     C                   SETON                                        LR
040600940913     C**************************************************************************
040700131024     C* CONTROLLI linea cappario
040800940913     C**************************************************************************
040900131024     C     ctrd01        BEGSR
041000940913     C*
041100131024     C                   SETOFF                                       90
041101131025     c                   setoff                                       010203
041102131025     c                   setoff                                       040506
041103131025     c                   setoff                                       070809
041104131025     c                   setoff                                       10
041105140109     c                   setoff                                       111213
041106140109     c                   setoff                                       141516
041107140109     c                   setoff                                       171819
041108140109     c                   setoff                                       20
041109131025     c                   clear                   dfel
041200940315     C*
041201131025     c                   do        10            ii                2 0
041202131025     c     fel(ii)       ifne      *blanks
041203131025     c     fel(ii)       andne     *zeros
041300131025     C     '?'           SCAN      fel(ii)                                90
041400940315     C     *IN90         IFEQ      *ON
041401131024     C                   MOVEL     *BLANKS       §COD1             3
041402131024     C                   MOVE      *BLANKS       §DES1
041403131024     C                   MOVE      *BLANKS       §PRA
041404131024     C                   CALL      'TNSD24R'
041405131024     C                   PARM                    §COD1
041406131024     C                   PARM                    §TIP              1
041407131024     C                   PARM                    §DES1            25
041408131024     C                   PARM                    §PRA              3
041409131025     C                   MOVEL     §COD1         fel(ii)
041410131025     C                   MOVEL     §DES1         dfel(ii)
041411131024     C                   GOTO      ENDCT1
042600940315     C                   ENDIF
042700940315     C*
042800140109     C                   TESTN                   fel(ii)              21
042900140109     C  N21              MOVEL     MSG(1)        VIDMSG
043000140109     C  N21              SETON                                        9028
043001140109     C  n21              move      '1'           *in(ii)
043100140109     C  N21              GOTO      ENDCT1
043200940315     C*
043300131024     C* CONTROLLI SE filiale esistente
043301131025     c                   move      fel(ii)       w0030             3 0
043600131024     C     w0030         chain     azorg01l
043601131024     c                   if        not %found(azorg01l) or orgfva<>*blanks
043602131024     c                             or (orgfag <> 'F' and orgfag <> 'A')
043603131024     c                   seton                                        2890
043604131025     c                   move      '1'           *in(ii)
043605131024     C                   MOVEL     MSG(1)        VIDMSG
043606131024     c                   goto      endct1
043607131024     c                   endif
043608131024     c* non ammesse filiali estere
043609131025     c                   movel     orgdes        dfel(ii)
043610131024     c                   movel     orgde3        og143
043611131024     c                   if        §ogntw='FED' or §ogntw='EEX'
043612131024     c                             or §ogntw='DPD'
043613131024     c                   seton                                        2890
043614131024     C                   MOVEL     MSG(2)        VIDMSG
043615131025     c                   move      '1'           *in(ii)
043616131024     c                   goto      endct1
043617131024     c                   endif
043618131025     c* errore se filiale ripetuta
043619131025     c                   z-add     1             yy                2 0
043620131025     c     fel(ii)       lookup    fel(YY)                                30
043621131025     c   30ii            ifne      yy
043622131025     c                   seton                                        2890
043623131025     c                   move      '1'           *in(ii)
043624131025     c                   movel     msg(3)        vidmsg
043625131025     c                   goto      endct1
043626131025     c                   endif
043627140109     c* Senza soglia: verifico se deve averla e in tal caso errore
043628140109     c                   if        sgl(ii)=' '
043629140109     c                   clear                   tisio9ds
043630140109     c                   eval      DO0TSER='C'
043631140109     c                   move      fel(ii)       DO0LNA
043632140109     c                   call      'TISIO9R'
043633140109     C                   PARM                    KPJBA
043634140109     C                   PARM                    tiSIO9ds
043635140109     c* carico orari se no errore e no dati standard
043636140109     c                   if        do0fcsgl='S'
043637140109     c                   seton                                        2890
043638140109     c                   eval      yy=ii+10
043639140109     c                   move      '1'           *in(yy)
043640140109     c                   movel     msg(4)        vidmsg
043641140109     c                   goto      endct1
043642140109     c                   endif
043643140109     c                   endif
043644131025     c                   endif
043647131025     c                   enddo
048800931130     C*
048900931130     C     ENDCT1        ENDSR
065201130703      /free
065202130703       //-------------------------------------------------------------*
065203130703       //Reperimento Dati del job (Utente/Operativi)                  *
065204130703       //-------------------------------------------------------------*
065205130703       Begsr DatiJob;
065206130703
065207130703          in(e) §azute;
065208130703          if not %error;
065209130703           in(e) §datiute;
065210130703          endif;
065211130703          if %error or rsut = *blanks;
065212130703           tibs34r(tibs34ds);
065213130703           in §azute;
065214130703           in §datiute;
065215130703          endif;
065216130703
065217130703         endsr;
065218130703
065219130703      /end-free
065300940913     C**************************************************************************
065400000612** MESSAGGI DI ERRORE
065500131024Linea Cappario inesistente o errata                                            1
065600131025Linea estera non elaborabile                                                   2
065700131025Linea già richiesta                                                            3
065800140109Soglia obbligatoria per questa filiale                                         4
