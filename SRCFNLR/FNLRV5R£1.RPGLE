000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200050616     H* FNLRV5R *-----------------------------------------------------*
000300050616     H*   Statistica m.cubi in arrivo per linea di arrivo
000400930126     H*---------------------------------------------------------------*
000500941221     FFNFVA02L  IF   E           K DISK
000600060920     FFNFgv01L  IF   E           K DISK
000700040610     FFNARB06L  IF   E           K DISK
000800091023     FFNbtp06L  IF   E           K DISK
000900971223     FAZORG01L  IF   E           K DISK
001000091022     FFNLRV5P   O    E             PRINTER oflind(*in90)
001100921109     D*
001200050616     D* passaggio dati  - FNLRV5R -
001300050616     D*
001400050616     D PARA05          DS
001500050616     D  P05DFV                 1      8  0
001600050616     D  P05NFV                 9     13  0
001700050616     D  P05NPG                14     14  0
001800050616     D  P05FGS                15     17  0
001900060919     D  P05pof                18     20  0
002000060920     D  P05loc                21     45
002100091022     D  P05tra                46     46
002200951114     D*
002300911010     D WLBDAT          DS
002400941221     D  G02DAT                 1      8  0
002500941221     D  G02INV                 9     16  0
002600941221     D  G02ERR                17     17
002700941221     D  G02TGI                18     22  0
002800960304     D*
002900050616      * Parametri per richiamo a TRUL09R
003000050616     d Trul09ds      e ds
003100050616     d  Arr2l                 19    108  0 dim(30)
003200050616     D trul06ds      E DS
003300050616     D  T06                    1     90  0
003400050616     D                                     DIM(30)
003500060919     d
003600060919     D L6              S              3  0 DIM(30)
003700060920     D NFVloc          S              5    DIM(5)
003800050616     D
003900050616     d stafva          s            132    dim(50)                              sk tbl con ass.
004000050616     d ncl             s             12  0 dim(30)
004100050616     d nclabi          s             12  0 dim(30)
004200050616     d pkf             s             12  3 dim(30)
004300050616     d pkfabi          s             12  3 dim(30)
004400050616     d vlf             s             12  3 dim(30)
004500050616     d vlfabi          s             12  3 dim(30)
004600060919     d spe             s              9  0 dim(30)
004700060919     d speabi          s              9  0 dim(30)
004800091022     d
004900091022     d skbtpvol        s             12  3 dim(990)
005000091022     d skbtppes        s             12  3 dim(990)
005100091022     d skbtpncl        s             12  0 dim(990)
005200091022     d skbtpspe        s              9  0 dim(990)
005300060920     d knfv            s                   like(fgvnfv)
005400091022     d wvol            s                   like(btpvlc)
005500091022     d wpes            s                   like(btppkc)
005600091026     d Knfa            s                   like(fvanfa)
005700091026     d Knpg            s                   like(fvanpg)
005800050616     d
005900040604     D CNCR80        E DS
006000040604     D KPJBA         E DS
006100030722     D UT§DSE0F      E DS
006200911010     C*---------------------------------------------------------------*
006300000000     C     *ENTRY        PLIST
006400000000     C                   PARM                    KPJBA
006500050616     c                   movel     kpjbu         para05
006600900511     C*---------------------------------------------------------------*
006700091026     c* Impostazioni e caricamenti di comodo
006800050616     C                   EXSR      CARImpo
006900091022     c* Dal fogLio arrivi leggo i fogli abbinati e relative bolle
007000050616     C                   EXSR      CARFoglipar
007100050616     c* Stampo la statistica
007200050616     C                   EXSR      STAMPA
007300960202     C*
007400000000     C                   SETON                                        LR
007500960205     C**************************************************************************
007600050616     C*    Impostazioni e caricamenti di comodo
007700960205     C**************************************************************************
007800050616     c     CARIMPO       begsr
007900050616     C****  KLIST  ****
008000050616     C     KARB          KLIST
008100050616     C                   KFLD                    fvalnp
008200050616     C                   KFLD                    fvanfv
008300091022     C     Kbtp          KLIST
008400091022     C                   KFLD                    p05pof
008500091022     C                   KFLD                    fvalnp
008600091022     C                   KFLD                    fvanfv
008700091026     C     KFVA2         KLIST                                                  FNFVA02L
008800091026     C                   KFLD                    KNPG                           ..CATEGORIA
008900091026     C                   KFLD                    kNFa                           ..FOGLIO
009000091026     C                   KFLD                    p05pof
009100060920     C     KFGV          KLIST                                                  FNFVA02L
009200060920     C                   KFLD                    knfv                           ..FOGLIO
009300060920     C                   KFLD                    p05pof                         ..CATEGORIA
009400050616     c
009500050616     C                   Z-ADD     1             CODUT
009600050616     C                   CALL      'X§PARUT'
009700050616     C                   PARM                    UT§DSE0F
009800050616     C                   MOVEL     REC80         CNCR80
009900941216     C*
010000930519     C* DATA FOGLIO ARRIVI
010100050616     C                   MOVE      p05DFV        G02INV
010200920709     C                   MOVE      *ZEROS        G02DAT
010300920709     C                   MOVEL     3             G02ERR
010400941221     C                   CALL      'XSRDA8'
010500920709     C                   PARM                    WLBDAT
010600050616     C                   Z-ADD     G02DAT        V1cDFA
010700050616     C
010800050616     C* DECODIFICA P.O. FOGLIO
010900060919     C     P05pof        CHAIN     AZORG01L
011000050616     C                   IF        %FOUND(AZORG01L)
011100060919     C                   MOVEL     ORGDES        P5Dpof
011200050616     C                   ELSE
011300060919     C                   MOVEL     *ALL'?'       P5Dpof
011400050616     C                   ENDIF
011500060919     C* DECODIFICA P.O. in gestione
011600060919     C     P05fgs        CHAIN     AZORG01L
011700060919     C                   IF        %FOUND(AZORG01L)
011800060919     C                   MOVEL     ORGDES        P5Dfgs
011900060919     C                   ELSE
012000060919     C                   MOVEL     *ALL'?'       P5Dfgs
012100060919     C                   ENDIF
012200091026     c*
012300091026     c* decodifico dati transito SI/NO
012400091026     c                   if        p05tra='S'
012500091026     c                   eval      v1ctrans='SI'
012600091026     c                   else
012700091026     c                   eval      v1ctrans='NO'
012800091026     c                   endif
012900050616     c
013000050616     c* Alla data foglio arrivi carico le possibili linee di cui è terminal
013100050616     c*  il p.o. gestione
013200060919     c                   Clear                   Trul09ds
013300060919     c                   if        p05fgs=p05pof
013400060919     c
013500050616     c                   Eval      I09Mod = 'TFU'
013600050616     c                   Eval      I09Tfa = p05fgs
013700050616     c                   Eval      I09dta = p05dfv
013800050616     c                   Call      'TRUL09R'
013900050616     c                   Parm                    Trul09ds
014000060919     c*
014100060919     c                   else
014200060919     C*
014300060919     C* Carico i p.o. della l6 del p.o. in gestione se <> dal p.o. foglio
014400060919     c                   clear                   kpjbu
014500060919     c                   clear                   trul06ds
014600060919     c                   eval      d06cod = '£6'
014700060919     c                   movel     p05fgs        d06key
014800060919     c                   movel     trul06ds      kpjbu
014900060919     c                   call      'TRUL06R'
015000060919     c                   parm                    kpjba
015100060919     c                   movel     kpjbu         trul06ds
015200060919     c                   movea     t06           Arr2l
015300060919     c                   endif
015400911008     C*
015500050616     C* Carico tutti i p.o. gestiti
015600050616     c                   clear                   kpjbu
015700050616     c                   clear                   trul06ds
015800050616     c                   eval      d06cod = '£6'
015900050616     c                   movel     p05fgs        d06key
016000050616     c                   eval      d06esc = 'G'
016100060919     c                   eval      d06tla = 'L'
016200050616     c                   movel     trul06ds      kpjbu
016300050616     c                   call      'TRUL06R'
016400050616     c                   parm                    kpjba
016500050616     c                   movel     kpjbu         trul06ds
016600050616     c
016700050616     c                   ENDSR
016800050616     C**************************************************************************
016900050616     C*    Letto fogli partenza abbinati al foglio arrivi
017000050616     C**************************************************************************
017100050616     c     CARFoglipar   begsr
017200091026     c
017300091026     c                   setoff                                       16
017400091026     c
017500060920     c                   z-add     1             lo                2 0
017600060920     c                   z-add     1             zz                4 0
017700050616     c                   z-add     1             yy                4 0
017800060920     c* Leggo prima e carico i dati dei fogli locali
017900060920     c                   movea     p05loc        nfvloc
018000060920     c
018100060920     c                   do        5             lo
018200060920     c                   if        nfvloc(lo)>*zeros
018300060920     c                   movel     nfvloc(lo)    knfv
018400060920     c
018500060920     c     kfgv          chain     fnfgv01l
018600060920     c                   if        %found(fnfgv01l)
018700060920     c                   eval      fvalnp=fgvlnp
018800060920     c                   eval      fvanfv=fgvnfv
018900060920     c                   exsr      ELABORA
019000060920     c                   endif
019100060920     c                   endif
019200060920     c                   enddo
019300060920     c
019400091026     c                   eval      knpg=p05npg
019500091026     c                   eval      knfa=p05nfv
019600060919     c
019700941221     C     KFVA2         SETLL     FNFVA02L
019800091026     C     KFVA2         READE     FNFVA02L                               30
019900091026
020000091026     c* se non c'e' nessun foglio abbinato, stampo i dati dei fogli ancora da abbinare
020100091026     c                   if        *in30
020200091026     c                   clear                   knpg
020300091026     c                   clear                   knfa
020400091026     c                   seton                                        16
020500091026     C     KFVA2         SETLL     FNFVA02L
020600091026     C     KFVA2         READE     FNFVA02L                               30
020700091026     c                   endif
020800091026     c
020900091026     c                   dow       not *in30
021000060920     c                   exsr      ELABORA
021100050616     c
021200091026     C     KFVA2         READE     FNFVA02L                               30
021300050616     c                   enddo
021400050616     c
021500050616     c                   if        p5dfva<>*Blanks
021600050616     c                   movel     p5dfva        stafva(YY)
021700050616     c                   endif
021800960205     C*
021900930127     C                   ENDSR
022000060920     C**************************************************************************
022100060920     C* elabora foglio viaggio partenza per totali
022200060920     C**************************************************************************
022300060920     c     ELABORa       begsr
022400060920     c
022500060920     c* Imposto il numero del foglio nelle riga di stampa
022600060920     c                   movel     fvalnp        w003a             3
022700060920     c                   eval      %subst(p5dfva:zz:3)=w003a
022800060920     c                   add       3             zz
022900060920     c                   eval      %subst(p5dfva:zz:1)='-'
023000060920     c                   add       1             zz
023100060920     c                   eval      %subst(p5dfva:zz:6)=(%editc(fvanfv:'2'))
023200060920     c                   add       8             zz
023300060920     c
023400060920     c* Se non ce ne stanno + memorizzo la riga di stampa e riparto da 1
023500060920     c                   if        zz>123
023600060920     c                   movel     p5dfva        stafva(YY)
023700060920     c                   add       1             yy
023800060920     c                   clear                   p5dfva
023900060920     c                   z-add     1             zz
024000060920     c                   endif
024100060920     c
024200060920     c
024300060920     c     karb          setll     fnarb06l
024400060920     c     karb          reade     fnarb06l
024500060920     c                   dow       not %eof(fnarb06l)
024600060920     c
024700060920     c* totalizzo per linea di arrivo colli peso e volumi
024800060920     c*  escludo transiti
024900060920     c                   z-add     1             xx                4 0
025000060920     c     arblna        lookup    Arr2l(XX)                              30
025100060920     c                   if        *in30
025200060920     c                   add       arbncl        ncl(xx)
025300060920     c                   add       arbvlf        vlf(xx)
025400060920     c                   add       arbpkf        pkf(xx)
025500060920     C                   ADD       1             SPE(XX)
025600060920     c* totale a parte per le abilitate (partite)
025700060920     c                   if        arbama>0
025800060920     c                   add       arbncl        nclabi(xx)
025900060920     c                   add       arbvlf        vlfabi(xx)
026000060920     c                   add       arbpkf        pkfabi(xx)
026100060920     C                   ADD       1             SPEabi(XX)
026200060920     c                   endif
026300060920     c                   endif
026400060920     c
026500060920     c     karb          reade     fnarb06l
026600060920     c                   enddo
026700091022     c
026800091022     c* Se richiesto elaboro anche le bolle transito per terminal di arrivo
026900091022     c                   if        p05tra='S'
027000091022     c                   exsr      ElaTrans
027100091022     c                   endif
027200091022     c
027300060920     c                   ENDSR
027400091022     C**************************************************************************
027500091022     C* ELABORO BOLLE TRANSITO
027600091022     C**************************************************************************
027700091022     C     ELATRANS      BEGSR
027800091022     c     kbtp          setll     fnbtp06l
027900091022     c     kbtp          reade     fnbtp06l
028000091022     c                   dow       not %eof(fnbtp06l)
028100091022     c* Mewmorizzo i dati
028200091022     c* Volume--> il VDL se totale altrimenti il bollettato
028300091022     c                   if        btpncr=btpncl
028400091022     c                   eval      wvol=btpvlc
028500091022     c                   else
028600091022     c                   if        btpvlc>btpvlb
028700091022     c                   eval      wvol=btpvlc
028800091022     c                   else
028900091022     c                   eval      wvol=btpvlb
029000091022     c                   endif
029100091022     c                   endif
029200091022     c                   if        btpncp=btpncl
029300091022     c                   eval      wpes=btppkc
029400091022     c                   else
029500091022     c                   if        btpvlc>btpvlb
029600091022     c                   eval      wpes=btppkc
029700091022     c                   else
029800091022     c                   eval      wpes=btppkb
029900091022     c                   endif
030000091022     c                   endif
030100091022     c
030200091022     C                   eval      skbtpvol(btpTFA) = skbtpvol(btpTFA) + wvol
030300091022     C                   eval      skbtppes(btpTFA) = skbtppes(btpTFA) + wpes
030400091023     C                   eval      skbtpncl(btpTFA) = skbtpncl(btpTFA) + btpncl
030500091022     C                   eval      skbtpspe(btpTFA) = skbtpspe(btpTFA) + 1
030600091022     c
030700091022     c     kbtp          reade     fnbtp06l
030800091022     c                   enddo
030900091022     c
031000091022     c                   ENDSR
031100040604     C**************************************************************************
031200050616     C* STAMPO STATISTICA
031300040604     C**************************************************************************
031400040604     C     STAMPA        BEGSR
031500091023     c                   setoff                                       15
031600050616     c* Imposta il campo systema informativo/p.o. simfel
031700050617     c                   eval      p5csyf=%subst(ragut:1:25)
031800040604     C* STAMPA TESTATA
031900050616     C                   write     lrv5pt
032000091104     c                   setoff                                       90
032100050616     c
032200050616     c* Stampo 2 parte testata con i fogli viaggio partenza abbinati
032300050616     c                   z-add     1             yy
032400050616     c                   dow       stafva(YY)<>*blanks
032500050616     c                   movel     stafva(YY)    p5dfva
032600050616     C                   write     lrv5pt2
032700050616     c
032800050616     c                   add       1             yy
032900050616     c                   enddo
033000050616     c
033100050616     c* Stampo 3 parte testata
033200050616     C                   write     lrv5pI
033300050616     C                   write     lrv5RI
033400050616     c
033500050616     c* stampo totali per linea di arrivo
033600050616     c                   z-add     1             xx
033700050616     c                   dow       ARr2l(XX)>0
033800050616     c                   movel     arr2l(XX)     p5clna
033900050616     c     p5clna        chain     azorg01l
034000050616     C                   IF        %FOUND(AZORG01L)
034100050616     C                   MOVEL     ORGDES        P5Dlna
034200050616     C                   ELSE
034300050616     C                   MOVEL     *ALL'?'       P5Dlna
034400050616     C                   ENDIF
034500050616     c
034600050616     c                   setoff                                       31
034700050616     c* Imposto i campi dei totali: se tutti a 0 e p.o. in £6 non stampo
034800050616     c                   if        ncl(XX)=0 and pkf(XX)=0 and vlf(XX)=0
034900050616     c     Arr2l(XX)     lookup    T06                                    31
035000050616     c                   endif
035100050616     c
035200050616     c                   if        not *in31
035300050616     c                   eval      p5cncl=ncl(XX)
035400050616     c                   eval      p5cnclabi=nclabi(XX)
035500050616     c                   eval      p5cpkf=pkf(XX)
035600050616     c                   eval      p5cpkfabi=pkfabi(XX)
035700050616     c                   eval      p5cvlf=vlf(XX)
035800050616     c                   eval      p5cvlfabi=vlfabi(XX)
035900060919     c                   eval      p5cspe=spe(XX)
036000060919     c                   eval      p5cspeabi=speabi(XX)
036100050616     c* Stampo riga di arrivo
036200050616     c                   write     lrv5TG
036300050616     C                   write     lrv5RI
036400050616     c                   endif
036500050616     c
036600050616     c                   add       1             xx
036700050616     c                   enddo
036800091022     c
036900091022     c* se richiesto stampo anche i transiti per terminal di arrivo
037000091022     c                   if        p05tra='S'
037100091022     c                   exsr      StaTrans
037200091022     c                   endif
037300050616     c
037400050616     c* fine stampa
037500050616     C                   write     lrv5FI
037600050616     c                   ENDSR
037700091022     C**************************************************************************
037800091022     C* STAMPO STATISTICA  per terminal di arrivo per i transiti
037900091022     C**************************************************************************
038000091022     C     STATrans      BEGSR
038100091023     c                   seton                                        15
038200091023     c                   clear                   p5cnclabi
038300091023     c                   clear                   p5cpkfabi
038400091023     c                   clear                   p5cvlfabi
038500091023     c                   clear                   p5cspeabi
038600091022     c
038700091022     c* Stampo 2 parte testata con i fogli viaggio partenza abbinati
038800091104     c***                z-add     1             yy
038900091104     c***                dow       stafva(YY)<>*blanks
039000091104     c***                movel     stafva(YY)    p5dfva
039100091104     C***                write     lrv5pt2
039200091022     c
039300091104     c***                add       1             yy
039400091104     c***                enddo
039500091104     c***                endif
039600091022     c
039700091022     c
039800091023     c* Stampo totali per terminal di arrivo
039900091023     c                   do        990           xx
040000091023     c                   if        skbtpspe(XX)>0
040100091023     c                   eval      p5clna=xx
040200091023     c     p5clna        chain     azorg01l
040300091023     c                   IF        %FOUND(AZORG01L)
040400091023     c                   MOVEL     ORGDES        P5Dlna
040500091023     c                   ELSE
040600091023     c                   MOVEL     *ALL'?'       P5Dlna
040700091023     c                   ENDIF
040800091023     c
040900091023     c                   eval      p5cncl=skbtpncl(XX)
041000091023     c                   eval      p5cpkf=skbtppes(XX)
041100091023     c                   eval      p5cvlf=skbtpvol(XX)
041200091023     c                   eval      p5cspe=skbtpspe(XX)
041300091104     C* STAMPA TESTATA
041400091104     c                   if        *in90
041500091104     C                   write     lrv5pt
041600091104     C                   write     lrv5pI
041700091104     C                   write     lrv5RI
041800091104     c                   setoff                                       90
041900091104     c                   endif
042000091023
042100091023     c* Stampo riga per terminal di arrivo
042200091022     c                   write     lrv5TG
042300091022     C                   write     lrv5RI
042400091022     c                   endif
042500091022     c
042600091022     c                   enddo
042700091022     c
042800091022     c                   ENDSR
