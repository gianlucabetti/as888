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
005900120822     d dataiso         s               d   datfmt(*iso)
006000040604     D CNCR80        E DS
006100040604     D KPJBA         E DS
006200030722     D UT§DSE0F      E DS
006300911010     C*---------------------------------------------------------------*
006400000000     C     *ENTRY        PLIST
006500000000     C                   PARM                    KPJBA
006600050616     c                   movel     kpjbu         para05
006700900511     C*---------------------------------------------------------------*
006800091026     c* Impostazioni e caricamenti di comodo
006900050616     C                   EXSR      CARImpo
007000091022     c* Dal fogLio arrivi leggo i fogli abbinati e relative bolle
007100050616     C                   EXSR      CARFoglipar
007200050616     c* Stampo la statistica
007300050616     C                   EXSR      STAMPA
007400960202     C*
007500000000     C                   SETON                                        LR
007600960205     C**************************************************************************
007700050616     C*    Impostazioni e caricamenti di comodo
007800960205     C**************************************************************************
007900050616     c     CARIMPO       begsr
008000050616     C****  KLIST  ****
008100050616     C     KARB          KLIST
008200050616     C                   KFLD                    fvalnp
008300050616     C                   KFLD                    fvanfv
008400091022     C     Kbtp          KLIST
008500091022     C                   KFLD                    p05pof
008600091022     C                   KFLD                    fvalnp
008700091022     C                   KFLD                    fvanfv
008800091026     C     KFVA2         KLIST                                                  FNFVA02L
008900091026     C                   KFLD                    KNPG                           ..CATEGORIA
009000091026     C                   KFLD                    kNFa                           ..FOGLIO
009100091026     C                   KFLD                    p05pof
009200060920     C     KFGV          KLIST                                                  FNFVA02L
009300060920     C                   KFLD                    knfv                           ..FOGLIO
009400060920     C                   KFLD                    p05pof                         ..CATEGORIA
009500050616     c
009600050616     C                   Z-ADD     1             CODUT
009700050616     C                   CALL      'X§PARUT'
009800050616     C                   PARM                    UT§DSE0F
009900050616     C                   MOVEL     REC80         CNCR80
010000941216     C*
010100930519     C* DATA FOGLIO ARRIVI
010200050616     C                   MOVE      p05DFV        G02INV
010300920709     C                   MOVE      *ZEROS        G02DAT
010400920709     C                   MOVEL     3             G02ERR
010500941221     C                   CALL      'XSRDA8'
010600920709     C                   PARM                    WLBDAT
010700050616     C                   Z-ADD     G02DAT        V1cDFA
010800050616     C
010900050616     C* DECODIFICA P.O. FOGLIO
011000060919     C     P05pof        CHAIN     AZORG01L
011100050616     C                   IF        %FOUND(AZORG01L)
011200060919     C                   MOVEL     ORGDES        P5Dpof
011300050616     C                   ELSE
011400060919     C                   MOVEL     *ALL'?'       P5Dpof
011500050616     C                   ENDIF
011600060919     C* DECODIFICA P.O. in gestione
011700060919     C     P05fgs        CHAIN     AZORG01L
011800060919     C                   IF        %FOUND(AZORG01L)
011900060919     C                   MOVEL     ORGDES        P5Dfgs
012000060919     C                   ELSE
012100060919     C                   MOVEL     *ALL'?'       P5Dfgs
012200060919     C                   ENDIF
012300091026     c*
012400091026     c* decodifico dati transito SI/NO
012500091026     c                   if        p05tra='S'
012600091026     c                   eval      v1ctrans='SI'
012700091026     c                   else
012800091026     c                   eval      v1ctrans='NO'
012900091026     c                   endif
013000050616     c
013100050616     c* Alla data foglio arrivi carico le possibili linee di cui è terminal
013200050616     c*  il p.o. gestione
013300060919     c                   Clear                   Trul09ds
013400060919     c                   if        p05fgs=p05pof
013500060919     c
013600050616     c                   Eval      I09Mod = 'TFU'
013700050616     c                   Eval      I09Tfa = p05fgs
013800120822     c* ES 22/08/12 passo la data foglio -1 per evitare problemi in casi di cambio termianl
013900120822     c                   movel     p05dfv        dataiso
014000120822     c                   subdur    1:*d          dataiso
014100120822     c                   movel     dataiso       i09dta
014200120822     c***                Eval      I09dta = p05dfv
014300050616     c                   Call      'TRUL09R'
014400050616     c                   Parm                    Trul09ds
014500060919     c*
014600060919     c                   else
014700060919     C*
014800060919     C* Carico i p.o. della l6 del p.o. in gestione se <> dal p.o. foglio
014900060919     c                   clear                   kpjbu
015000060919     c                   clear                   trul06ds
015100060919     c                   eval      d06cod = '£6'
015200060919     c                   movel     p05fgs        d06key
015300060919     c                   movel     trul06ds      kpjbu
015400060919     c                   call      'TRUL06R'
015500060919     c                   parm                    kpjba
015600060919     c                   movel     kpjbu         trul06ds
015700060919     c                   movea     t06           Arr2l
015800060919     c                   endif
015900911008     C*
016000050616     C* Carico tutti i p.o. gestiti
016100050616     c                   clear                   kpjbu
016200050616     c                   clear                   trul06ds
016300050616     c                   eval      d06cod = '£6'
016400050616     c                   movel     p05fgs        d06key
016500050616     c                   eval      d06esc = 'G'
016600060919     c                   eval      d06tla = 'L'
016700050616     c                   movel     trul06ds      kpjbu
016800050616     c                   call      'TRUL06R'
016900050616     c                   parm                    kpjba
017000050616     c                   movel     kpjbu         trul06ds
017100050616     c
017200050616     c                   ENDSR
017300050616     C**************************************************************************
017400050616     C*    Letto fogli partenza abbinati al foglio arrivi
017500050616     C**************************************************************************
017600050616     c     CARFoglipar   begsr
017700091026     c
017800091026     c                   setoff                                       16
017900091026     c
018000060920     c                   z-add     1             lo                2 0
018100060920     c                   z-add     1             zz                4 0
018200050616     c                   z-add     1             yy                4 0
018300060920     c* Leggo prima e carico i dati dei fogli locali
018400060920     c                   movea     p05loc        nfvloc
018500060920     c
018600060920     c                   do        5             lo
018700060920     c                   if        nfvloc(lo)>*zeros
018800060920     c                   movel     nfvloc(lo)    knfv
018900060920     c
019000060920     c     kfgv          chain     fnfgv01l
019100060920     c                   if        %found(fnfgv01l)
019200060920     c                   eval      fvalnp=fgvlnp
019300060920     c                   eval      fvanfv=fgvnfv
019400060920     c                   exsr      ELABORA
019500060920     c                   endif
019600060920     c                   endif
019700060920     c                   enddo
019800060920     c
019900091026     c                   eval      knpg=p05npg
020000091026     c                   eval      knfa=p05nfv
020100060919     c
020200941221     C     KFVA2         SETLL     FNFVA02L
020300091026     C     KFVA2         READE     FNFVA02L                               30
020400091026
020500091026     c* se non c'e' nessun foglio abbinato, stampo i dati dei fogli ancora da abbinare
020600091026     c                   if        *in30
020700091026     c                   clear                   knpg
020800091026     c                   clear                   knfa
020900091026     c                   seton                                        16
021000091026     C     KFVA2         SETLL     FNFVA02L
021100091026     C     KFVA2         READE     FNFVA02L                               30
021200091026     c                   endif
021300091026     c
021400091026     c                   dow       not *in30
021500060920     c                   exsr      ELABORA
021600050616     c
021700091026     C     KFVA2         READE     FNFVA02L                               30
021800050616     c                   enddo
021900050616     c
022000050616     c                   if        p5dfva<>*Blanks
022100050616     c                   movel     p5dfva        stafva(YY)
022200050616     c                   endif
022300960205     C*
022400930127     C                   ENDSR
022500060920     C**************************************************************************
022600060920     C* elabora foglio viaggio partenza per totali
022700060920     C**************************************************************************
022800060920     c     ELABORa       begsr
022900060920     c
023000060920     c* Imposto il numero del foglio nelle riga di stampa
023100060920     c                   movel     fvalnp        w003a             3
023200060920     c                   eval      %subst(p5dfva:zz:3)=w003a
023300060920     c                   add       3             zz
023400060920     c                   eval      %subst(p5dfva:zz:1)='-'
023500060920     c                   add       1             zz
023600060920     c                   eval      %subst(p5dfva:zz:6)=(%editc(fvanfv:'2'))
023700060920     c                   add       8             zz
023800060920     c
023900060920     c* Se non ce ne stanno + memorizzo la riga di stampa e riparto da 1
024000060920     c                   if        zz>123
024100060920     c                   movel     p5dfva        stafva(YY)
024200060920     c                   add       1             yy
024300060920     c                   clear                   p5dfva
024400060920     c                   z-add     1             zz
024500060920     c                   endif
024600060920     c
024700060920     c
024800060920     c     karb          setll     fnarb06l
024900060920     c     karb          reade     fnarb06l
025000060920     c                   dow       not %eof(fnarb06l)
025100060920     c
025200060920     c* totalizzo per linea di arrivo colli peso e volumi
025300060920     c*  escludo transiti
025400161024     C     ARBNCR        IFEQ      ARBNCL
025500161024     C                   Z-ADD     ARBVLC        ARBVLF                         VOLUME RILEVATO
025600161024     C                   ELSE
025700161024     C     ARBVLC        IFGE      ARBVLF
025800161024     C                   Z-ADD     ARBVLC        ARBVLF                         VOLUME RILEVATO
025900161024     C                   ENDIF
026000161024     C                   ENDIF
026100161024     C     ARBNCp        IFEQ      ARBNCL
026200161024     C                   Z-ADD     ARBpkC        ARBpkF                         VOLUME RILEVATO
026300161024     C                   ELSE
026400161024     C     ARBpkC        IFGE      ARBpkF
026500161024     C                   Z-ADD     ARBpkC        ARBpkF                         VOLUME RILEVATO
026600161024     C                   ENDIF
026700161024     C                   ENDIF
026800060920     c                   z-add     1             xx                4 0
026900060920     c     arblna        lookup    Arr2l(XX)                              30
027000060920     c                   if        *in30
027100060920     c                   add       arbncl        ncl(xx)
027200060920     c                   add       arbvlf        vlf(xx)
027300060920     c                   add       arbpkf        pkf(xx)
027400060920     C                   ADD       1             SPE(XX)
027500060920     c* totale a parte per le abilitate (partite)
027600060920     c                   if        arbama>0
027700060920     c                   add       arbncl        nclabi(xx)
027800060920     c                   add       arbvlf        vlfabi(xx)
027900060920     c                   add       arbpkf        pkfabi(xx)
028000060920     C                   ADD       1             SPEabi(XX)
028100060920     c                   endif
028200060920     c                   endif
028300060920     c
028400060920     c     karb          reade     fnarb06l
028500060920     c                   enddo
028600091022     c
028700091022     c* Se richiesto elaboro anche le bolle transito per terminal di arrivo
028800091022     c                   if        p05tra='S'
028900091022     c                   exsr      ElaTrans
029000091022     c                   endif
029100091022     c
029200060920     c                   ENDSR
029300091022     C**************************************************************************
029400091022     C* ELABORO BOLLE TRANSITO
029500091022     C**************************************************************************
029600091022     C     ELATRANS      BEGSR
029700091022     c     kbtp          setll     fnbtp06l
029800091022     c     kbtp          reade     fnbtp06l
029900091022     c                   dow       not %eof(fnbtp06l)
030000091022     c* Mewmorizzo i dati
030100091022     c* Volume--> il VDL se totale altrimenti il bollettato
030200091022     c                   if        btpncr=btpncl
030300091022     c                   eval      wvol=btpvlc
030400091022     c                   else
030500091022     c                   if        btpvlc>btpvlb
030600091022     c                   eval      wvol=btpvlc
030700091022     c                   else
030800091022     c                   eval      wvol=btpvlb
030900091022     c                   endif
031000091022     c                   endif
031100091022     c                   if        btpncp=btpncl
031200091022     c                   eval      wpes=btppkc
031300091022     c                   else
031400091022     c                   if        btpvlc>btpvlb
031500091022     c                   eval      wpes=btppkc
031600091022     c                   else
031700091022     c                   eval      wpes=btppkb
031800091022     c                   endif
031900091022     c                   endif
032000091022     c
032100091022     C                   eval      skbtpvol(btpTFA) = skbtpvol(btpTFA) + wvol
032200091022     C                   eval      skbtppes(btpTFA) = skbtppes(btpTFA) + wpes
032300091023     C                   eval      skbtpncl(btpTFA) = skbtpncl(btpTFA) + btpncl
032400091022     C                   eval      skbtpspe(btpTFA) = skbtpspe(btpTFA) + 1
032500091022     c
032600091022     c     kbtp          reade     fnbtp06l
032700091022     c                   enddo
032800091022     c
032900091022     c                   ENDSR
033000040604     C**************************************************************************
033100050616     C* STAMPO STATISTICA
033200040604     C**************************************************************************
033300040604     C     STAMPA        BEGSR
033400091023     c                   setoff                                       15
033500050616     c* Imposta il campo systema informativo/p.o. simfel
033600050617     c                   eval      p5csyf=%subst(ragut:1:25)
033700040604     C* STAMPA TESTATA
033800050616     C                   write     lrv5pt
033900091104     c                   setoff                                       90
034000050616     c
034100050616     c* Stampo 2 parte testata con i fogli viaggio partenza abbinati
034200050616     c                   z-add     1             yy
034300050616     c                   dow       stafva(YY)<>*blanks
034400050616     c                   movel     stafva(YY)    p5dfva
034500050616     C                   write     lrv5pt2
034600050616     c
034700050616     c                   add       1             yy
034800050616     c                   enddo
034900050616     c
035000050616     c* Stampo 3 parte testata
035100050616     C                   write     lrv5pI
035200050616     C                   write     lrv5RI
035300050616     c
035400050616     c* stampo totali per linea di arrivo
035500050616     c                   z-add     1             xx
035600050616     c                   dow       ARr2l(XX)>0
035700050616     c                   movel     arr2l(XX)     p5clna
035800050616     c     p5clna        chain     azorg01l
035900050616     C                   IF        %FOUND(AZORG01L)
036000050616     C                   MOVEL     ORGDES        P5Dlna
036100050616     C                   ELSE
036200050616     C                   MOVEL     *ALL'?'       P5Dlna
036300050616     C                   ENDIF
036400050616     c
036500050616     c                   setoff                                       31
036600050616     c* Imposto i campi dei totali: se tutti a 0 e p.o. in £6 non stampo
036700050616     c                   if        ncl(XX)=0 and pkf(XX)=0 and vlf(XX)=0
036800050616     c     Arr2l(XX)     lookup    T06                                    31
036900050616     c                   endif
037000050616     c
037100050616     c                   if        not *in31
037200050616     c                   eval      p5cncl=ncl(XX)
037300050616     c                   eval      p5cnclabi=nclabi(XX)
037400050616     c                   eval      p5cpkf=pkf(XX)
037500050616     c                   eval      p5cpkfabi=pkfabi(XX)
037600050616     c                   eval      p5cvlf=vlf(XX)
037700050616     c                   eval      p5cvlfabi=vlfabi(XX)
037800060919     c                   eval      p5cspe=spe(XX)
037900060919     c                   eval      p5cspeabi=speabi(XX)
038000050616     c* Stampo riga di arrivo
038100050616     c                   write     lrv5TG
038200050616     C                   write     lrv5RI
038300050616     c                   endif
038400050616     c
038500050616     c                   add       1             xx
038600050616     c                   enddo
038700091022     c
038800091022     c* se richiesto stampo anche i transiti per terminal di arrivo
038900091022     c                   if        p05tra='S'
039000091022     c                   exsr      StaTrans
039100091022     c                   endif
039200050616     c
039300050616     c* fine stampa
039400050616     C                   write     lrv5FI
039500050616     c                   ENDSR
039600091022     C**************************************************************************
039700091022     C* STAMPO STATISTICA  per terminal di arrivo per i transiti
039800091022     C**************************************************************************
039900091022     C     STATrans      BEGSR
040000091023     c                   seton                                        15
040100091023     c                   clear                   p5cnclabi
040200091023     c                   clear                   p5cpkfabi
040300091023     c                   clear                   p5cvlfabi
040400091023     c                   clear                   p5cspeabi
040500091022     c
040600091022     c* Stampo 2 parte testata con i fogli viaggio partenza abbinati
040700091104     c***                z-add     1             yy
040800091104     c***                dow       stafva(YY)<>*blanks
040900091104     c***                movel     stafva(YY)    p5dfva
041000091104     C***                write     lrv5pt2
041100091022     c
041200091104     c***                add       1             yy
041300091104     c***                enddo
041400091104     c***                endif
041500091022     c
041600091022     c
041700091023     c* Stampo totali per terminal di arrivo
041800091023     c                   do        990           xx
041900091023     c                   if        skbtpspe(XX)>0
042000091023     c                   eval      p5clna=xx
042100091023     c     p5clna        chain     azorg01l
042200091023     c                   IF        %FOUND(AZORG01L)
042300091023     c                   MOVEL     ORGDES        P5Dlna
042400091023     c                   ELSE
042500091023     c                   MOVEL     *ALL'?'       P5Dlna
042600091023     c                   ENDIF
042700091023     c
042800091023     c                   eval      p5cncl=skbtpncl(XX)
042900091023     c                   eval      p5cpkf=skbtppes(XX)
043000091023     c                   eval      p5cvlf=skbtpvol(XX)
043100091023     c                   eval      p5cspe=skbtpspe(XX)
043200091104     C* STAMPA TESTATA
043300091104     c                   if        *in90
043400091104     C                   write     lrv5pt
043500091104     C                   write     lrv5pI
043600091104     C                   write     lrv5RI
043700091104     c                   setoff                                       90
043800091104     c                   endif
043900091023
044000091023     c* Stampo riga per terminal di arrivo
044100091022     c                   write     lrv5TG
044200091022     C                   write     lrv5RI
044300091022     c                   endif
044400091022     c
044500091022     c                   enddo
044600091022     c
044700091022     c                   ENDSR
