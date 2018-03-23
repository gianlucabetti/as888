000100940224     H*--------------------------------------------------------------*
000200060320     FTITAS00F  iF   E             DISK
000201060320     Ffnevb01L  iF   E           k DISK
000202060320     Ffnarb01L  iF   E           k DISK
000203060321     FFIFCE01L  iF   E           k DISK
000204060321     FFIFCEnew  o  a E             DISK    rename(FIFCE000:FCENEW)
000300060320     Ffiapd01l  iF   E           k DISK
000301060321     Ffnarb998  iF   E             DISK    rename(fnarb998:ARB998)
000302060321     f                                     prefix(X)
000500900131     C*---------------------------------------------------------------*
000501060320     C* CHIAVE LETTURA FNAPD01L - COMPLETA
000502060320     C     KEYAPD        KLIST
000503060320     C                   KFLD                    APDTIP
000504060320     C                   KFLD                    APDPDR
000506060320     C*
000507060320     C     Kevb          KLIST
000510060320     C                   KFLD                    tasAAS
000511060320     C                   KFLD                    tasLNP
000512060320     C                   KFLD                    tasNRS
000513060320     C                   KFLD                    tasNSP
000514060321     C*
000515060321     C     Kxarb         KLIST
000516060321     C                   KFLD                    xarbAAS
000517060321     C                   KFLD                    xarbLNP
000518060321     C                   KFLD                    xarbNRS
000519060321     C                   KFLD                    xarbNSP
000520060320     C*
000521060320     C* CHIAVE LETTURA FIFCE01L - COMPLETA
000522060320     C     KEYFCE        KLIST
000523060320     C                   KFLD                    FCEFGS
000524060320     C                   KFLD                    FCEPDR
000525060320     C                   KFLD                    FCETSR
000526060320     C                   KFLD                    FCENDC
000527060320     C                   KFLD                    FCEDDC
000528060320     C                   KFLD                    FCEAAS
000529060320     C                   KFLD                    FCELNP
000530060320     C                   KFLD                    FCENRS
000531060320     C                   KFLD                    FCENSP
000532060320     C                   KFLD                    FCECCE
001700030205      *
001701060320      * Legge TITAS00F
001800030515     c                   do        *hival
001900060320     c                   read      titas00f
002000030515     c* fine file
002100060320     c                   if        %eoF(titas00f)
002200030515     c                   leave
002300030515     c                   end
002400030205     C*
002401060320     C* Controlla come sul conto economico
002402060320     c                   exsr      AGGFCE
003000041122     C*
003100030205     c                   enddo
003200000616     C*
003201060321      *-----------------------
003202060321      * Legge ARB998
003203060321     c                   do        *hival
003204060321     c                   read      fnarb998
003205060321     c* fine file
003206060321     c                   if        %eoF(fnarb998)
003207060321     c                   leave
003208060321     c                   end
003209060321     C*
003210060321     C* Controlla come sul conto economico
003211060321     c                   exsr      AGGFCE998
003212060321     C*
003213060321     c                   enddo
003214060321     C*
003300000000     C                   SETON                                        LR
003400060320     C*------------------------------------------------------------------------*
003500060320     C*?AGGFCE - AGGIORNA IL FILE FIFCE00F (CONTEGGI PADRONCINI)
003600060320     C*------------------------------------------------------------------------*
003700060320     C     AGGFCE        BEGSR
004100060320     C*---
004101060321     c                   exsr      RITIRI
004102060321     C*---
004103060321     c                   exsr      CONSEGNE
004104060321     C*
004105060321     C                   ENDSR
004106060321     C*------------------------------------------------------------------------*
004107060321     C*---
004108060321     C*------------------------------------------------------------------------*
004109060321     C     RITIRI        BEGSR
004110060321     C*---
004200060320     C* RITIRI
004400060320     C* . Controlla che il padroncino ritiro sia un dipendente
004500060320     C* . Controlla che la LNP sia uguale alla linea del padroncino
004600060320     C* . Scrive un record a zero nei conteggi padroncini solo se NON esiste già
004700060320     C*---
005000060320     C                   MOVEL     'A'           APDTIP
005100060320     C                   Z-ADD     TASPDR        APDPDR
005200060320     C     KEYAPD        CHAIN     FIAPD01L                           99
005300060320     C                   MOVEL     TASPDR        N3                3 0
005400060320IF  3C     *IN99         IFEQ      *OFF
005600060320     C     N3            ANDEQ     TASLNP
005700060320     c* creo la spia nel FIFCE se padroncino escluso da autofatturazione
005800060320     c* oppure se padroncino non escluso ma tipo bolla FY modifica
005900060320     c* il cod. autotrasp. con uno fittizio
006000060320   a3c                   if        apdpdd = 'S' or
006100060320     c                             (apdpdd <>'S' and tascbo = 'FY')
006200060320     C                   MOVEL     tasPDR        N3                3 0
006300060320     C                   Z-ADD     N3            FCEFGS
006400060320     c                   if        apdpdd = 'S'
006500060320     C                   Z-ADD     tasPDR        FCEPDR
006600060320     c                   else
006700060320     c                   movel     fcefgs        fcepdr
006800060320     c                   move      0998          fcepdr
006900060320     c                   end
007000060320     C                   MOVEL     'R'           FCETSR
007100060320     C                   EVAL      FCENDC = (5000 + tasNRT)
007200060320     C                   Z-ADD     tasDRT        FCEDDC
007300060320     C                   Z-ADD     tasAAS        FCEAAS
007400060320     C                   Z-ADD     tasLNP        FCELNP
007500060320     C                   Z-ADD     tasNRS        FCENRS
007600060320     C                   Z-ADD     tasNSP        FCENSP
007700060320     C                   MOVEL     '001'         FCECCE
007800060320     C     KEYFCE        CHAIN(N)  FIFCE01L                           99
007900060320IF  4C     *IN99         IFEQ      *ON
007901060320IF  4C     fceddc        andge     20060301
008000060320     C                   Z-ADD     *ZEROS        FCEICE
008100060320     C                   MOVEL     *BLANKS       FCESAL
008200060320     C                   MOVEL     tasCBO        FCECBO
008300060321     C                   WRITE     FCEnew
008301060321     C                   feod      FIFCEnew
008400060320E   4C                   ENDIF
008500060320E  a3C                   ENDIF
008600060320E   3C                   ENDIF
008601060321     C*
008602060321     C                   ENDSR
008603060321     C*------------------------------------------------------------------------*
008800060320     C*---
008801060321     C*------------------------------------------------------------------------*
008802060321     C     CONSEGNE      BEGSR
008803060321     C*---
008900060320     C* CONSEGNE
009000060320     C* . Gestisce solo le bolle consegnate tranne x i dirottamenti
009100060320     C* . che scrive la spia su fifce x pagare le competenze di consegna
009200060320     C* . anche alla bolla originale
009300060320     C* . Controlla se la variazione interessa (dopo l'ultima contabilzzazione)
009400060320     C* . Controlla che il padroncino consegna sia un dipendene
009500060320     C* . Controlla che la LNA sia uguale alla linea del padroncino
009600060320     C* . Scrive un record a zero nei conteggi padroncini solo se NON esiste già
010001060321     C*---
010002060321      *    se dirottata o resa deve andare su ARB
010003060321     c                   if        tascca = '1' or tascca = '2'
010004060321     c     kevb          chain     fnarb01l
010005060321     c                   if        %Found(fnarb01l)
010007060321     C                   z-add     arbpdc        tasPDC
010008060321     C                   Z-ADD     arbndc        tasNDC
010009060321     C                   Z-ADD     arbddc        tasDDC
010010060321     c                   else
010011060321      * se non trovato ARB e dirottata ci prova con EVB
010013060321     c                   if        tascca = '1'
010018060321     c     kevb          setll     fnevb01l
010019060321     c     kevb          reade     fnevb01l
010020060321     c                   dow       not %Eof(fnevb01l)
010021060321     c                   if        evbcev='DIR'
010022060321     C                   Z-ADD     evbDEV        tasDDC
010023060321     C                   movel     tasLNA        taspdc
010024060321     C                   move      '0998'        taspdc
010025060321     C                   Z-ADD     999999999     tasNDC
010026060321     c                   leave
010027060321     c                   end
010028060321     c     kevb          reade     fnevb01l
010029060321     c                   end
010030060321     c                   end
010031060321     c                   end
010032060321      *
010033060321     c                   end
010034060321     C*---
010300060320     C                   MOVEL     'A'           APDTIP
010400060320     C                   Z-ADD     tasPDC        APDPDR
010500060320     C     KEYAPD        CHAIN     FIAPD01L                           99
010600060320     C                   MOVEL     tasPDC        N3                3 0
010601060320      *
010602060320      * con padroncino fittizio
010700060320IF  4C     *IN99         IFEQ      *OFF
010800060320     C     APDPDD        ANDEQ     'S'
010900060320     C     N3            ANDEQ     tasLNA
010901060320      *
010902060320      * dirottate con padroncino
010903060320IF  4C     *IN99         oreq      *OFF
010904060320     C     APDPDD        andne     'S'
010905060320     C     tascca        andeq     '1'
010906060321
010907060321      * resi mittenti
010908060320IF  4C     *IN99         oreq      *OFF
010909060320     C     APDPDD        andne     'S'
010910060320     C     tascca        andeq     '2'
010911060320      *
011200060320     C                   MOVEL     tasPDC        N3                3 0
011300060320     C                   Z-ADD     N3            FCEFGS
011400060320     C                   Z-ADD     tasPDC        FCEPDR
011500060320     C                   MOVEL     'C'           FCETSR
011600060320     C                   Z-ADD     tasNDC        FCENDC
011601060320     C                   Z-ADD     tasDDC        FCEDDC
011620060320      *
011800060320     C                   Z-ADD     tasAAS        FCEAAS
011900060320     C                   Z-ADD     tasLNP        FCELNP
012000060320     C                   Z-ADD     tasNRS        FCENRS
012100060320     C                   Z-ADD     tasNSP        FCENSP
012200060320     C                   MOVEL     '005'         FCECCE
012201060321      *
012300060320     C     KEYFCE        CHAIN(N)  FIFCE01L                           99
012400060320IF  6C     *IN99         IFEQ      *ON
012401060320IF  4C     fceddc        andge     20060301
012500060320     C                   Z-ADD     *ZEROS        FCEICE
012600060320     C                   MOVEL     *BLANKS       FCESAL
012700060320     C                   MOVEL     tasCBO        FCECBO
012800060321     C                   WRITE     FCEnew
012801060321     C                   feod      FIFCEnew
012900060320E   6C                   ENDIF
012901060321     C*
013300060320E   2C                   ENDIF
013600060320     C*
013700060320     C                   ENDSR
013701060321     C*------------------------------------------------------------------------*
013702060321     C*?AGGFCE998 solo da ARB e non da TITAS x bolle precedenti
013703060321     C*------------------------------------------------------------------------*
013704060321     C     AGGFCE998     BEGSR
013705060321     C*---
013707060321     C* CONSEGNE
013708060321     C* . Gestisce solo le bolle consegnate tranne x i dirottamenti
013709060321     C* . che scrive la spia su fifce x pagare le competenze di consegna
013710060321     C* . anche alla bolla originale
013711060321     C* . Controlla se la variazione interessa (dopo l'ultima contabilzzazione)
013712060321     C* . Controlla che il padroncino consegna sia un dipendene
013713060321     C* . Controlla che la LNA sia uguale alla linea del padroncino
013714060321     C* . Scrive un record a zero nei conteggi padroncini solo se NON esiste già
013715060321     C*---
013723060321     c     kxarb         chain     fnarb01l
013724060321     c                   if        %Found(fnarb01l)
013725060321     C                   z-add     arbpdc        tasPDC
013726060321     C                   Z-ADD     arbndc        tasNDC
013727060321     C                   Z-ADD     arbddc        tasDDC
013728060321     c                   end
013742060321     C*---
013743060321     C                   MOVEL     'A'           APDTIP
013744060321     C                   Z-ADD     arbPDC        APDPDR
013745060321     C     KEYAPD        CHAIN     FIAPD01L                           99
013746060321     C                   MOVEL     arbPDC        N3                3 0
013747060321      *
013748060321      * con padroncino fittizio
013749060321IF  4C     *IN99         IFEQ      *OFF
013750060321     C     APDPDD        ANDEQ     'S'
013751060321     C     N3            ANDEQ     arbLNA
013752060321      *
013753060321      * dirottate con padroncino
013754060321IF  4C     *IN99         oreq      *OFF
013755060321     C     APDPDD        andne     'S'
013756060321     C     arbcca        andeq     '1'
013757060321
013758060321      * resi mittenti
013759060321IF  4C     *IN99         oreq      *OFF
013760060321     C     APDPDD        andne     'S'
013761060321     C     arbcca        andeq     '2'
013762060321      *
013763060321     C                   MOVEL     arbPDC        N3                3 0
013764060321     C                   Z-ADD     N3            FCEFGS
013765060321     C                   Z-ADD     arbPDC        FCEPDR
013766060321     C                   MOVEL     'C'           FCETSR
013767060321     C                   Z-ADD     arbNDC        FCENDC
013768060321     C                   Z-ADD     arbDDC        FCEDDC
013769060321      *
013770060321     C                   Z-ADD     arbAAS        FCEAAS
013771060321     C                   Z-ADD     arbLNP        FCELNP
013772060321     C                   Z-ADD     arbNRS        FCENRS
013773060321     C                   Z-ADD     arbNSP        FCENSP
013774060321     C                   MOVEL     '005'         FCECCE
013775060321      *
013776060321     C     KEYFCE        CHAIN(N)  FIFCE01L                           99
013777060321IF  6C     *IN99         IFEQ      *ON
013778060321IF  4C     fceddc        andge     20060301
013779060321     C                   Z-ADD     *ZEROS        FCEICE
013780060321     C                   MOVEL     *BLANKS       FCESAL
013781060321     C                   MOVEL     arbCBO        FCECBO
013782060321     C                   WRITE     FCEnew
013783060321     C                   feod      FIFCEnew
013784060321E   6C                   ENDIF
013785060321     C*
013786060321E   2C                   ENDIF
013787060321     C*
013788060321     C*
013789060321     C                   ENDSR
013800060320     C*------------------------------------------------------------------------*
