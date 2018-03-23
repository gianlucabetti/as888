000100940224     H*--------------------------------------------------------------*
000200060320     FTITAS00F  iF   E             DISK
000201060320     Ffnevb01L  iF   E           k DISK
000202060320     Ffnarb01L  iF   E           k DISK
000203060321     FFIFCE01L  iF   E           k DISK
000204060321     FFIFCEnew  o  a E             DISK    rename(FIFCE000:FCENEW)
000300060320     Ffiapd01l  iF   E           k DISK
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
000514060320     C*
000515060320     C* CHIAVE LETTURA FIFCE01L - COMPLETA
000516060320     C     KEYFCE        KLIST
000517060320     C                   KFLD                    FCEFGS
000518060320     C                   KFLD                    FCEPDR
000519060320     C                   KFLD                    FCETSR
000520060320     C                   KFLD                    FCENDC
000521060320     C                   KFLD                    FCEDDC
000522060320     C                   KFLD                    FCEAAS
000523060320     C                   KFLD                    FCELNP
000524060320     C                   KFLD                    FCENRS
000525060320     C                   KFLD                    FCENSP
000526060320     C                   KFLD                    FCECCE
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
003300000000     C                   SETON                                        LR
003400060320     C*------------------------------------------------------------------------*
003500060320     C*?AGGFCE - AGGIORNA IL FILE FIFCE00F (CONTEGGI PADRONCINI)
003600060320     C*------------------------------------------------------------------------*
003700060320     C     AGGFCE        BEGSR
004100060320     C*---
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
008400060320E   4C                   ENDIF
008500060320E  a3C                   ENDIF
008600060320E   3C                   ENDIF
008800060320     C*---
008900060320     C* CONSEGNE
009000060320     C* . Gestisce solo le bolle consegnate tranne x i dirottamenti
009100060320     C* . che scrive la spia su fifce x pagare le competenze di consegna
009200060320     C* . anche alla bolla originale
009300060320     C* . Controlla se la variazione interessa (dopo l'ultima contabilzzazione)
009400060320     C* . Controlla che il padroncino consegna sia un dipendene
009500060320     C* . Controlla che la LNA sia uguale alla linea del padroncino
009600060320     C* . Scrive un record a zero nei conteggi padroncini solo se NON esiste già
009700060320     C*---
009800060320IF  2C                   IF        tasdcm > 0 or (
009900060320IF  2C                             tasdcm = 0 and tascca = '1' and
010000060320IF  2C                              tasddc > 0 )
010001060321     C*---
010002060321      *    se dirottata o resa deve andare su ARB
010003060321     c                   if        tascca = '1' or tascca = '2'
010004060321     c     kevb          chain     fnarb01l
010005060321     c                   if        %Found(fnarb01l)
010007060321     C                   z-add     arbpdc        tasPDC
010008060321     C                   Z-ADD     arbndc        tasNDC
010009060321     C                   Z-ADD     arbddc        tasDDC
010010060321     c                   end
010011060321     c                   end
010012060321     C*---
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
010907060320      * resmi mittenti
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
011602060320      * se dirottata
011603060320     c                   goto      nonfare
011604060320     c                   if        tascca = '1' and APDPDD <> 'S'
011605060320     C                   movel     tasLNA        FCEFGS
011606060320     C                   movel     tasLNA        FCEPDR
011607060320     C                   move      '0998'        FCEPDR
011608060320     C                   Z-ADD     999999999     FCENDC
011609060320     c     kevb          setll     fnevb01l
011610060320     c     kevb          reade     fnevb01l
011611060320     c                   dow       not %Eof(fnevb01l)
011612060320     c                   if        evbcev='DIR'
011613060320     C                   Z-ADD     evbDEV        FCEDDC
011614060320     c                   leave
011615060320     c                   end
011616060320     c     kevb          reade     fnevb01l
011617060320     c                   end
011618060320     c                   end
011619060320     c     nonfare       tag
011620060320      *
011621060321      * se reso mittente o dirottata
011625060320     c                   if        tascca = '2'  or
011626060320     c                             tascca = '1'
011627060320     c     kevb          chain     fnarb01l
011628060320     c                   if        %Found(fnarb01l)
011629060320     C                   movel     arbpdc        FCEFGS
011630060320     C                   z-add     arbpdc        FCEPDR
011631060320     C                   Z-ADD     arbndc        FCENDC
011632060320     C                   Z-ADD     arbddc        FCEDDC
011636060320     c                   end
011637060320     c                   end
011638060320      *
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
012900060320E   6C                   ENDIF
013200060320E   3C                   ENDIF
013300060320E   2C                   ENDIF
013600060320     C*
013700060320     C                   ENDSR
013800060320     C*------------------------------------------------------------------------*
