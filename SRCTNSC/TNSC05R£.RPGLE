000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200890809     F*--------------------------------------------------------------* H
000300060921     F*        Tnsc05r -  elimina dati pagamento x mittenti         **
000400890809     F*--------------------------------------------------------------*
000500941028     FTNCSB03L  UF   E           K DISK
000900941028     FTNCSA01L  IF   E           K DISK
001000060921     FTNSC05D   CF   E             WORKSTN
001200941028     D*--------------------------------------------------------------*
001300000000     D KPJBA         E DS
002000970408     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003000941028     D WLBDA8          DS
003100941028     D  G02DAT                 1      8  0
003200941028     D  G02INV                 9     16  0
003300941028     D  G02ERR                17     17
003400941028     D  G02TGI                18     22  0
003600971218     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003700971218     D CNACO         E DS                  EXTNAME(CNACO00F)
003800971218     D CNIND         E DS                  EXTNAME(CNIND00F)
003900971218     D CNCLP         E DS                  EXTNAME(CNCLP00F)
004000971218     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004001060921     d dtaiso          s               d
004002060921     d dtaeur          s               d   datfmt(*eur)
004100000000     C*---------------------------------------------------------------*
004300000000     C                   Z-ADD     0             CONLNP
004400000000     C                   Z-ADD     0             CONNSP
004500891113     C                   Z-ADD     0             CONNRS
004600060921     C                   do        *hival
004700941028     C                   EXFMT     SC02D01
004800941028     C     *INKC         IFEQ      '1'
004900941028     C                   Z-ADD     0             CONLNP
005000941028     C     KCON          CHAIN     TNCSB000                           99
005100941028     C     KCSA          CHAIN     TNCSA000                           99
005200060921     C                   leave
005300941028     C                   END
005301060921     c                   exsr      srctr
005302060921     c   88              iter
005303060921     c                   do        *hival
005304060921     c                   move      csbddp        dtaiso
005305060921     c                   move      dtaiso        dtaeur
005306060921     c                   move      dtaeur        ddpv
005307060921     C                   EXFMT     SC02D02
005308060921     c                   if        *inkc or *inkl
005309060921     C                   leave
005310060921     C                   end
005311060921     C  NKF              iter
005312060921     c                   clear                   csbddp
005313060921     c                   clear                   csbbna
005314060921     c                   clear                   csbndp
005315060921     C                   EXCept    agg
005316060921     C                   Z-ADD     0             CONLNP
005317060921     C                   Z-ADD     0             CONNSP
005318060921     C                   Z-ADD     0             CONNRS
005319060921     c                   leave
005320060921     c                   enddo
005321060921     c                   if        *inkc
005322060921     C                   leave
005323060921     C                   end
005324060921     c                   enddo
005325060921     C                   CLEAR                   TIBS69
005326060921     C                   MOVEL     'C'           I69TLA
005327060921     C                   CALL      'TIBS69R'
005328060921     C                   PARM                    TIBS69
005329060921     C                   PARM                    CNACO
005330060921     C                   PARM                    CNIND
005331060921     C                   PARM                    CNCLP
005332060921     C                   PARM                    FNCLS
005333060921     C                   SETON                                        LR
005334060921     C*---------------------------------------------------------------*
005335060921     c     srctr         begsr
005336060921     C*---------------------------------------------------------------*
005400941028     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
005401060921     c                   setoff                                       88
005402060921     c                   do
005500941028     C                   MOVEL     CONAAS        WAA2              2 0
005600941028     C     WAA2          IFEQ      0
005700941028     C     CONAAS        IFGT      60
005800941028     C                   ADD       1900          CONAAS
005900941028     C                   ELSE
006000941028     C                   ADD       2000          CONAAS
006100941028     C                   END
006200941028     C                   END
006300891113     C* CONTROLLO ANNO SPEDIZIONE CHE SIA CORRETTO
006400941028     C                   MOVE      WHHDAT        UAAS              4 0
006500891113     C     CONAAS        COMP      UAAS                               98  83
006600891113     C  N98
006700891113     CANN83              SUB       1             UAAS
006701060921     c                   if        *in98
006702060921     c                   seton                                        88
006800060921     C                   leave
006801060921     c                   end
006900000000     C**
007000941028     C                   CLEAR                   WLBDA8
007100941028     C                   MOVE      WHHDAT        G02DAT
007200941028     C                   CALL      'XSRDA8'
007300941028     C                   PARM                    WLBDA8
007400941028     C                   Z-ADD     G02INV        RUDATE            8 0
007500000000     C** DATA SISTEMA AMG
007600941028     C     KCON          CHAIN     TNCSB000                           99
007601060921     c                   if        *in99
007602060921     c                   seton                                        88
007700060921     C                   leave
007701060921     c                   end
007702060921     c* controllo che sia un mittente, che ci siano i dati di pagamento e
007703060921     c* non la registrazione
007704060921     c                   if        csbtpi <> 'M' or csbddp = 0 or csbnrg<>0
007705060921     c                   seton                                        8897
007706060921     C                   leave
007707060921     c                   end
009600910912     C                   MOVE      CSBKSC        KSC4              4 0
009601060921     c                   select
009700060921     C                   when      ksc4 = 8888
009800941028     C     KCSA          CHAIN     TNCSA000                           10
009900910912     C  N10              MOVEL     CSARSD        RSD
010000000000     C   10              MOVEL     *BLANKS       RSD
010300060921     C                   when      ksc4 = 9999
010400941028     C     KCSA          CHAIN     TNCSA000                           10
010500910912     C  N10              MOVEL     CSARSD        RSD
010600000000     C   10              MOVEL     *BLANKS       RSD
010601060921     c                   other
010900000000     C** ACCESSO CNACO
011100971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE
011200971218     C                   CLEAR                   TIBS69
011300971218     C                   CLEAR                   CNACO
011400971218     C                   CLEAR                   CNIND
011500971218     C                   CLEAR                   CNCLP
011600971218     C                   CLEAR                   FNCLS
011700971218     C                   MOVEL     ' '           I69TLA
011800971218     C                   MOVEL     KNSIF         I69SIF
011900971218     C                   Z-ADD     CSBCDI        I69KAC
012000971218     C                   CALL      'TIBS69R'
012100971218     C                   PARM                    TIBS69
012200971218     C                   PARM                    CNACO
012300971218     C                   PARM                    CNIND
012400971218     C                   PARM                    CNCLP
012500971218     C                   PARM                    FNCLS
012600971218     C     O69ERR        IFEQ      *BLANK
012700971218     C                   MOVEL     ACORAG        RSD
012800971218     C                   ELSE
012900971218     C                   MOVEL     *BLANKS       RSD
013000971218     C                   ENDIF
013100060921     C                   endsl
013101060921     c                   enddo
017500060921     C                   endsr
017600941028     C*--------------------------------------------------------------*
017700941028     C*  OPERAZIONI INIZIALI
017800941028     C*--------------------------------------------------------------*
017900941028     C     *INZSR        BEGSR
018000941028     C*
018100941028     C     *ENTRY        PLIST
018200941028     C                   PARM                    KPJBA
018300941028     C                   Z-ADD     1             CODUT
018400941028     C                   CALL      'XPARUT'
018500941028     C                   PARM                    UTEDSE
018700941028     C                   MOVEL     RAGUT         RSUT             20
018800941028     C                   MOVEL     RAGUT         RSUT2            25
019800941028     C*---------------------------------------------------------------*
020400941028     C*  TNCSB
020500941028     C     KCON          KLIST
020600941028     C                   KFLD                    CONAAS
020700941028     C                   KFLD                    CONLNP
020800941028     C                   KFLD                    CONNRS
020900941028     C                   KFLD                    CONNSP
021000941028     C*  TNCSA
021100941028     C     KCSA          KLIST
021200941028     C                   KFLD                    CONAAS
021300941028     C                   KFLD                    CONLNP
021400941028     C                   KFLD                    CONNRS
021500941028     C                   KFLD                    CONNSP
022300941028     C                   SETOFF                                       99
022400941028     C                   TIME                    WHHDAT           14 0
022500941028     C                   MOVE      WHHDAT        CONAAS
022600941028     C*
022700890717     C                   ENDSR
022800890717     C*---------------------------------------------------------------*
022900941028     OTNCSB000  E            AGG
023000060921     O                       CSBbna
023100060921     O                       CSBndp
023200060921     O                       CSBddp
