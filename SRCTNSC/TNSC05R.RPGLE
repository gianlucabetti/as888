000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200890809     F*--------------------------------------------------------------* H
000300060921     F*        Tnsc05r -  elimina dati pagamento x mittenti         **
000400890809     F*--------------------------------------------------------------*
000500941028     FTNCSB03L  UF   E           K DISK
000600120214     FTNCSb09L  uF   E           K DISK    rename(tncsb000: tncsb009)
000700941028     FTNCSA01L  IF   E           K DISK
000800060921     FTNSC05D   CF   E             WORKSTN
000900941028     D*--------------------------------------------------------------*
001000000000     D KPJBA         E DS
001100970408     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
001200941028     D WLBDA8          DS
001300941028     D  G02DAT                 1      8  0
001400941028     D  G02INV                 9     16  0
001500941028     D  G02ERR                17     17
001600941028     D  G02TGI                18     22  0
001700971218     D TIBS69        E DS                  EXTNAME(TIBS69DS)
001800971218     D CNACO         E DS                  EXTNAME(CNACO00F)
001900971218     D CNIND         E DS                  EXTNAME(CNIND00F)
002000971218     D CNCLP         E DS                  EXTNAME(CNCLP00F)
002100971218     D FNCLS         E DS                  EXTNAME(FNCLS00F)
002200120213     D tnsc30ds      E DS
002300060921     d dtaiso          s               d
002400060921     d dtaeur          s               d   datfmt(*eur)
002500120213     D comkey          s                   like(csbnra)
002600000000     C*---------------------------------------------------------------*
002700120214     c                   setoff                                       44
002800000000     C                   Z-ADD     0             CONLNP
002900000000     C                   Z-ADD     0             CONNSP
003000891113     C                   Z-ADD     0             CONNRS
003100060921     C                   do        *hival
003200941028     C                   EXFMT     SC02D01
003300941028     C     *INKC         IFEQ      '1'
003400941028     C                   Z-ADD     0             CONLNP
003500941028     C     KCON          CHAIN     TNCSB000                           99
003600941028     C     KCSA          CHAIN     TNCSA000                           99
003700060921     C                   leave
003800941028     C                   END
003900120214     c* contraolli
004000060921     c                   exsr      srctr
004100060921     c   88              iter
004200060921     c                   do        *hival
004300060921     c                   move      csbddp        dtaiso
004400060921     c                   move      dtaiso        dtaeur
004500060921     c                   move      dtaeur        ddpv
004600060921     C                   EXFMT     SC02D02
004700060921     c                   if        *inkc or *inkl
004800060921     C                   leave
004900060921     C                   end
005000120214     C   Ki              EXSR      multiass
005100120214     c* forzatura se multiassegno
005200120214     c                   if        *in44 and *inkf
005300120214     C                   SETON                                        45
005400120214     C                   iter
005500120214     c                   end
005600120214     C*
005700120214     c                   if        not *inkj and not *inkf
005800120214     C                   iter
005900120214     c                   end
006000120214     C* conferma
006100120220     c                   if        %subst(csbnra: 10: 1)<> ' ' and
006200120220     c                             csbtpi = 'M' and csbabi = 0
006300120214     C                   EXSR      SRAGGIO
006400120214     c                   else
006500120214     C     KCON          CHAIN     TNCSB000
006600120214     c                   if        %found(tncsb03l)
006700060921     c                   clear                   csbddp
006800060921     c                   clear                   csbbna
006900060921     c                   clear                   csbndp
007000060921     C                   EXCept    agg
007100120214     c                   end
007200120214     c                   end
007300060921     C                   Z-ADD     0             CONLNP
007400060921     C                   Z-ADD     0             CONNSP
007500060921     C                   Z-ADD     0             CONNRS
007600120214     c                   setoff                                       44
007700060921     c                   leave
007800060921     c                   enddo
007900060921     c                   if        *inkc
008000060921     C                   leave
008100060921     C                   end
008200060921     c                   enddo
008300060921     C                   CLEAR                   TIBS69
008400060921     C                   MOVEL     'C'           I69TLA
008500060921     C                   CALL      'TIBS69R'
008600060921     C                   PARM                    TIBS69
008700060921     C                   PARM                    CNACO
008800060921     C                   PARM                    CNIND
008900060921     C                   PARM                    CNCLP
009000060921     C                   PARM                    FNCLS
009100060921     C                   SETON                                        LR
009200060921     C*---------------------------------------------------------------*
009300060921     c     srctr         begsr
009400060921     C*---------------------------------------------------------------*
009500941028     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
009600060921     c                   setoff                                       88
009700060921     c                   do
009800941028     C                   MOVEL     CONAAS        WAA2              2 0
009900941028     C     WAA2          IFEQ      0
010000941028     C     CONAAS        IFGT      60
010100941028     C                   ADD       1900          CONAAS
010200941028     C                   ELSE
010300941028     C                   ADD       2000          CONAAS
010400941028     C                   END
010500941028     C                   END
010600891113     C* CONTROLLO ANNO SPEDIZIONE CHE SIA CORRETTO
010700941028     C                   MOVE      WHHDAT        UAAS              4 0
010800891113     C     CONAAS        COMP      UAAS                               98  83
010900891113     C  N98
011000891113     CANN83              SUB       1             UAAS
011100060921     c                   if        *in98
011200060921     c                   seton                                        88
011300060921     C                   leave
011400060921     c                   end
011500000000     C**
011600941028     C                   CLEAR                   WLBDA8
011700941028     C                   MOVE      WHHDAT        G02DAT
011800941028     C                   CALL      'XSRDA8'
011900941028     C                   PARM                    WLBDA8
012000941028     C                   Z-ADD     G02INV        RUDATE            8 0
012100000000     C** DATA SISTEMA AMG
012200120214     C     KCON          CHAIN(n)  TNCSB000                           99
012300060921     c                   if        *in99
012400060921     c                   seton                                        88
012500060921     C                   leave
012600060921     c                   end
012700120214     c* multiassegno
012800120220     c                   if        %subst(csbnra: 10: 1)<> ' ' and
012900120220     c                             csbtpi = 'M' and csbabi = 0
013000120214     c                   seton                                        44
013100120214     c                   eval      comkey = csbnra
013200120214     c                   else
013300120214     C                   CLEAR                   COMKEY
013400120214     c                   setoff                                       44
013500120214     c                   end
013600060921     c* controllo che sia un mittente, che ci siano i dati di pagamento e
013700060921     c* non la registrazione
013800120214     c* IMPORTANTE:
013900120214     c* è scontato che, in caso di multiassegno, anche le altre spediz.
014000120214     c* siano nella stessa situazione pagate ma non registrate
014100120214     c                   if        csbtpi <> 'M' or csbddp = 0 or csbnrg<>0
014200060921     c                   seton                                        8897
014300060921     C                   leave
014400060921     c                   end
014500910912     C                   MOVE      CSBKSC        KSC4              4 0
014600060921     c                   select
014700060921     C                   when      ksc4 = 8888
014800941028     C     KCSA          CHAIN     TNCSA000                           10
014900910912     C  N10              MOVEL     CSARSD        RSD
015000000000     C   10              MOVEL     *BLANKS       RSD
015100060921     C                   when      ksc4 = 9999
015200941028     C     KCSA          CHAIN     TNCSA000                           10
015300910912     C  N10              MOVEL     CSARSD        RSD
015400000000     C   10              MOVEL     *BLANKS       RSD
015500060921     c                   other
015600000000     C** ACCESSO CNACO
015700971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE
015800971218     C                   CLEAR                   TIBS69
015900971218     C                   CLEAR                   CNACO
016000971218     C                   CLEAR                   CNIND
016100971218     C                   CLEAR                   CNCLP
016200971218     C                   CLEAR                   FNCLS
016300971218     C                   MOVEL     ' '           I69TLA
016400971218     C                   MOVEL     KNSIF         I69SIF
016500971218     C                   Z-ADD     CSBCDI        I69KAC
016600971218     C                   CALL      'TIBS69R'
016700971218     C                   PARM                    TIBS69
016800971218     C                   PARM                    CNACO
016900971218     C                   PARM                    CNIND
017000971218     C                   PARM                    CNCLP
017100971218     C                   PARM                    FNCLS
017200971218     C     O69ERR        IFEQ      *BLANK
017300971218     C                   MOVEL     ACORAG        RSD
017400971218     C                   ELSE
017500971218     C                   MOVEL     *BLANKS       RSD
017600971218     C                   ENDIF
017700060921     C                   endsl
017800060921     c                   enddo
017900060921     C                   endsr
018000120214     c**********************************************************************
018100120214     c     sraggio       begsr
018200120214     c**********************************************************************
018300120214     C     kcsb          SETLL     tncsb09l
018400120214?    C                   do        *hival
018500120214     C     kcsb          READE     tncsb09l
018600120214     C                   if        %eof(tncsb09l)
018700120214     c                   leave
018800120214     c                   end
018900120214     c* solo il contrassegno con legame
019000120214     c                   if        csbnra <> comkey
019100120214     c                   iter
019200120214     c                   end
019300120214     c* per sicurezza controllo
019400120214     c                   if        csbtpi <> 'M' or csbddp = 0 or csbnrg<>0
019500120214     c                   iter
019600120214     c                   end
019700120214     c                   clear                   csbddp
019800120214     c                   clear                   csbbna
019900120214     c                   clear                   csbndp
020000120214     C                   update    tncsb009
020100120214     C                   enddo
020200120214     c                   endsr
020300120214     C**********************************************************************
020400120214     C     multiass      BEGSR
020500120214     C**********************************************************************
020600120214     c                   clear                   tnsc30ds
020700120214     c                   eval      sc30CMTI = 'N'
020800120214     c                   eval      sc30AMBI = 'S'
020900120214     c                   eval      sc30TPCI = 'V'
021000120214     c                   eval      sc30AASI = conAAS
021100120214     c                   eval      sc30LNPI = conLNP
021200120214     c                   eval      sc30NRSI = conNRS
021300120214     c                   eval      sc30NSPI = conNSP
021400120214     c                   movel(p)  tnsc30ds      kpjbu
021500120214     c                   call      'TNSC30R'
021600120214     c                   parm                    kpjba
021700120214     C                   ENDSR
021800941028     C*--------------------------------------------------------------*
021900941028     C*  OPERAZIONI INIZIALI
022000941028     C*--------------------------------------------------------------*
022100941028     C     *INZSR        BEGSR
022200941028     C*
022300941028     C     *ENTRY        PLIST
022400941028     C                   PARM                    KPJBA
022500941028     C                   Z-ADD     1             CODUT
022600941028     C                   CALL      'XPARUT'
022700941028     C                   PARM                    UTEDSE
022800941028     C                   MOVEL     RAGUT         RSUT             20
022900941028     C                   MOVEL     RAGUT         RSUT2            25
023000941028     C*---------------------------------------------------------------*
023100941028     C*  TNCSB
023200941028     C     KCON          KLIST
023300941028     C                   KFLD                    CONAAS
023400941028     C                   KFLD                    CONLNP
023500941028     C                   KFLD                    CONNRS
023600941028     C                   KFLD                    CONNSP
023700941028     C*  TNCSA
023800941028     C     KCSA          KLIST
023900941028     C                   KFLD                    CONAAS
024000941028     C                   KFLD                    CONLNP
024100941028     C                   KFLD                    CONNRS
024200941028     C                   KFLD                    CONNSP
024300120214     c     kcsb          klist
024400120214     c                   kfld                    csbrgf
024500120214     c                   kfld                    csbndt
024600120214     c                   kfld                    csbddc
024700941028     C                   SETOFF                                       99
024800941028     C                   TIME                    WHHDAT           14 0
024900941028     C                   MOVE      WHHDAT        CONAAS
025000941028     C*
025100890717     C                   ENDSR
025200890717     C*---------------------------------------------------------------*
025300941028     OTNCSB000  E            AGG
025400060921     O                       CSBbna
025500060921     O                       CSBndp
025600060921     O                       CSBddp
