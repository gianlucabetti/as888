000100950208     H DECEDIT('0,') DATEDIT(*DMY.)
000200941128      * FNLR92R *----------------------------------------------------*
000300911119      * ARRIVI:STAMPA ELENCO VARIAZIONI C/ASSEGNO                    *
000400911119      *--------------------------------------------------------------*
000500020927     FFNARBK2L  IF   E           K DISK
000600020927     F                                     INFDS(InfArbK2L)
000700020927     F                                     RENAME(FnArbK00:FnArbK20)
000800020927     F                                     USROPN
000900020927     FFNARBK0F  UF   E             DISK
001000020927     F                                     USROPN
001100020927     FFIARBT2L  IF   E           K DISK
001200020927     F                                     INFDS(InfArbT2L)
001300020927     F                                     RENAME(FiArbT00:FiArbT20)
001400020927     F                                     USROPN
001500020927     FFIARBT0F  UF   E             DISK
001600020927     F                                     USROPN
001700941128     FFNARB01L  IF   E           K DISK
001800051115     FFIAR901L  IF   E           K DISK
001900991012     FFIAR601L  IF   E           K DISK
002000911106     FTABEL00F  IF   E           K DISK
002100050309     FTIGCP21L  IF   E           K DISK
002200030204     Fazorg01l  IF   E           K DISK
002300060914     Ffnsvbw0f  O    e             disk
002400941129     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002500941129     F                                     USROPN
002600020927
002700020927     D InfArbK2L       DS
002800020927     D  ArbK2LRRN            397    400I 0
002900020927
003000020927     D InfArbT2L       DS
003100020927     D  ArbT2LRRN            397    400I 0
003200020927
003300060404     D TRul06ds      E DS
003400060404     D  LIN                    1     90  0 dim(30)
003500020927
003600900606     D FLU             S              1    DIM(32)                              FLAG PAR.UTENT
003700920304     D TIPV            S              2    DIM(99)                              COD.VARIAZIONE
003800920304     D FLGV            S              1    DIM(99)                              FLAG STAMPA
003900920304     D DESV            S             20    DIM(99)                              DESCR. VARIAZ.
004000060404     D prpV            S              1    DIM(99)                              FLAG STAMPA
004100941128     D TIPB            S              2    DIM(99)                              CODICI BOLLA
004200920316     D TB1             S              2    DIM(99)                              TIPO BOLLA 1
004300920316     D TB2             S              2    DIM(99)                              TIPO BOLLA 2
004400931118     D CCA             S              1    DIM(20)                              COD.CONS.ANIOMAL
004500931118     D DCCA            S             16    DIM(20)                              DESCR.CONS.ANOM.
004600911106     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)            PRINTER OVRPRTF
004700170320     D righest         S             66    DIM(3) CTDATA PERRCD(1)
004800900515      *
004900941128     D WLBDA8          DS
005000941128     D  G08DAT                 1      8  0
005100941128     D  G08INV                 9     16  0
005200941128     D  G08ERR                17     17
005300941128     D  G08TGI                18     22  0
005400050203     D ddatiute      e ds
005500050203     D azuteds       e ds                  extname(AZUTE00F)
005600050203     D tibs34ds      E DS                  inz
005700060405     D fnlr96ds2     E DS                  inz
005800030204     d og143         e ds
005900920316     D DS3A          E DS
006000911119     D DS7L          E DS
006100900514     D KPJBA         E DS
006200900514     D  LIBSYS                92    100
006300900514     D  MSGERR               453    502
006400941129     D                SDS
006500941129     D  §PGM                   1     10
006600060529     D param           DS
006700060529     D  parmpo                 1      3s 0
006800170320     d
006900170320     d conta           s              3  0 inz
007000020927
007100020927     ***********************************************************************
007200020927     **
007300020927     ** STAMPA VARIAZIONE BOLLE
007400020927     **
007500020927     ***********************************************************************
007600020927     C                   OPEN      FNARBK2L
007700020927     C                   OPEN      FNARBK0F
007800060914     c                   seton                                        44
007900170320     c                   clear                   conta
008000020927
008100941128     C     *LOVAL        SETLL     FNARBK2L
008200911119     C     *IN30         DOWEQ     '0'
008300020927     C                   READ      FNARBK2L                               30
008400020927
008500020927     C                   IF        NOT %EOF
008600060404      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
008700060404     C                   Z-ADD     1             I
008800060404     C     ARBCVB        LOOKUP    TIPV(I)                                22
008900060404     c* se variazione pre-pagato salto stampo nel pgm fnlr96r
009000060404     c                   if        prpv(i) ='S'
009100060404     c                   iter
009200060404     c                   end
009300020927
009400020927     C                   EXSR      RtvFnArb01L
009500020927
009600020927     C                   IF        NOT *IN01
009700020927     C                   ITER
009800020927     C                   ENDIF
009900030204
010000020927     ** Aggiorno il flag di stampato.
010100020927     C     ArbK2LRRN     CHAIN     FnArbK0F
010200020927     C                   IF        %FOUND
010300020927     C                   MOVEL     'S'           ARBFST
010400020927     C                   UPDATE    FnArbK00
010500020927     C                   ENDIF
010600030204     c* se la variazione è generata dal passaggio delle consegne a SDA
010700030204     c* non stampa
010800030204     c                   if        arbdcm <> 0 and arbcca <> *blanks
010900030204     c                             and arbcca <> '5'
011000030204     c     arblnp        chain     azorg01l
011100030204     c                   if        %found(azorg01l)
011200030204     c                   movel     orgde3        og143
011300030204     c                   if        §ogntw = 'PPT'
011400030204     c                   iter
011500030204     c                   end
011600030204     c                   end
011700030204     c                   end
011800020927
011900920304     C   22FLGV(I)       IFEQ      'S'
012000911119      *
012100920304     C                   MOVEL     DESV(I)       DCVB             20
012200920302     C                   SETOFF                                       505152
012300920316     C                   SETOFF                                       53
012400950118     C                   CLEAR                   AR9CAS
012500950118     C                   CLEAR                   AR9VCA
012600920302      * CONTRASSEGNO ANNULLATO
012700051115     C     KARB          CHAIN     FIAR901L                           20
012800941128     C     *IN20         IFEQ      '1'
012900941129     C     ARBCAS        ANDNE     0
013000941128     C     *IN20         OREQ      '0'
013100941128     C     AR9CAS        ANDEQ     0
013200941129     C     ARBCAS        ANDNE     0
013300920302     C                   SETON                                        50
013400920302     C                   END
013500920302     C* CONTRASSEGNO AGGIUNTO
013600941128     C  N20AR9CAS        IFNE      0
013700941128     C     ARBCAS        ANDEQ     0
013800920302     C                   SETON                                        51
013900920302     C                   END
014000941129     C* CONTRASSEGNO VARIATO
014100941128     C  N20AR9CAS        IFNE      0
014200941128     C     ARBCAS        ANDNE     0
014300941128     C     ARBCAS        ANDNE     AR9CAS
014400920302     C                   SETON                                        52
014500920302     C                   END
014600941129     C*  VARIATO DIVISA
014700950118     C  N20
014800950118     CANN50
014900950118     CANN51
015000950118     CANN52AR9VCA        IFNE      ARBVCA
015100941129     C                   SETON                                        52
015200941129     C                   ENDIF
015300920316     C  N50
015400920316     CANN51
015500920316     CANN52              DO
015600920316     C                   Z-ADD     1             X                 2 0
015700941128     C     ARBCBO        LOOKUP    TIPB(X)                                41
015800920316     C   41TB1(X)        IFEQ      'AS'
015900920316     C     TB1(X)        OREQ      'FS'
016000920316     C     TB2(X)        OREQ      'AP'
016100920316     C                   SETON                                        53
016200920316     C                   END
016300920316     C                   END
016400920302     C     *IN50         IFEQ      '1'
016500920302     C     *IN51         OREQ      '1'
016600920302     C     *IN52         OREQ      '1'
016700920316     C     *IN53         OREQ      '1'
016800911121      *- GIRO LA DATA PER LA STAMPA ----------------------------------*
016900941128     C                   Z-ADD     ARBDTV        G08INV
017000941128     C                   MOVE      *ZEROS        G08DAT
017100941128     C                   MOVEL     '3'           G08ERR
017200941128     C                   CALL      'XSRDA8'
017300941128     C                   PARM                    WLBDA8
017400941128     C                   Z-ADD     G08DAT        DATA1             8 0
017500911119      *
017600930923      * ESEGUO REPERRIMENTO DATI GIACENZA
017700930923     C                   MOVE      *ZERO         GCPAGC
017800930923     C                   MOVE      *ZERO         GCPFGC
017900930923     C                   MOVE      *ZERO         GCPNGC
018000930923     C                   MOVE      ARBAAS        GCPAAS
018100930923     C                   MOVE      ARBLNP        GCPLNP
018200930923     C                   MOVE      ARBNRS        GCPNRS
018300930923     C                   MOVE      ARBNSP        GCPNSP
018400050309     C     KGCA          CHAIN     TIGCP21L                           21
018500930923     C*
018600931119     C*  DECODIFICO TIPO CONSEGNA ANOMALA
018700931118     C                   Z-ADD     1             W                 2 0
018800931118     C                   MOVEL     *BLANKS       DECC
018900931118     C     ARBCCA        LOOKUP    CCA(W)                                 40
019000931118     C   40              MOVE      DCCA(W)       DECC             16
019100060405     C* cerco motivo variazione
019200060405     c                   exsr      srdecmtv
019300060405     c     d962TXT       comp      ' '                                8888
019400170320     c
019500170320     c                   add       arbcas        casold           15 3
019600170320     c                   add       ar9cas        casnew           15 3
019700931118     C*
019800921029     C   OF              EXCEPT    TESTA
019900170320     c                   setoff                                       of
020000170320     c
020100170320     c                   add       1             conta
020200911121     C                   EXCEPT    DETTA
020300060405     C   88              EXCEPT    DETTA2
020400060914     c                   exsr      srwtr
020500060405     C   88              EXCEPT    salto
020600991118     C                   MOVEL     'S'           WSTP              1
020700911121      *
020800911121     C                   END
020900920302     C                   END
021000911119      *
021100911119     C                   END
021200911119      *
021300920302     C                   END
021400020927
021500020927     C                   CLOSE     FNARBK2L
021600020927     C                   CLOSE     FNARBK0F
021700020927
021800080117     C*    WSTP          IFEQ      'S'
021900080117     C   OF              EXCEPT    TESTA
022000080117     C                   EXCEPT    FINE
022100080117     C*                  END
022200020927
022300020927      /EJECT
022400020927     C**********************************************************************
022500020927     C*
022600920304     C* STAMPA VARIAZIONE PORTI ASSEGNATI
022700920304     C*
022800020927     C**********************************************************************
022900020927     C                   OPEN      FIARBT2L
023000020927     C                   OPEN      FIARBT0F
023100060914     c                   setoff                                       44
023200170320     c                   clear                   conta
023300020927
023400991118     C                   SETON                                        OF
023500920304     C                   Z-ADD     0             PAGE
023600020927
023700991012     C     *LOVAL        SETLL     FIARBT2L
023800920304     C     *IN31         DOWEQ     '0'
023900020927     C                   READ      FIARBT2L                               31
024000020927
024100060404     c                   IF        NOT %EOF
024200060404      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
024300060404     C                   Z-ADD     1             I
024400060404     C     ARBCVB        LOOKUP    TIPV(I)                                22
024500060404     c* se variazione pre-pagato salto stampo nel pgm fnlr96r
024600060404     c                   if        prpv(i) ='S'
024700060404     c                   iter
024800060404     c                   end
024900020927
025000020927     C                   EXSR      RtvFnArb01L
025100020927
025200020927     C                   IF        NOT *IN01
025300020927     C                   ITER
025400020927     C                   ENDIF
025500020927
025600020927     ** Aggiorno il flag di stampato.
025700020927     C     ArbT2LRRN     CHAIN     FiArbT0F
025800020927     C                   IF        %FOUND
025900020927     C                   MOVEL     'S'           ARBFST
026000020927     C                   UPDATE    FiArbT00
026100020927     C                   ENDIF
026200020927
026300920304     C   22FLGV(I)       IFEQ      'S'
026400920304      *
026500950118     C                   MOVEL     DESV(I)       DCVB             20
026600920304     C                   SETOFF                                       52
026700950118     C                   CLEAR                   AR6IFT
026800950118     C     KARB          CHAIN     FNARB01L                           20
026900991012     C     KAR6          CHAIN     FIAR601L                           20
027000920316     C* IMPORTO FATTURA VARIATO
027100950118     C  N20ARBIFT        IFNE      AR6IFT
027200920304     C                   SETON                                        52
027300920304     C                   END
027400920304     C     *IN52         IFEQ      '1'
027500920304      *- GIRO LA DATA PER LA STAMPA ----------------------------------*
027600941128     C                   Z-ADD     ARBDTV        G08INV
027700941128     C                   MOVE      *ZEROS        G08DAT
027800941128     C                   MOVEL     '3'           G08ERR
027900941128     C                   CALL      'XSRDA8'
028000941128     C                   PARM                    WLBDA8
028100941128     C                   Z-ADD     G08DAT        DATA1
028200930923      *
028300950118     C                   Z-ADD     ARBDFT        G08INV
028400950118     C                   MOVE      *ZEROS        G08DAT
028500950118     C                   MOVEL     '3'           G08ERR
028600950118     C                   CALL      'XSRDA8'
028700950118     C                   PARM                    WLBDA8
028800950118     C                   Z-ADD     G08DAT        GMADFT            8 0
028900950118      *
029000930923      * ESEGUO REPERRIMENTO DATI GIACENZA
029100930923     C                   MOVE      *ZERO         GCPAGC
029200930923     C                   MOVE      *ZERO         GCPFGC
029300930923     C                   MOVE      *ZERO         GCPNGC
029400930923     C                   MOVE      ARBAAS        GCPAAS
029500930923     C                   MOVE      ARBLNP        GCPLNP
029600930923     C                   MOVE      ARBNRS        GCPNRS
029700930923     C                   MOVE      ARBNSP        GCPNSP
029800050309     C     KGCA          CHAIN     TIGCP21L                           21
029900931118     C*
030000931119     C*  DECODIFICO TIPO CONSEGNA ANOMALA
030100931118     C                   Z-ADD     1             W                 2 0
030200931118     C                   MOVEL     *BLANKS       DECC
030300931118     C     ARBCCA        LOOKUP    CCA(W)                                 40
030400931118     C   40              MOVE      DCCA(W)       DECC             16
030500060405     C* cerco motivo variazione
030600060405     c                   exsr      srdecmtv
030700060405     c     d962TXT       comp      ' '                                8888
030800170320     c
030900170320     c                   add       arbift        iftold           15 3
031000170320     c                   add       ar6ift        iftnew           15 3
031100920304      *
031200921029     C   OF              EXCEPT    TESTA1
031300170320     c                   setoff                                       of
031400170320     c                   add       1             conta
031500920304     C                   EXCEPT    DETTA1
031600060405     C   88              EXCEPT    DETTA2
031700060914     c                   exsr      srwtr
031800060406     C   88              EXCEPT    salto
031900991118     C                   MOVEL     'S'           WSTP1             1
032000920304      *
032100920304     C                   END
032200920304     C                   END
032300920304      *
032400920304     C                   END
032500920304      *
032600920304     C                   END
032700020927
032800020927     C                   CLOSE     FIARBT2L
032900020927     C                   CLOSE     FIARBT0F
033000020927
033100080117     C*    WSTP1         IFEQ      'S'
033200080117     C   OF              EXCEPT    TESTA1
033300921029     C                   EXCEPT    FINE1
033400080117     C*                  END
033500921029     C*
033600910521     C                   SETON                                        LR
033700060405      *---------------------------------------------------------------*
033800060914     C     srwtr         BEGSR
033900060405      *---------------------------------------------------------------*
034000060914     c                   eval      wvbaas = ARBAAS
034100060914     c                   eval      wvbLNP = ARBLNP
034200060914     c                   eval      wvbLNA = ARBLNA
034300060914     c                   eval      wvbNRS = ARBNRS
034400060914     c                   eval      wvbNSP = ARBNSP
034500060914     c                   eval      wvbDTV = ARBDTV
034600060928     c                   eval      wvborV = ARBorV
034700060928     c                   eval      wvbpru = ARBpru
034800060928     c                   eval      wvbcbop= ARBCBo
034900060928     c                   eval      wvbcbod= ARBCBN
035000060928     c* giacenza
035100060928     c                   if        GCPNGC <> 0
035200060928     c                   eval      wvbgia = 'S'
035300060928     c                   else
035400060928     c                   eval      wvbgia = 'N'
035500060928     c                   end
035600060914     C                   MOVEL     dcvb          wvbcvbd
035700060914     c                   eval      WVBMTVD= d962des
035800060914     c                   clear                   wvbnot
035900060914     c   88              eval      WVBNOT  = %trimr(d962txt)
036000060914     c* variazione c/ass
036100060914     c                   if        *in44
036200060914     c                   eval      wvbimpp= ARBcas
036300060914     c                   eval      wvbdivp= ARBvca
036400060914     c                   eval      wvbimpd= AR9cas
036500060914     c                   eval      wvbdivd= AR9vca
036600060914     c                   eval      wvbtic= ARbtic
036700060914     c   50              eval      wvbdes = 'C/A ANNULLATO'
036800060914     c   51              eval      wvbdes = 'C/A AGGIUNTO '
036900060914     c   52              eval      wvbdes = 'C/A VARIATO  '
037000060914     c   53              eval      wvbdes = 'VAR.COD.BOL. '
037100060914     c                   else
037200060914     c* variazione assegnati
037300060928     c                   clear                   wvbcbod
037400060914     c                   eval      wvbimpp= ARBift
037500060914     c                   eval      wvbdivp= ARBdiv
037600060914     c                   eval      wvbimpd= AR6ift
037700060914     c                   eval      wvbdivd= AR6div
037800060914     c                   clear                   wvbtic
037900060914     c                   clear                   wvbdes
038000060914     c                   end
038100060914     c                   write     fnsvbw00
038200060914     c                   endsr
038300060914      *---------------------------------------------------------------*
038400060914     C     srdecmtv      BEGSR
038500060914      *---------------------------------------------------------------*
038600060405     c                   clear                   fnlr96ds2
038700060405     c                   eval      d962aas = ARBAAS
038800060405     c                   eval      d962LNP = ARBLNP
038900060405     c                   eval      d962NRS = ARBNRS
039000060405     c                   eval      d962NSP = ARBNSP
039100060405     c                   eval      d962ORV = ARBORV
039200060405     c                   eval      d962DTV = ARBDTV
039300060405     c                   eval      d962cvb = ARBcvb
039400060405     c                   movel(p)  fnlr96ds2     kpjbu
039500060405     c                   call      'FNLR96R2'
039600060405     c                   parm                    kpjba
039700060405     c                   movel(p)  kpjbu         fnlr96ds2
039800060405     c                   endsr
039900911119      *
040000911121      *---------------------------------------------------------------*
040100941129     C     *INZSR        BEGSR
040200000000     C     *ENTRY        PLIST
040300000000     C                   PARM                    KPJBA
040400060529     c                   movel     kpjbu         param
040500900516      *
040600941129     C                   TIME                    WHHUDT           14 0
040700941129     C                   MOVEL     WHHUDT        ORA               6 0
040800941129     C                   MOVE      WHHUDT        G08DAT
040900941129     C                   MOVE      *ZEROS        G08INV
041000941129     C                   MOVEL     *BLANK        G08ERR
041100941129     C                   CALL      'XSRDA8'
041200941129     C                   PARM                    WLBDA8
041300941129     C                   Z-ADD     G08DAT        GMAUD8            8 0
041400060405     C                   MOVEL     *ALL'-'       TRATT           198
041500050203     C                   Z-ADD     1             CODUT             1 0
041600050203     c* dati utente
041700050203     c     *dtaara       define    §azute        azuteds
041800050203     c     *dtaara       define    §datiute      ddatiute
041900050203     C                   in(E)     *dtaara
042000050203     C                   IF        %Error  or  RSUT = *blanks
042100050203     C                   call      'TIBS34R'
042200050203     C                   parm                    Tibs34Ds
042300050203     C                   in        *dtaara
042400050203     c                   ENDIF
042500911119      *
042600920304      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
042700920304     C                   MOVEL     '7L'          CODTAB
042800920304     C                   MOVE      *BLANKS       KEYTAB
042900920304     C                   Z-ADD     0             I
043000920304     C     KTAB          SETLL     TABEL00F                           22
043100920304     C     *IN22         DOWEQ     '0'
043200920304     C     KTAB1         READE     TABEL00F                               22
043300920304     C  N22              DO
043400920304     C                   ADD       1             I                 3 0
043500920304     C                   MOVEL     TBLUNI        DS7L
043600920304     C                   MOVEL     TBLKEY        TIPV(I)
043700920304     C                   MOVEL     §7LSTF        FLGV(I)
043800920304     C                   MOVEL     §7LDES        DESV(I)
043900060404     C                   MOVEL     §7Lprp        prpv(I)
044000920304     C                   END
044100920304     C                   END
044200920316      *- CARICO TABELLA TIPI BOLLA
044300920316     C                   MOVEL     '3A'          CODTAB
044400920316     C                   MOVE      *BLANKS       KEYTAB
044500920316     C                   Z-ADD     0             I
044600920316     C     KTAB          SETLL     TABEL00F                           22
044700920316     C     *IN22         DOWEQ     '0'
044800920316     C     KTAB1         READE     TABEL00F                               22
044900920316     C  N22              DO
045000920316     C                   ADD       1             I                 3 0
045100920316     C                   MOVEL     TBLUNI        DS3A
045200920316     C                   MOVEL     TBLKEY        TIPB(I)
045300920316     C                   MOVEL     §3ATB1        TB1(I)
045400920316     C                   MOVE      §3ATB2        TB2(I)
045500920316     C                   END
045600920316     C                   END
045700931118      *
045800931119      *- CARICO DESCRIZIONI CONS.ANOMALA         ---------------------*
045900931118     C                   MOVEL     '7O'          CODTAB
046000931118     C                   MOVE      *BLANKS       KEYTAB
046100931118     C                   Z-ADD     0             I
046200931118     C     KTAB          SETLL     TABEL00F                           22
046300931118     C     *IN22         DOWEQ     '0'
046400931118     C     KTAB1         READE     TABEL00F                               22
046500931118     C     *IN22         IFEQ      '0'
046600931118     C     I             ANDLT     20
046700931118     C                   ADD       1             I                 3 0
046800931118     C                   MOVEL     TBLKEY        CCA(I)
046900931118     C                   MOVEL     TBLUNI        DCCA(I)
047000931118     C                   END
047100931118     C                   END
047200020927
047300060404      * ricarico la £6 se veniva cambiato il P.O aveva i dati errati
047400060404     c                   clear                   trul06ds
047500060404     C                   MOVEL     '£6'          D06COD
047600060529     C                   MOVEL     parmpo        D06KEY
047700060404     C                   MOVEL     TRul06ds      KPJBU
047800060404     C                   CALL      'TRUL06R'
047900060404     C                   PARM                    KPJBA
048000060404     C                   MOVEL     KPJBU         TRul06ds
048100020927
048200911119      *- CHIAVE X TABEL00F -------------------------------------------*
048300911106     C     KTAB          KLIST
048400911106     C                   KFLD                    CODUT
048500911106     C                   KFLD                    CODTAB            2
048600911106     C                   KFLD                    KEYTAB            8
048700920304     C     KTAB1         KLIST
048800920304     C                   KFLD                    CODUT
048900920304     C                   KFLD                    CODTAB
049000941129      *
049100941129     C     KARB          KLIST
049200941129     C                   KFLD                    ARBAAS
049300941129     C                   KFLD                    ARBLNP
049400941129     C                   KFLD                    ARBNRS
049500941129     C                   KFLD                    ARBNSP
049600991012     C     KAR6          KLIST
049700991012     C                   KFLD                    ARBAAS
049800991012     C                   KFLD                    ARBLNP
049900991012     C                   KFLD                    ARBNRS
050000991012     C                   KFLD                    ARBNSP
050100991012     C                   KFLD                    ARBTRC
050200941129     C*
050300941129     C*  CHIAVE PER POSIZIONAMENTO SU ARCHIVIO GIACENZE
050400941129     C     KGCA          KLIST
050500941129     C                   KFLD                    GCPAAS
050600941129     C                   KFLD                    GCPLNP
050700941129     C                   KFLD                    GCPNRS
050800941129     C                   KFLD                    GCPNSP
050900941129      *
051000941129      *- APRO FILE DI STAMPA -----------------------------------------*
051100941129     C                   Z-ADD     60            LUNCMD           15 5
051200941129     C                   CALL      'QCMDEXC'
051300941129     C                   PARM                    CMDP
051400941129     C                   PARM                    LUNCMD
051500941129     C                   OPEN      PRTF198
051600941129     C*
051700991118     C                   SETON                                        OF
051800941129     C*
051900911121      *---------------------------------------------------------------*
052000910521     C                   ENDSR
052100900606      *----------------------------------------------------------------
052200020927
052300020927     ***********************************************************************
052400020927     **
052500020927     ** Reperisco il record di testata della bolla.
052600020927     **
052700020927     ***********************************************************************
052800020927     C     RtvFnArb01L   BEGSR
052900020927
053000020927     C     KARB          CHAIN     FNARB01L                           20
053100020927
053200020927     ** Seleziono le linee di arrivo del terminal dell'utente.
053300020927     C                   IF        %FOUND
053400040316     C     ArbLnA        LOOKUP    Lin                                    01
053500020927     C                   ELSE
053600020927     C                   EVAL      *IN01 = *OFF
053700020927     C                   ENDIF
053800020927
053900020927     C                   ENDSR
054000020927
054100941129     OPRTF198   E            TESTA          1 01
054200911106     O                       RSUT                20
054300060529     O                       parmpo              28
054400911121     O                                           62 'ELENCO VARIAZIONI'
054500911119     O                                         +  1 'C/ASSEGNI'
054600941129     O                       §PGM               110
054700941129     O                       GMAUD8             130 '  /  /    '
054800941129     O                                          150 'PAG.'
054900941129     O                       PAGE               156
055000911106      *
055100941129     O          E            TESTA          1
055200941129     O                       KNSIF             +  0
055300941129     O                       KNMUS             +  1
055400941129     O                       ORA                130 '  :  :  '
055500941129     O          E            TESTA          1
055600911120     O                       TRATT             +  0
055700911106      *
055800941129     O          E            TESTA          1
055900941128     O                                            8 '  DATA'
056000941128     O                                           24 'C A U S A L E'
056100941128     O                                           45 'SPEDIZIONE'
056200950118     O                                          122 ' DATI GIACENZA '
056300941129     O          E            TESTA          1
056400170320     O                                           14 'VARIAZ.'
056500170320     O                                           30 'CD DESCRIZIONE'
056600170320     O                                           43 'LNP'
056700170320     O                                           47 'LNA'
056800170320     O                                           51 'AA'
056900170320     O                                           55 'NR'
057000170320     O                                           63 'SPEDIZ.'
057100170320     O                                           80 'C/A VECCHIO'
057200170320     O                                          102 'C/A NUOVO'
057300170320     O                                          110 'TP'
057400170320     O                                          114 'AA'
057500170320     O                                          119 'FGC '
057600170320     O                                          127 'NUMERO'
057700950118     O                                         +  1 'CONSEGNA ANOMALA'
057800170320     O                                          154 'MOTIVO'
057900911106      *
058000941129     O          E            TESTA          1
058100911120     O                       TRATT             +  0
058200911120      *
058300911121     O          E            DETTA       1
058400170320     o                       conta         z      3
058500170320     o                                            4 ')'
058600170320     O                       DATA1               15 '  /  /    '
058700170320     O                       ARBCVB              18
058800170320     O                       DCVB                39
058900170320     O                       ARBLNP              43
059000170320     O                       ARBLNA              47
059100170320     O                       ARBAAS        Z     52
059200170320     O                       ARBNRS              55
059300170320     O                       ARBNSP        Z     63
059400170320     O                       ARBCAS        2     81
059500170320     O                       ARBVCA              85
059600170320     O                       AR9CAS        2    103
059700170320     O                       AR9VCA             107
059800170320     O                       ARBTIC             110
059900170320     O                       GCPAGC        Z    115
060000170320     O                       GCPFGC        Z    119
060100170320     O                       GCPNGC        Z    127
060200170320     O                       ARBCCA             129
060300950118     O                       DECC              +  1
060400170320     O               50                         161 'C/A ANNULLATO'
060500170320     O               51                         161 'C/A AGGIUNTO '
060600170320     O               52                         161 'C/A VARIATO  '
060700170320     O               53                         161 'VAR.COD.BOL. '
060800060406     O                       D962des           +  1
060900921029     O          E            FINE        1
061000170320     o                       righest(1)          66
061100170320     o                       righest(1)         132
061200170320     o                       righest(1)         198
061300170320     O          E            FINE        1
061400170320     o                                        +   2 'Totale spedizioni:'
061500170320     o                       conta         2  +   2
061600170320     o                                           58 'Totale importi:'
061700170321     O                       casold        2     81
061800170320     O                       casnew        2    103
061900170320     O          E            FINE        1
062000170320     o                       righest(1)          66
062100170320     o                       righest(1)         132
062200170320     o                       righest(1)         198
062300170320     O                                           70 ' FINE STAMPA'
062400170320     O                                         +  0 ' VARIAZIONI'
062500170320     O                                         +  0 ' CONTRASSEGNI '
062600941129     O          E            TESTA1         1 01
062700920304     O                       RSUT                20
062800060529     O                       parmpo              28
062900941129     O                                           57 'ELENCO VARIAZIONI'
063000941129     O                                         +  1 'PORTI ASSEGNATI'
063100941129     O                       §PGM               110
063200941129     O                       GMAUD8             130 '  /  /    '
063300941129     O                                          150 'PAG.'
063400941129     O                       PAGE               156
063500941129      *
063600950118     O          E            TESTA1         1
063700941129     O                       KNSIF             +  0
063800941129     O                       KNMUS             +  1
063900941129     O                       ORA                130 '  :  :  '
064000920304      *
064100941129     O          E            TESTA1         1
064200920304     O                       TRATT             +  0
064300920304      *
064400941129     O          E            TESTA1         1
064500170320     O                                           13 '  DATA'
064600170320     O                                           29 'C A U S A L E'
064700170320     O                                           50 'SPEDIZIONE'
064800170320     O                                          122 ' DATI GIACENZA'
064900941129     O          E            TESTA1         1
065000170320     O                                           15 'VARIAZ.'
065100170320     O                                           30 'CD DESCRIZIONE'
065200170320     O                                           43 'LNP'
065300170320     O                                           47 'LNA'
065400170320     O                                           51 'AA'
065500170320     O                                           55 'NR'
065600170320     O                                           63 'SPEDIZ.'
065700170320     O                                           77 'IMPOR.VECCHIO'
065800170320     O                                           99 'IMPORTO NUOVO'
065900170320     O                                          117 'N.FATT.'
066000170320     O                                          132 'DATA'
066100170320     O                                          130 'AA'
066200170320     O                                          135 'FGC'
066300170320     O                                          143 'NUMERO'
066400950118     O                                         +  1 'CONSEGNA ANOMALA'
066500060406     O                                         +  3 'MOTIVO'
066600920304      *
066700941129     O          E            TESTA1         1
066800920304     O                       TRATT             +  0
066900920304      *
067000920304     O          E            DETTA1      1
067100170320     o                       conta         z      3
067200170320     o                                            4 ')'
067300170320     O                       DATA1               15 '  /  /    '
067400170320     O                       ARBCVB              18
067500170320     O                       DCVB                39
067600170320     O                       ARBLNP              43
067700170320     O                       ARBLNA              47
067800170320     O                       ARBAAS        Z     52
067900170320     O                       ARBNRS              55
068000170320     O                       ARBNSP        Z     63
068100170320     O                       ARBIFT        2     81
068200170320     O                       ARBDIV              85
068300170320     O                       AR6IFT        2    103
068400170320     O                       AR6DIV             107
068500170320     O                       ARBNFT        Z    115
068600170320     O                       GMADFT             126 '  /  /    '
068700170320     O                       GCPAGC        Z    131
068800170320     O                       GCPFGC        Z    135
068900170320     O                       GCPNGC        Z    143
069000170320     O                       ARBCCA             145
069100950118     O                       DECC              +  1
069200060406     O                       D962des           +  1
069300060405     O          E            DETTA2      1
069400060406     o                       d962TXT           +  0
069500060405     O          E            salto       1
069600170320     O          E            FINE1       1
069700170320     o                       righest(1)          66
069800170320     o                       righest(1)         132
069900170320     o                       righest(1)         198
070000170320     O          E            FINE1       1
070100170320     o                                        +   2 'Totale spedizioni:'
070200170320     o                       conta         2  +   2
070300170320     o                                           58 'Totale importi:'
070400170320     O                       iftold        2     81
070500170320     O                       iftnew        2    103
070600921029     O          E            FINE1       1
070700170320     o                       righest(1)          66
070800170320     o                       righest(1)         132
070900170320     o                       righest(1)         198
071000941129     O                                           70 'FINE STAMPA'
071100170320     O                                         +  0 ' VARIAZIONI'
071200170320     O                                         +  0 ' ASSEGNATI'
071300911119      *
071400950118**  CMDP
071500950118OVRPRTF FILE(PRTF198) USRDTA(ELEN_VARIA) SAVE(*YES)
071600170320**
071700170320------------------------------------------------------------------
