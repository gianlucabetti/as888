000100950208     H DECEDIT('0,') DATEDIT(*DMY.)
000200941128      * FNLR92R *----------------------------------------------------*
000300911119      *-----------------*                                            *
000400911119      * ARRIVI:STAMPA ELENCO VARIAZIONI C/ASSEGNO                    *
000500911119      *--------------------------------------------------------------*
000600020927     FFNARBK2L  IF   E           K DISK
000700020927     F                                     INFDS(InfArbK2L)
000800020927     F                                     RENAME(FnArbK00:FnArbK20)
000900020927     F                                     USROPN
001000020927     FFNARBK0F  UF   E             DISK
001100020927     F                                     USROPN
001200020927     FFIARBT2L  IF   E           K DISK
001300020927     F                                     INFDS(InfArbT2L)
001400020927     F                                     RENAME(FiArbT00:FiArbT20)
001500020927     F                                     USROPN
001600020927     FFIARBT0F  UF   E             DISK
001700020927     F                                     USROPN
001800941128     FFNARB01L  IF   E           K DISK
001900051115     FFIAR901L  IF   E           K DISK
002000991012     FFIAR601L  IF   E           K DISK
002100911106     FTABEL00F  IF   E           K DISK
002200050309     FTIGCP21L  IF   E           K DISK
002300030204     Fazorg01l  IF   E           K DISK
002301060914     Ffnsvbw0f  O    e             disk
002400941129     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002500941129     F                                     USROPN
002600020927
002700020927     D InfArbK2L       DS
002800020927     D  ArbK2LRRN            397    400I 0
002900020927
003000020927     D InfArbT2L       DS
003100020927     D  ArbT2LRRN            397    400I 0
003200020927
003201060404     D TRul06ds      E DS
003202060404     D  LIN                    1     90  0 dim(30)
003700020927
003800900606     D FLU             S              1    DIM(32)                              FLAG PAR.UTENT
003900920304     D TIPV            S              2    DIM(99)                              COD.VARIAZIONE
004000920304     D FLGV            S              1    DIM(99)                              FLAG STAMPA
004100920304     D DESV            S             20    DIM(99)                              DESCR. VARIAZ.
004101060404     D prpV            S              1    DIM(99)                              FLAG STAMPA
004200941128     D TIPB            S              2    DIM(99)                              CODICI BOLLA
004300920316     D TB1             S              2    DIM(99)                              TIPO BOLLA 1
004400920316     D TB2             S              2    DIM(99)                              TIPO BOLLA 2
004500931118     D CCA             S              1    DIM(20)                              COD.CONS.ANIOMAL
004600931118     D DCCA            S             16    DIM(20)                              DESCR.CONS.ANOM.
004700911106     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)            PRINTER OVRPRTF
004800900515      *
004900941128     D WLBDA8          DS
005000941128     D  G08DAT                 1      8  0
005100941128     D  G08INV                 9     16  0
005200941128     D  G08ERR                17     17
005300941128     D  G08TGI                18     22  0
005400050203     D ddatiute      e ds
005500050203     D azuteds       e ds                  extname(AZUTE00F)
005600050203     D tibs34ds      E DS                  inz
005601060405     D fnlr96ds2     E DS                  inz
005700030204     d og143         e ds
005800920316     D DS3A          E DS
005900911119     D DS7L          E DS
006000900514     D KPJBA         E DS
006100900514     D  LIBSYS                92    100
006200900514     D  MSGERR               453    502
006300941129     D                SDS
006400941129     D  §PGM                   1     10
006401060529     D param           DS
006402060529     D  parmpo                 1      3s 0
006500020927
006600020927     ***********************************************************************
006700020927     **
006800020927     ** STAMPA VARIAZIONE BOLLE
006900020927     **
007000020927     ***********************************************************************
007100020927     C                   OPEN      FNARBK2L
007200020927     C                   OPEN      FNARBK0F
007201060914     c                   seton                                        44
007300020927
007400941128     C     *LOVAL        SETLL     FNARBK2L
007500911119     C     *IN30         DOWEQ     '0'
007600020927     C                   READ      FNARBK2L                               30
007700020927
007800020927     C                   IF        NOT %EOF
007801060404      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
007802060404     C                   Z-ADD     1             I
007803060404     C     ARBCVB        LOOKUP    TIPV(I)                                22
007804060404     c* se variazione pre-pagato salto stampo nel pgm fnlr96r
007805060404     c                   if        prpv(i) ='S'
007806060404     c                   iter
007807060404     c                   end
007900020927
008000020927     C                   EXSR      RtvFnArb01L
008100020927
008200020927     C                   IF        NOT *IN01
008300020927     C                   ITER
008400020927     C                   ENDIF
008500030204
008600020927     ** Aggiorno il flag di stampato.
008700020927     C     ArbK2LRRN     CHAIN     FnArbK0F
008800020927     C                   IF        %FOUND
008900020927     C                   MOVEL     'S'           ARBFST
009000020927     C                   UPDATE    FnArbK00
009100020927     C                   ENDIF
009200030204     c* se la variazione è generata dal passaggio delle consegne a SDA
009300030204     c* non stampa
009400030204     c                   if        arbdcm <> 0 and arbcca <> *blanks
009500030204     c                             and arbcca <> '5'
009600030204     c     arblnp        chain     azorg01l
009700030204     c                   if        %found(azorg01l)
009800030204     c                   movel     orgde3        og143
009900030204     c                   if        §ogntw = 'PPT'
010000030204     c                   iter
010100030204     c                   end
010200030204     c                   end
010300030204     c                   end
010400020927
010800920304     C   22FLGV(I)       IFEQ      'S'
010900911119      *
011000920304     C                   MOVEL     DESV(I)       DCVB             20
011100920302     C                   SETOFF                                       505152
011200920316     C                   SETOFF                                       53
011300950118     C                   CLEAR                   AR9CAS
011400950118     C                   CLEAR                   AR9VCA
011500920302      * CONTRASSEGNO ANNULLATO
011600051115     C     KARB          CHAIN     FIAR901L                           20
011700941128     C     *IN20         IFEQ      '1'
011800941129     C     ARBCAS        ANDNE     0
011900941128     C     *IN20         OREQ      '0'
012000941128     C     AR9CAS        ANDEQ     0
012100941129     C     ARBCAS        ANDNE     0
012200920302     C                   SETON                                        50
012300920302     C                   END
012400920302     C* CONTRASSEGNO AGGIUNTO
012500941128     C  N20AR9CAS        IFNE      0
012600941128     C     ARBCAS        ANDEQ     0
012700920302     C                   SETON                                        51
012800920302     C                   END
012900941129     C* CONTRASSEGNO VARIATO
013000941128     C  N20AR9CAS        IFNE      0
013100941128     C     ARBCAS        ANDNE     0
013200941128     C     ARBCAS        ANDNE     AR9CAS
013300920302     C                   SETON                                        52
013400920302     C                   END
013500941129     C*  VARIATO DIVISA
013600950118     C  N20
013700950118     CANN50
013800950118     CANN51
013900950118     CANN52AR9VCA        IFNE      ARBVCA
014000941129     C                   SETON                                        52
014100941129     C                   ENDIF
014200920316     C  N50
014300920316     CANN51
014400920316     CANN52              DO
014500920316     C                   Z-ADD     1             X                 2 0
014600941128     C     ARBCBO        LOOKUP    TIPB(X)                                41
014700920316     C   41TB1(X)        IFEQ      'AS'
014800920316     C     TB1(X)        OREQ      'FS'
014900920316     C     TB2(X)        OREQ      'AP'
015000920316     C                   SETON                                        53
015100920316     C                   END
015200920316     C                   END
015300920302     C     *IN50         IFEQ      '1'
015400920302     C     *IN51         OREQ      '1'
015500920302     C     *IN52         OREQ      '1'
015600920316     C     *IN53         OREQ      '1'
015700911121      *- GIRO LA DATA PER LA STAMPA ----------------------------------*
015800941128     C                   Z-ADD     ARBDTV        G08INV
015900941128     C                   MOVE      *ZEROS        G08DAT
016000941128     C                   MOVEL     '3'           G08ERR
016100941128     C                   CALL      'XSRDA8'
016200941128     C                   PARM                    WLBDA8
016300941128     C                   Z-ADD     G08DAT        DATA1             8 0
016400911119      *
016500930923      * ESEGUO REPERRIMENTO DATI GIACENZA
016600930923     C                   MOVE      *ZERO         GCPAGC
016700930923     C                   MOVE      *ZERO         GCPFGC
016800930923     C                   MOVE      *ZERO         GCPNGC
016900930923     C                   MOVE      ARBAAS        GCPAAS
017000930923     C                   MOVE      ARBLNP        GCPLNP
017100930923     C                   MOVE      ARBNRS        GCPNRS
017200930923     C                   MOVE      ARBNSP        GCPNSP
017300050309     C     KGCA          CHAIN     TIGCP21L                           21
017400930923     C*
017500931119     C*  DECODIFICO TIPO CONSEGNA ANOMALA
017600931118     C                   Z-ADD     1             W                 2 0
017700931118     C                   MOVEL     *BLANKS       DECC
017800931118     C     ARBCCA        LOOKUP    CCA(W)                                 40
017900931118     C   40              MOVE      DCCA(W)       DECC             16
017901060405     C* cerco motivo variazione
017902060405     c                   exsr      srdecmtv
017903060405     c     d962TXT       comp      ' '                                8888
018000931118     C*
018100921029     C   OF              EXCEPT    TESTA
018200911121     C                   EXCEPT    DETTA
018202060405     C   88              EXCEPT    DETTA2
018204060914     c                   exsr      srwtr
018205060405     C   88              EXCEPT    salto
018300991118     C                   MOVEL     'S'           WSTP              1
018400911121      *
018500911121     C                   END
018600920302     C                   END
018700911119      *
018800911119     C                   END
018900911119      *
019000920302     C                   END
019100020927
019200020927     C                   CLOSE     FNARBK2L
019300020927     C                   CLOSE     FNARBK0F
019400020927
019500080117     C*    WSTP          IFEQ      'S'
019701080117     C   OF              EXCEPT    TESTA
019702080117     C                   EXCEPT    FINE
019703080117     C*                  END
019800020927
019900020927      /EJECT
020000020927     C**********************************************************************
020100020927     C*
020200920304     C* STAMPA VARIAZIONE PORTI ASSEGNATI
020300920304     C*
020400020927     C**********************************************************************
020500020927     C                   OPEN      FIARBT2L
020600020927     C                   OPEN      FIARBT0F
020601060914     c                   setoff                                       44
020700020927
020800991118     C                   SETON                                        OF
020900920304     C                   Z-ADD     0             PAGE
021000020927
021100991012     C     *LOVAL        SETLL     FIARBT2L
021200920304     C     *IN31         DOWEQ     '0'
021300020927     C                   READ      FIARBT2L                               31
021400020927
021500060404     c                   IF        NOT %EOF
021501060404      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
021502060404     C                   Z-ADD     1             I
021503060404     C     ARBCVB        LOOKUP    TIPV(I)                                22
021504060404     c* se variazione pre-pagato salto stampo nel pgm fnlr96r
021505060404     c                   if        prpv(i) ='S'
021506060404     c                   iter
021507060404     c                   end
021600020927
021700020927     C                   EXSR      RtvFnArb01L
021800020927
021900020927     C                   IF        NOT *IN01
022000020927     C                   ITER
022100020927     C                   ENDIF
022200020927
022300020927     ** Aggiorno il flag di stampato.
022400020927     C     ArbT2LRRN     CHAIN     FiArbT0F
022500020927     C                   IF        %FOUND
022600020927     C                   MOVEL     'S'           ARBFST
022700020927     C                   UPDATE    FiArbT00
022800020927     C                   ENDIF
022900020927
023300920304     C   22FLGV(I)       IFEQ      'S'
023400920304      *
023500950118     C                   MOVEL     DESV(I)       DCVB             20
023600920304     C                   SETOFF                                       52
023700950118     C                   CLEAR                   AR6IFT
023800950118     C     KARB          CHAIN     FNARB01L                           20
023900991012     C     KAR6          CHAIN     FIAR601L                           20
024000920316     C* IMPORTO FATTURA VARIATO
024100950118     C  N20ARBIFT        IFNE      AR6IFT
024200920304     C                   SETON                                        52
024300920304     C                   END
024400920304     C     *IN52         IFEQ      '1'
024500920304      *- GIRO LA DATA PER LA STAMPA ----------------------------------*
024600941128     C                   Z-ADD     ARBDTV        G08INV
024700941128     C                   MOVE      *ZEROS        G08DAT
024800941128     C                   MOVEL     '3'           G08ERR
024900941128     C                   CALL      'XSRDA8'
025000941128     C                   PARM                    WLBDA8
025100941128     C                   Z-ADD     G08DAT        DATA1
025200930923      *
025300950118     C                   Z-ADD     ARBDFT        G08INV
025400950118     C                   MOVE      *ZEROS        G08DAT
025500950118     C                   MOVEL     '3'           G08ERR
025600950118     C                   CALL      'XSRDA8'
025700950118     C                   PARM                    WLBDA8
025800950118     C                   Z-ADD     G08DAT        GMADFT            8 0
025900950118      *
026000930923      * ESEGUO REPERRIMENTO DATI GIACENZA
026100930923     C                   MOVE      *ZERO         GCPAGC
026200930923     C                   MOVE      *ZERO         GCPFGC
026300930923     C                   MOVE      *ZERO         GCPNGC
026400930923     C                   MOVE      ARBAAS        GCPAAS
026500930923     C                   MOVE      ARBLNP        GCPLNP
026600930923     C                   MOVE      ARBNRS        GCPNRS
026700930923     C                   MOVE      ARBNSP        GCPNSP
026800050309     C     KGCA          CHAIN     TIGCP21L                           21
026900931118     C*
027000931119     C*  DECODIFICO TIPO CONSEGNA ANOMALA
027100931118     C                   Z-ADD     1             W                 2 0
027200931118     C                   MOVEL     *BLANKS       DECC
027300931118     C     ARBCCA        LOOKUP    CCA(W)                                 40
027400931118     C   40              MOVE      DCCA(W)       DECC             16
027500060405     C* cerco motivo variazione
027501060405     c                   exsr      srdecmtv
027502060405     c     d962TXT       comp      ' '                                8888
027600920304      *
027700921029     C   OF              EXCEPT    TESTA1
027800920304     C                   EXCEPT    DETTA1
027801060405     C   88              EXCEPT    DETTA2
027803060914     c                   exsr      srwtr
027804060406     C   88              EXCEPT    salto
027900991118     C                   MOVEL     'S'           WSTP1             1
028000920304      *
028100920304     C                   END
028200920304     C                   END
028300920304      *
028400920304     C                   END
028500920304      *
028600920304     C                   END
028700020927
028800020927     C                   CLOSE     FIARBT2L
028900020927     C                   CLOSE     FIARBT0F
029000020927
029100080117     C*    WSTP1         IFEQ      'S'
029101080117     C   OF              EXCEPT    TESTA1
029200921029     C                   EXCEPT    FINE1
029300080117     C*                  END
029400921029     C*
029500910521     C                   SETON                                        LR
029501060405      *---------------------------------------------------------------*
029502060914     C     srwtr         BEGSR
029503060405      *---------------------------------------------------------------*
029505060914     c                   eval      wvbaas = ARBAAS
029506060914     c                   eval      wvbLNP = ARBLNP
029507060914     c                   eval      wvbLNA = ARBLNA
029508060914     c                   eval      wvbNRS = ARBNRS
029509060914     c                   eval      wvbNSP = ARBNSP
029510060914     c                   eval      wvbDTV = ARBDTV
029511060928     c                   eval      wvborV = ARBorV
029512060928     c                   eval      wvbpru = ARBpru
029513060928     c                   eval      wvbcbop= ARBCBo
029514060928     c                   eval      wvbcbod= ARBCBN
029515060928     c* giacenza
029516060928     c                   if        GCPNGC <> 0
029517060928     c                   eval      wvbgia = 'S'
029518060928     c                   else
029519060928     c                   eval      wvbgia = 'N'
029520060928     c                   end
029521060914     C                   MOVEL     dcvb          wvbcvbd
029522060914     c                   eval      WVBMTVD= d962des
029523060914     c                   clear                   wvbnot
029524060914     c   88              eval      WVBNOT  = %trimr(d962txt)
029525060914     c* variazione c/ass
029526060914     c                   if        *in44
029527060914     c                   eval      wvbimpp= ARBcas
029528060914     c                   eval      wvbdivp= ARBvca
029529060914     c                   eval      wvbimpd= AR9cas
029530060914     c                   eval      wvbdivd= AR9vca
029531060914     c                   eval      wvbtic= ARbtic
029532060914     c   50              eval      wvbdes = 'C/A ANNULLATO'
029533060914     c   51              eval      wvbdes = 'C/A AGGIUNTO '
029534060914     c   52              eval      wvbdes = 'C/A VARIATO  '
029535060914     c   53              eval      wvbdes = 'VAR.COD.BOL. '
029536060914     c                   else
029537060914     c* variazione assegnati
029539060928     c                   clear                   wvbcbod
029540060914     c                   eval      wvbimpp= ARBift
029541060914     c                   eval      wvbdivp= ARBdiv
029542060914     c                   eval      wvbimpd= AR6ift
029543060914     c                   eval      wvbdivd= AR6div
029544060914     c                   clear                   wvbtic
029545060914     c                   clear                   wvbdes
029546060914     c                   end
029547060914     c                   write     fnsvbw00
029548060914     c                   endsr
029549060914      *---------------------------------------------------------------*
029550060914     C     srdecmtv      BEGSR
029551060914      *---------------------------------------------------------------*
029552060405     c                   clear                   fnlr96ds2
029553060405     c                   eval      d962aas = ARBAAS
029554060405     c                   eval      d962LNP = ARBLNP
029555060405     c                   eval      d962NRS = ARBNRS
029556060405     c                   eval      d962NSP = ARBNSP
029557060405     c                   eval      d962ORV = ARBORV
029558060405     c                   eval      d962DTV = ARBDTV
029559060405     c                   eval      d962cvb = ARBcvb
029560060405     c                   movel(p)  fnlr96ds2     kpjbu
029561060405     c                   call      'FNLR96R2'
029562060405     c                   parm                    kpjba
029563060405     c                   movel(p)  kpjbu         fnlr96ds2
029564060405     c                   endsr
029600911119      *
029700911121      *---------------------------------------------------------------*
029800941129     C     *INZSR        BEGSR
029900000000     C     *ENTRY        PLIST
030000000000     C                   PARM                    KPJBA
030002060529     c                   movel     kpjbu         param
030100900516      *
030200941129     C                   TIME                    WHHUDT           14 0
030300941129     C                   MOVEL     WHHUDT        ORA               6 0
030400941129     C                   MOVE      WHHUDT        G08DAT
030500941129     C                   MOVE      *ZEROS        G08INV
030600941129     C                   MOVEL     *BLANK        G08ERR
030700941129     C                   CALL      'XSRDA8'
030800941129     C                   PARM                    WLBDA8
030900941129     C                   Z-ADD     G08DAT        GMAUD8            8 0
031000060405     C                   MOVEL     *ALL'-'       TRATT           198
031100050203     C                   Z-ADD     1             CODUT             1 0
031200050203     c* dati utente
031300050203     c     *dtaara       define    §azute        azuteds
031400050203     c     *dtaara       define    §datiute      ddatiute
031500050203     C                   in(E)     *dtaara
031600050203     C                   IF        %Error  or  RSUT = *blanks
031700050203     C                   call      'TIBS34R'
031800050203     C                   parm                    Tibs34Ds
031900050203     C                   in        *dtaara
032000050203     c                   ENDIF
032100911119      *
032200920304      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
032300920304     C                   MOVEL     '7L'          CODTAB
032400920304     C                   MOVE      *BLANKS       KEYTAB
032500920304     C                   Z-ADD     0             I
032600920304     C     KTAB          SETLL     TABEL00F                           22
032700920304     C     *IN22         DOWEQ     '0'
032800920304     C     KTAB1         READE     TABEL00F                               22
032900920304     C  N22              DO
033000920304     C                   ADD       1             I                 3 0
033100920304     C                   MOVEL     TBLUNI        DS7L
033200920304     C                   MOVEL     TBLKEY        TIPV(I)
033300920304     C                   MOVEL     §7LSTF        FLGV(I)
033400920304     C                   MOVEL     §7LDES        DESV(I)
033401060404     C                   MOVEL     §7Lprp        prpv(I)
033500920304     C                   END
033600920304     C                   END
033700920316      *- CARICO TABELLA TIPI BOLLA
033800920316     C                   MOVEL     '3A'          CODTAB
033900920316     C                   MOVE      *BLANKS       KEYTAB
034000920316     C                   Z-ADD     0             I
034100920316     C     KTAB          SETLL     TABEL00F                           22
034200920316     C     *IN22         DOWEQ     '0'
034300920316     C     KTAB1         READE     TABEL00F                               22
034400920316     C  N22              DO
034500920316     C                   ADD       1             I                 3 0
034600920316     C                   MOVEL     TBLUNI        DS3A
034700920316     C                   MOVEL     TBLKEY        TIPB(I)
034800920316     C                   MOVEL     §3ATB1        TB1(I)
034900920316     C                   MOVE      §3ATB2        TB2(I)
035000920316     C                   END
035100920316     C                   END
035200931118      *
035300931119      *- CARICO DESCRIZIONI CONS.ANOMALA         ---------------------*
035400931118     C                   MOVEL     '7O'          CODTAB
035500931118     C                   MOVE      *BLANKS       KEYTAB
035600931118     C                   Z-ADD     0             I
035700931118     C     KTAB          SETLL     TABEL00F                           22
035800931118     C     *IN22         DOWEQ     '0'
035900931118     C     KTAB1         READE     TABEL00F                               22
036000931118     C     *IN22         IFEQ      '0'
036100931118     C     I             ANDLT     20
036200931118     C                   ADD       1             I                 3 0
036300931118     C                   MOVEL     TBLKEY        CCA(I)
036400931118     C                   MOVEL     TBLUNI        DCCA(I)
036500931118     C                   END
036600931118     C                   END
036700020927
036701060404      * ricarico la £6 se veniva cambiato il P.O aveva i dati errati
036702060404     c                   clear                   trul06ds
036703060404     C                   MOVEL     '£6'          D06COD
036705060529     C                   MOVEL     parmpo        D06KEY
036706060404     C                   MOVEL     TRul06ds      KPJBU
036707060404     C                   CALL      'TRUL06R'
036708060404     C                   PARM                    KPJBA
036709060404     C                   MOVEL     KPJBU         TRul06ds
037800020927
037900911119      *- CHIAVE X TABEL00F -------------------------------------------*
038000911106     C     KTAB          KLIST
038100911106     C                   KFLD                    CODUT
038200911106     C                   KFLD                    CODTAB            2
038300911106     C                   KFLD                    KEYTAB            8
038400920304     C     KTAB1         KLIST
038500920304     C                   KFLD                    CODUT
038600920304     C                   KFLD                    CODTAB
038700941129      *
038800941129     C     KARB          KLIST
038900941129     C                   KFLD                    ARBAAS
039000941129     C                   KFLD                    ARBLNP
039100941129     C                   KFLD                    ARBNRS
039200941129     C                   KFLD                    ARBNSP
039300991012     C     KAR6          KLIST
039400991012     C                   KFLD                    ARBAAS
039500991012     C                   KFLD                    ARBLNP
039600991012     C                   KFLD                    ARBNRS
039700991012     C                   KFLD                    ARBNSP
039800991012     C                   KFLD                    ARBTRC
039900941129     C*
040000941129     C*  CHIAVE PER POSIZIONAMENTO SU ARCHIVIO GIACENZE
040100941129     C     KGCA          KLIST
040200941129     C                   KFLD                    GCPAAS
040300941129     C                   KFLD                    GCPLNP
040400941129     C                   KFLD                    GCPNRS
040500941129     C                   KFLD                    GCPNSP
040600941129      *
040700941129      *- APRO FILE DI STAMPA -----------------------------------------*
040800941129     C                   Z-ADD     60            LUNCMD           15 5
040900941129     C                   CALL      'QCMDEXC'
041000941129     C                   PARM                    CMDP
041100941129     C                   PARM                    LUNCMD
041200941129     C                   OPEN      PRTF198
041300941129     C*
041400991118     C                   SETON                                        OF
041500941129     C*
041600911121      *---------------------------------------------------------------*
041700910521     C                   ENDSR
041800900606      *----------------------------------------------------------------
041900020927
042000020927     ***********************************************************************
042100020927     **
042200020927     ** Reperisco il record di testata della bolla.
042300020927     **
042400020927     ***********************************************************************
042500020927     C     RtvFnArb01L   BEGSR
042600020927
042700020927     C     KARB          CHAIN     FNARB01L                           20
042800020927
042900020927     ** Seleziono le linee di arrivo del terminal dell'utente.
043000020927     C                   IF        %FOUND
043100040316     C     ArbLnA        LOOKUP    Lin                                    01
043200020927     C                   ELSE
043300020927     C                   EVAL      *IN01 = *OFF
043400020927     C                   ENDIF
043500020927
043600020927     C                   ENDSR
043700020927
043800941129     OPRTF198   E            TESTA          1 01
043900911106     O                       RSUT                20
044000060529     O                       parmpo              28
044100911121     O                                           62 'ELENCO VARIAZIONI'
044200911119     O                                         +  1 'C/ASSEGNI'
044300941129     O                       §PGM               110
044400941129     O                       GMAUD8             130 '  /  /    '
044500941129     O                                          150 'PAG.'
044600941129     O                       PAGE               156
044700911106      *
044800941129     O          E            TESTA          1
044900941129     O                       KNSIF             +  0
045000941129     O                       KNMUS             +  1
045100941129     O                       ORA                130 '  :  :  '
045200941129     O          E            TESTA          1
045300911120     O                       TRATT             +  0
045400911106      *
045500941129     O          E            TESTA          1
045600941128     O                                            8 '  DATA'
045700941128     O                                           24 'C A U S A L E'
045800941128     O                                           45 'SPEDIZIONE'
045900950118     O                                          122 ' DATI GIACENZA '
046000941129     O          E            TESTA          1
046100941129     O                                            9 'VARIAZ.'
046200941128     O                                           25 'CD DESCRIZIONE'
046300941128     O                                           38 'LNP'
046400941128     O                                           42 'LNA'
046500941129     O                                           46 'AA'
046600941128     O                                           50 'NR'
046700941128     O                                           58 'SPEDIZ.'
046800941129     O                                           75 'C/A VECCHIO'
046900941129     O                                           97 'C/A NUOVO'
047000941129     O                                          105 'TP'
047100941129     O                                          109 'AA'
047200941129     O                                          114 'FGC '
047300941129     O                                          122 'NUMERO'
047400950118     O                                         +  1 'CONSEGNA ANOMALA'
047401060406     O                                          163 'MOTIVO'
047500911106      *
047600941129     O          E            TESTA          1
047700911120     O                       TRATT             +  0
047800911120      *
047900911121     O          E            DETTA       1
048000941128     O                       DATA1               10 '  /  /    '
048100941128     O                       ARBCVB              13
048200941128     O                       DCVB                34
048300941128     O                       ARBLNP              38
048400941128     O                       ARBLNA              42
048500941128     O                       ARBAAS        Z     47
048600941128     O                       ARBNRS              50
048700941128     O                       ARBNSP        Z     58
048800941129     O                       ARBCAS        2     76
048900941129     O                       ARBVCA              80
049000941129     O                       AR9CAS        2     98
049100941129     O                       AR9VCA             102
049200941129     O                       ARBTIC             105
049300941129     O                       GCPAGC        Z    110
049400941129     O                       GCPFGC        Z    114
049500941129     O                       GCPNGC        Z    122
049600941129     O                       ARBCCA             124
049700950118     O                       DECC              +  1
049800941129     O               50                         156 'C/A ANNULLATO'
049900941129     O               51                         156 'C/A AGGIUNTO '
050000941129     O               52                         156 'C/A VARIATO  '
050100950118     O               53                         156 'VAR.COD.BOL. '
050101060406     O                       D962des           +  1
050200921029     O          E            FINE        1
050300941129     O                                           70 'FINE STAMPA'
050400921029     O                                         +  1 'VARIAZIONI'
050500921029     O                                         +  1 'CONTRASSEGNI'
050600941129     O          E            TESTA1         1 01
050700920304     O                       RSUT                20
050800060529     O                       parmpo              28
050900941129     O                                           57 'ELENCO VARIAZIONI'
051000941129     O                                         +  1 'PORTI ASSEGNATI'
051100941129     O                       §PGM               110
051200941129     O                       GMAUD8             130 '  /  /    '
051300941129     O                                          150 'PAG.'
051400941129     O                       PAGE               156
051500941129      *
051600950118     O          E            TESTA1         1
051700941129     O                       KNSIF             +  0
051800941129     O                       KNMUS             +  1
051900941129     O                       ORA                130 '  :  :  '
052000920304      *
052100941129     O          E            TESTA1         1
052200920304     O                       TRATT             +  0
052300920304      *
052400941129     O          E            TESTA1         1
052500941129     O                                            8 '  DATA'
052600950118     O                                           24 'C A U S A L E'
052700950118     O                                           45 'SPEDIZIONE'
052800950118     O                                          117 ' DATI GIACENZA'
052900941129     O          E            TESTA1         1
053000941128     O                                           10 'VARIAZ.'
053100941129     O                                           25 'CD DESCRIZIONE'
053200941129     O                                           38 'LNP'
053300941129     O                                           42 'LNA'
053400941129     O                                           46 'AA'
053500941129     O                                           50 'NR'
053600941129     O                                           58 'SPEDIZ.'
053700991012     O                                           72 'IMPOR.VECCHIO'
053800991012     O                                           94 'IMPORTO NUOVO'
053900991012     O                                          110 'N.FATT.'
054000991012     O                                          118 'DATA'
054100991012     O                                          125 'AA'
054200991012     O                                          130 'FGC'
054300991012     O                                          138 'NUMERO'
054400950118     O                                         +  1 'CONSEGNA ANOMALA'
054401060406     O                                         +  3 'MOTIVO'
054500920304      *
054600941129     O          E            TESTA1         1
054700920304     O                       TRATT             +  0
054800920304      *
054900920304     O          E            DETTA1      1
055000941129     O                       DATA1               10 '  /  /    '
055100941129     O                       ARBCVB              13
055200941129     O                       DCVB                34
055300941129     O                       ARBLNP              38
055400941129     O                       ARBLNA              42
055500941129     O                       ARBAAS        Z     47
055600941129     O                       ARBNRS              50
055700941129     O                       ARBNSP        Z     58
055800991012     O                       ARBIFT        2     76
055900991012     O                       ARBDIV              80
056000991012     O                       AR6IFT        2     98
056100991012     O                       AR6DIV             102
056200991012     O                       ARBNFT        Z    110
056300991012     O                       GMADFT             121 '  /  /    '
056400991012     O                       GCPAGC        Z    126
056500991012     O                       GCPFGC        Z    130
056600991012     O                       GCPNGC        Z    138
056700991012     O                       ARBCCA             140
056800950118     O                       DECC              +  1
056801060406     O                       D962des           +  1
056802060405     O          E            DETTA2      1
056803060406     o                       d962TXT           +  0
056804060405     O          E            salto       1
056900921029     O          E            FINE1       1
057000941129     O                                           70 'FINE STAMPA'
057100921029     O                                         +  1 'VARIAZIONI'
057200921029     O                                         +  1 'ASSEGNATI'
057300911119      *
057400950118**  CMDP
057500950118OVRPRTF FILE(PRTF198) USRDTA(ELEN_VARIA) SAVE(*YES)
