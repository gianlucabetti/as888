000100950208     H DECEDIT('0,') DATEDIT(*DMY.)
000200060404      * FNLR96R *----------------------------------------------------*
000300060404      * STAMPA ELENCO VARIAZIONI pre-pagati
000400911119      *--------------------------------------------------------------*
000500020927     FFNARBK2L  IF   E           K DISK
000600020927     F                                     INFDS(InfArbK2L)
000700020927     F                                     RENAME(FnArbK00:FnArbK20)
000800020927     FFNARBK0F  UF   E             DISK
000900020927     FFIARBT2L  IF   E           K DISK
001000020927     F                                     INFDS(InfArbT2L)
001100020927     F                                     RENAME(FiArbT00:FiArbT20)
001200020927     FFIARBT0F  UF   E             DISK
001300060404     FFNblp01L  IF   E           K DISK
001400991012     FFIAR601L  IF   E           K DISK
001500911106     FTABEL00F  IF   E           K DISK
001600030204     Fazorg01l  IF   E           K DISK
001700060914     Ffnsvbw0f  O    e             disk
001800060404     FPRTF198   O    F  198        PRINTER OFLIND(*INOF) usropn
001900020927
002000020927     D InfArbK2L       DS
002100020927     D  ArbK2LRRN            397    400I 0
002200020927
002300020927     D InfArbT2L       DS
002400020927     D  ArbT2LRRN            397    400I 0
002500020927
002600060404     D TIPV            S              2    DIM(99)                              COD.VARIAZIONE
002700920304     D FLGV            S              1    DIM(99)                              FLAG STAMPA
002800920304     D DESV            S             20    DIM(99)                              DESCR. VARIAZ.
002900060404     D prpV            S              1    DIM(99)                              FLAG STAMPA
003000911106     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)            PRINTER OVRPRTF
003100170320     d
003200060404     d gmadft          s              8  0 inz
003300900515      *
003400941128     D WLBDA8          DS
003500941128     D  G08DAT                 1      8  0
003600941128     D  G08INV                 9     16  0
003700941128     D  G08ERR                17     17
003800941128     D  G08TGI                18     22  0
003900060529     D param           DS
004000060529     D  parmpo                 1      3s 0
004100050203     D ddatiute      e ds
004200050203     D azuteds       e ds                  extname(AZUTE00F)
004300050203     D tibs34ds      E DS                  inz
004400060405     D fnlr96ds2     E DS                  inz
004500911119     D DS7L          E DS
004600900514     D KPJBA         E DS
004700941129     D                SDS
004800941129     D  §PGM                   1     10
004900170320     d
005000170320     d conta           s              3  0 inz
005100020927
005200020927     ***********************************************************************
005300020927     **
005400020927     ** STAMPA VARIAZIONE BOLLE
005500020927     **
005600020927     ***********************************************************************
005700170320     c                   clear                   conta
005800020927
005900941128     C     *LOVAL        SETLL     FNARBK2L
006000060404     C                   DO        *hival
006100060404     C                   READ      FNARBK2L
006200060404     c                   if        %eof(fnarbk2l)
006300060404     c                   leave
006400060404     c                   end
006500060529     c                   if        arblnp <> parmpo
006600060406     c                   iter
006700060406     c                   end
006800060404      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
006900060404     C                   Z-ADD     1             I
007000060404     C     ARBCVB        LOOKUP    TIPV(I)                                22
007100060404     c                   if        not *in22
007200060404     c                   iter
007300060404     c                   end
007400060404     c* se variazione pre-pagato
007500060404     c                   if        prpv(i) <>'S'
007600060404     c                   iter
007700060404     c                   end
007800020927
007900060404     C     Kblp          CHAIN     FNblp01L
008000060404     C                   IF        NOT %found(fnblp01l)
008100020927     C                   ITER
008200020927     C                   ENDIF
008300060404     c                   exsr      srdecmtv
008400060405     c     d962TXT       comp      ' '                                8888
008500060404     c* da stampare
008600060404     C     FLGV(I)       IFEQ      'S'
008700920304     C                   MOVEL     DESV(I)       DCVB             20
008800060405     C                   CLEAR                   fiar6000
008900060405     c                   eval      arbtrc = '1'
009000060405     C     KAR6          CHAIN     FIAR601L                           20
009100911121      *- GIRO LA DATA PER LA STAMPA ----------------------------------*
009200941128     C                   Z-ADD     ARBDTV        G08INV
009300941128     C                   MOVE      *ZEROS        G08DAT
009400941128     C                   MOVEL     '3'           G08ERR
009500941128     C                   CALL      'XSRDA8'
009600941128     C                   PARM                    WLBDA8
009700941128     C                   Z-ADD     G08DAT        DATA1             8 0
009800060405      *
009900060405     C                   Z-ADD     AR6DFT        G08INV
010000060405     C                   MOVE      *ZEROS        G08DAT
010100060405     C                   MOVEL     '3'           G08ERR
010200060405     C                   CALL      'XSRDA8'
010300060405     C                   PARM                    WLBDA8
010400060405     C                   Z-ADD     G08DAT        GMADFT
010500170320     c
010600170320     c                   add       arbift        iftold           15 3
010700170320     c                   add       ar6ift        iftnew           15 3
010800931118     C*
010900060404     C   OF              EXCEPT    TESTA1
011000170320     c
011100170320     c                   add       1             conta
011200060404     C                   EXCEPT    DETTA1
011300060404     C   88              EXCEPT    DETTA2
011400060914     c                   exsr      srwtr
011500060406     C   88              EXCEPT    salto
011600991118     C                   MOVEL     'S'           WSTP              1
011700060404     C                   ENDIF
011800060404     ** Aggiorno il flag di stampato.
011900060404     C     ArbK2LRRN     CHAIN     FnArbK0F
012000060404     C                   IF        %FOUND
012100060404     C                   MOVEL     'S'           ARBFST
012200060404     C                   UPDATE    FnArbK00
012300060404     C                   ENDIF
012400060404     C                   ENDDO
012500020927
012600020927
012700020927      /EJECT
012800020927     C**********************************************************************
012900020927     C*
013000060404     C* STAMPA VARIAZIONE tassazione
013100920304     C*
013200020927     C**********************************************************************
013300060928     c                   clear                   arbcbn
013400991012     C     *LOVAL        SETLL     FIARBT2L
013500060404     c                   do        *hival
013600060404     C                   READ      FIARBT2L
013700060404     c                   IF        %EOF(fiarbt2l)
013800060404     c                   leave
013900060404     c                   end
014000060529     c                   if        arblnp <> parmpo
014100060406     c                   iter
014200060406     c                   end
014300060404      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
014400060404     C                   Z-ADD     1             I
014500060404     C     ARBCVB        LOOKUP    TIPV(I)                                22
014600060404     c                   if        not *in22
014700060404     c                   iter
014800060404     c                   end
014900060404     c* se variazione pre-pagato
015000060404     c                   if        prpv(i) <>'S'
015100060404     c                   iter
015200060404     c                   end
015300060404     C     Kblp          CHAIN     FNblp01L
015400060404     C                   IF        NOT %found(fnblp01l)
015500060404     C                   ITER
015600060404     C                   ENDIF
015700060404     c                   exsr      srdecmtv
015800060405     c     d962TXT       comp      ' '                                8888
015900060404     c* da stampare
016000060404     C     FLGV(I)       IFEQ      'S'
016100950118     C                   MOVEL     DESV(I)       DCVB             20
016200920304     C                   SETOFF                                       52
016300950118     C                   CLEAR                   AR6IFT
016400991012     C     KAR6          CHAIN     FIAR601L                           20
016500920316     C* IMPORTO FATTURA VARIATO
016600950118     C  N20ARBIFT        IFNE      AR6IFT
016700920304     C                   SETON                                        52
016800920304     C                   END
016900920304     C     *IN52         IFEQ      '1'
017000920304      *- GIRO LA DATA PER LA STAMPA ----------------------------------*
017100941128     C                   Z-ADD     ARBDTV        G08INV
017200941128     C                   MOVE      *ZEROS        G08DAT
017300941128     C                   MOVEL     '3'           G08ERR
017400941128     C                   CALL      'XSRDA8'
017500941128     C                   PARM                    WLBDA8
017600941128     C                   Z-ADD     G08DAT        DATA1
017700930923      *
017800060405     C                   Z-ADD     AR6DFT        G08INV
017900950118     C                   MOVE      *ZEROS        G08DAT
018000950118     C                   MOVEL     '3'           G08ERR
018100950118     C                   CALL      'XSRDA8'
018200950118     C                   PARM                    WLBDA8
018300060404     C                   Z-ADD     G08DAT        GMADFT
018400170320
018500170320     c                   add       arbift        iftold           15 3
018600170320     c                   add       ar6ift        iftnew           15 3
018700920304      *
018800921029     C   OF              EXCEPT    TESTA1
018900170320     c                   add       1             conta
019000920304     C                   EXCEPT    DETTA1
019100060404     C   88              EXCEPT    DETTA2
019200060914     c                   exsr      srwtr
019300060406     C   88              EXCEPT    salto
019400991118     C                   MOVEL     'S'           WSTP1             1
019500920304     C                   END
019600920304     C                   END
019700060404     ** Aggiorno il flag di stampato.
019800060404     C     ArbT2LRRN     CHAIN     FiArbT0F
019900060404     C                   IF        %FOUND
020000060404     C                   MOVEL     'S'           ARBFST
020100060404     C                   UPDATE    FiArbT00
020200060404     C                   ENDIF
020300060404     C                   ENDdo
020400020927
020500080117     C*    WSTP1         IFEQ      'S'
020600080117     C*    WSTP          orEQ      'S'
020700080117     C   OF              EXCEPT    TESTA1
020800921029     C                   EXCEPT    FINE1
020900080117     C*                  END
021000921029     C*
021100910521     C                   SETON                                        LR
021200060914      *---------------------------------------------------------------*
021300060914     C     srwtr         BEGSR
021400060914      *---------------------------------------------------------------*
021500060914     c                   eval      wvbaas = ARBAAS
021600060914     c                   eval      wvbLNP = ARBLNP
021700060914     c                   eval      wvbLNA = blpLNA
021800060914     c                   eval      wvbNRS = ARBNRS
021900060914     c                   eval      wvbNSP = ARBNSP
022000060914     c                   eval      wvbDTV = ARBDTV
022100060928     c                   eval      wvborV = ARBorV
022200060928     c                   eval      wvbpru = ARBpru
022300060928     c                   eval      wvbcbop= ARBCBo
022400060928     c                   eval      wvbcbod= ARBCBN
022500060914     C                   MOVEL     dcvb          wvbcvbd
022600060914     c                   eval      WVBMTVD= d962des
022700060914     c                   clear                   wvbnot
022800060914     c   88              eval      WVBNOT  = %trimr(d962txt)
022900060928     c                   eval      wvbgia = 'N'
023000060914     c                   eval      wvbimpp= ARBift
023100060914     c                   eval      wvbdivp= ARBdiv
023200060914     c                   eval      wvbimpd= AR6ift
023300060914     c                   eval      wvbdivd= AR6div
023400060914     c                   clear                   wvbtic
023500060914     c                   clear                   wvbdes
023600060914     c                   write     fnsvbw00
023700060914     c                   endsr
023800911121      *---------------------------------------------------------------*
023900060404     C     srdecmtv      BEGSR
024000060404      *---------------------------------------------------------------*
024100060405     c                   clear                   fnlr96ds2
024200060405     c                   eval      d962aas = ARBAAS
024300060405     c                   eval      d962LNP = ARBLNP
024400060405     c                   eval      d962NRS = ARBNRS
024500060405     c                   eval      d962NSP = ARBNSP
024600060405     c                   eval      d962ORV = ARBORV
024700060405     c                   eval      d962DTV = ARBDTV
024800060405     c                   eval      d962cvb = ARBcvb
024900060405     c                   movel(p)  fnlr96ds2     kpjbu
025000060405     c                   call      'FNLR96R2'
025100060405     c                   parm                    kpjba
025200060405     c                   movel(p)  kpjbu         fnlr96ds2
025300060404     c                   endsr
025400060404      *---------------------------------------------------------------*
025500060404     C     *INZSR        BEGSR
025600060404      *---------------------------------------------------------------*
025700000000     C     *ENTRY        PLIST
025800000000     C                   PARM                    KPJBA
025900060529     c                   movel     kpjbu         param
026000900516      *
026100941129     C                   TIME                    WHHUDT           14 0
026200941129     C                   MOVEL     WHHUDT        ORA               6 0
026300941129     C                   MOVE      WHHUDT        G08DAT
026400941129     C                   MOVE      *ZEROS        G08INV
026500941129     C                   MOVEL     *BLANK        G08ERR
026600941129     C                   CALL      'XSRDA8'
026700941129     C                   PARM                    WLBDA8
026800941129     C                   Z-ADD     G08DAT        GMAUD8            8 0
026900950118     C                   MOVEL     *ALL'-'       TRATT           156
027000050203     C                   Z-ADD     1             CODUT             1 0
027100050203     c* dati utente
027200050203     c     *dtaara       define    §azute        azuteds
027300050203     c     *dtaara       define    §datiute      ddatiute
027400050203     C                   in(E)     *dtaara
027500050203     C                   IF        %Error  or  RSUT = *blanks
027600050203     C                   call      'TIBS34R'
027700050203     C                   parm                    Tibs34Ds
027800050203     C                   in        *dtaara
027900050203     c                   ENDIF
028000911119      *
028100920304      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
028200920304     C                   MOVEL     '7L'          CODTAB
028300920304     C                   MOVE      *BLANKS       KEYTAB
028400920304     C                   Z-ADD     0             I
028500920304     C     KTAB          SETLL     TABEL00F                           22
028600920304     C     *IN22         DOWEQ     '0'
028700920304     C     KTAB1         READE     TABEL00F                               22
028800920304     C  N22              DO
028900920304     C                   ADD       1             I                 3 0
029000920304     C                   MOVEL     TBLUNI        DS7L
029100920304     C                   MOVEL     TBLKEY        TIPV(I)
029200920304     C                   MOVEL     §7LSTF        FLGV(I)
029300920304     C                   MOVEL     §7LDES        DESV(I)
029400060404     C                   MOVEL     §7Lprp        prpv(I)
029500920304     C                   END
029600920304     C                   END
029700911119      *- CHIAVE X TABEL00F -------------------------------------------*
029800911106     C     KTAB          KLIST
029900911106     C                   KFLD                    CODUT
030000911106     C                   KFLD                    CODTAB            2
030100911106     C                   KFLD                    KEYTAB            8
030200920304     C     KTAB1         KLIST
030300920304     C                   KFLD                    CODUT
030400920304     C                   KFLD                    CODTAB
030500941129      *
030600060404     C     Kblp          KLIST
030700941129     C                   KFLD                    ARBAAS
030800941129     C                   KFLD                    ARBLNP
030900941129     C                   KFLD                    ARBNRS
031000941129     C                   KFLD                    ARBNSP
031100991012     C     KAR6          KLIST
031200991012     C                   KFLD                    ARBAAS
031300991012     C                   KFLD                    ARBLNP
031400991012     C                   KFLD                    ARBNRS
031500991012     C                   KFLD                    ARBNSP
031600991012     C                   KFLD                    ARBTRC
031700941129      *
031800941129      *- APRO FILE DI STAMPA -----------------------------------------*
031900941129     C                   Z-ADD     60            LUNCMD           15 5
032000941129     C                   CALL      'QCMDEXC'
032100941129     C                   PARM                    CMDP
032200941129     C                   PARM                    LUNCMD
032300941129     C                   OPEN      PRTF198
032400941129     C*
032500991118     C                   SETON                                        OF
032600941129     C*
032700910521     C                   ENDSR
032800020927
032900060404     OPRTF198   E            TESTA1         1 01
033000920304     O                       RSUT                20
033100060529     O                       parmpo              28
033200941129     O                                           57 'ELENCO VARIAZIONI'
033300060404     O                                         +  1 'PRE-PAGATI'
033400941129     O                       §PGM               110
033500941129     O                       GMAUD8             130 '  /  /    '
033600941129     O                                          150 'PAG.'
033700941129     O                       PAGE               156
033800941129      *
033900950118     O          E            TESTA1         1
034000941129     O                       KNSIF             +  0
034100941129     O                       KNMUS             +  1
034200941129     O                       ORA                130 '  :  :  '
034300920304      *
034400941129     O          E            TESTA1         1
034500920304     O                       TRATT             +  0
034600920304      *
034700941129     O          E            TESTA1         1
034800941129     O                                            8 '  DATA'
034900950118     O                                           24 'C A U S A L E'
035000950118     O                                           45 'SPEDIZIONE'
035100941129     O          E            TESTA1         1
035200170320     O                                           15 'VARIAZ.'
035300170320     O                                           30 'CD DESCRIZIONE'
035400170320     O                                           43 'LNP'
035500170320     O                                           47 'LNA'
035600170320     O                                           51 'AA'
035700170320     O                                           55 'NR'
035800170320     O                                           63 'SPEDIZ.'
035900170320     O                                           77 'IMP.PRIMA VAR.'
036000170320     O                                           99 'IMPORTO FATT.'
036100170320     O                                          115 'N.FATT.'
036200170320     O                                          123 'DATA'
036300170320     O                                          132 'MOTIVO'
036400941129     O          E            TESTA1         1
036500920304     O                       TRATT             +  0
036600920304      *
036700060405     O          E            DETTA1      1
036800170320     O                       conta         z      3
036900170320     o                                            4 ')'
037000170320     O                       DATA1               15 '  /  /    '
037100170320     O                       ARBCVB              18
037200170320     O                       DCVB                39
037300170320     O                       ARBLNP              43
037400170320     O                       blpLNA              47
037500170320     O                       ARBAAS        Z     52
037600170320     O                       ARBNRS              55
037700170320     O                       ARBNSP        Z     63
037800170320     O                       ARBIFT        2     81
037900170320     O                       ARBDIV              85
038000170320     O                       AR6IFT        2    103
038100170320     O                       AR6DIV             107
038200170320     O                       AR6NFT        Z    115
038300170320     O                       GMADFT             126 '  /  /    '
038400060405     o                       d962des           +  1
038500060404     O          E            DETTA2      1
038600060405     o                       d962TXT           +  0
038700060405     O          E            salto       1
038800170320     O          E            FINE1       1
038900170321     O                       TRATT             +  0
039000170320     O          E            FINE1       1
039100170320     o                                        +   2 'Totale spedizioni:'
039200170320     o                       conta         2  +   2
039300170320     o                                           58 'Totale importi:'
039400170320     O                       iftold        2     81
039500170320     O                       iftnew        2    103
039600921029     O          E            FINE1       1
039700170321     O                       TRATT             +  0
039800941129     O                                           70 'FINE STAMPA'
039900170320     O                                         +  0 ' VARIAZIONI'
040000170320     O                                         +  0 ' PRE-PAGATI'
040100911119      *
040200950118**  CMDP
040300060404OVRPRTF FILE(PRTF198) USRDTA(VARIA_PRE) SAVE(*YES)
