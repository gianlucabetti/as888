000100950208     H DECEDIT('0,') DATEDIT(*DMY.)
000200060404      * FNLR96R *----------------------------------------------------*
000300911119      *-----------------*                                            *
000400060404      * STAMPA ELENCO VARIAZIONI pre-pagati
000500911119      *--------------------------------------------------------------*
000600020927     FFNARBK2L  IF   E           K DISK
000700020927     F                                     INFDS(InfArbK2L)
000800020927     F                                     RENAME(FnArbK00:FnArbK20)
001000020927     FFNARBK0F  UF   E             DISK
001200020927     FFIARBT2L  IF   E           K DISK
001300020927     F                                     INFDS(InfArbT2L)
001400020927     F                                     RENAME(FiArbT00:FiArbT20)
001600020927     FFIARBT0F  UF   E             DISK
001800060404     FFNblp01L  IF   E           K DISK
002000991012     FFIAR601L  IF   E           K DISK
002100911106     FTABEL00F  IF   E           K DISK
002300030204     Fazorg01l  IF   E           K DISK
002301060914     Ffnsvbw0f  O    e             disk
002400060404     FPRTF198   O    F  198        PRINTER OFLIND(*INOF) usropn
002600020927
002700020927     D InfArbK2L       DS
002800020927     D  ArbK2LRRN            397    400I 0
002900020927
003000020927     D InfArbT2L       DS
003100020927     D  ArbT2LRRN            397    400I 0
003200020927
003902060404     D TIPV            S              2    DIM(99)                              COD.VARIAZIONE
004000920304     D FLGV            S              1    DIM(99)                              FLAG STAMPA
004100920304     D DESV            S             20    DIM(99)                              DESCR. VARIAZ.
004101060404     D prpV            S              1    DIM(99)                              FLAG STAMPA
004700911106     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)            PRINTER OVRPRTF
004701060404     d gmadft          s              8  0 inz
004800900515      *
004900941128     D WLBDA8          DS
005000941128     D  G08DAT                 1      8  0
005100941128     D  G08INV                 9     16  0
005200941128     D  G08ERR                17     17
005300941128     D  G08TGI                18     22  0
005301060529     D param           DS
005302060529     D  parmpo                 1      3s 0
005400050203     D ddatiute      e ds
005500050203     D azuteds       e ds                  extname(AZUTE00F)
005600050203     D tibs34ds      E DS                  inz
005601060405     D fnlr96ds2     E DS                  inz
005900911119     D DS7L          E DS
006000900514     D KPJBA         E DS
006300941129     D                SDS
006400941129     D  §PGM                   1     10
006500020927
006600020927     ***********************************************************************
006700020927     **
006800020927     ** STAMPA VARIAZIONE BOLLE
006900020927     **
007000020927     ***********************************************************************
007300020927
007400941128     C     *LOVAL        SETLL     FNARBK2L
007500060404     C                   DO        *hival
007600060404     C                   READ      FNARBK2L
007601060404     c                   if        %eof(fnarbk2l)
007602060404     c                   leave
007603060404     c                   end
007604060529     c                   if        arblnp <> parmpo
007605060406     c                   iter
007606060406     c                   end
007801060404      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
007802060404     C                   Z-ADD     1             I
007803060404     C     ARBCVB        LOOKUP    TIPV(I)                                22
007804060404     c                   if        not *in22
007805060404     c                   iter
007806060404     c                   end
007807060404     c* se variazione pre-pagato
007808060404     c                   if        prpv(i) <>'S'
007809060404     c                   iter
007810060404     c                   end
007900020927
008001060404     C     Kblp          CHAIN     FNblp01L
008200060404     C                   IF        NOT %found(fnblp01l)
008300020927     C                   ITER
008400020927     C                   ENDIF
008410060404     c                   exsr      srdecmtv
008411060405     c     d962TXT       comp      ' '                                8888
008501060404     c* da stampare
010800060404     C     FLGV(I)       IFEQ      'S'
011000920304     C                   MOVEL     DESV(I)       DCVB             20
011001060405     C                   CLEAR                   fiar6000
011002060405     c                   eval      arbtrc = '1'
011003060405     C     KAR6          CHAIN     FIAR601L                           20
015700911121      *- GIRO LA DATA PER LA STAMPA ----------------------------------*
015800941128     C                   Z-ADD     ARBDTV        G08INV
015900941128     C                   MOVE      *ZEROS        G08DAT
016000941128     C                   MOVEL     '3'           G08ERR
016100941128     C                   CALL      'XSRDA8'
016200941128     C                   PARM                    WLBDA8
016300941128     C                   Z-ADD     G08DAT        DATA1             8 0
016301060405      *
016302060405     C                   Z-ADD     AR6DFT        G08INV
016303060405     C                   MOVE      *ZEROS        G08DAT
016304060405     C                   MOVEL     '3'           G08ERR
016305060405     C                   CALL      'XSRDA8'
016306060405     C                   PARM                    WLBDA8
016307060405     C                   Z-ADD     G08DAT        GMADFT
018000931118     C*
018100060404     C   OF              EXCEPT    TESTA1
018200060404     C                   EXCEPT    DETTA1
018201060404     C   88              EXCEPT    DETTA2
018202060914     c                   exsr      srwtr
018203060406     C   88              EXCEPT    salto
018300991118     C                   MOVEL     'S'           WSTP              1
018301060404     C                   ENDIF
019001060404     ** Aggiorno il flag di stampato.
019002060404     C     ArbK2LRRN     CHAIN     FnArbK0F
019003060404     C                   IF        %FOUND
019004060404     C                   MOVEL     'S'           ARBFST
019005060404     C                   UPDATE    FnArbK00
019006060404     C                   ENDIF
019007060404     C                   ENDDO
019400020927
019800020927
019900020927      /EJECT
020000020927     C**********************************************************************
020100020927     C*
020200060404     C* STAMPA VARIAZIONE tassazione
020300920304     C*
020400020927     C**********************************************************************
020401060928     c                   clear                   arbcbn
021100991012     C     *LOVAL        SETLL     FIARBT2L
021101060404     c                   do        *hival
021300060404     C                   READ      FIARBT2L
021500060404     c                   IF        %EOF(fiarbt2l)
021501060404     c                   leave
021502060404     c                   end
021503060529     c                   if        arblnp <> parmpo
021504060406     c                   iter
021505060406     c                   end
021506060404      *- PRENDO DESCRIZ.CAUS.VARIAZ.BOLLE ARRIVO ---------------------*
021507060404     C                   Z-ADD     1             I
021508060404     C     ARBCVB        LOOKUP    TIPV(I)                                22
021509060404     c                   if        not *in22
021510060404     c                   iter
021511060404     c                   end
021512060404     c* se variazione pre-pagato
021513060404     c                   if        prpv(i) <>'S'
021514060404     c                   iter
021515060404     c                   end
021701060404     C     Kblp          CHAIN     FNblp01L
021702060404     C                   IF        NOT %found(fnblp01l)
021703060404     C                   ITER
021704060404     C                   ENDIF
021713060404     c                   exsr      srdecmtv
021714060405     c     d962TXT       comp      ' '                                8888
022101060404     c* da stampare
023300060404     C     FLGV(I)       IFEQ      'S'
023500950118     C                   MOVEL     DESV(I)       DCVB             20
023600920304     C                   SETOFF                                       52
023700950118     C                   CLEAR                   AR6IFT
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
025300060405     C                   Z-ADD     AR6DFT        G08INV
025400950118     C                   MOVE      *ZEROS        G08DAT
025500950118     C                   MOVEL     '3'           G08ERR
025600950118     C                   CALL      'XSRDA8'
025700950118     C                   PARM                    WLBDA8
025800060404     C                   Z-ADD     G08DAT        GMADFT
027600920304      *
027700921029     C   OF              EXCEPT    TESTA1
027800920304     C                   EXCEPT    DETTA1
027801060404     C   88              EXCEPT    DETTA2
027802060914     c                   exsr      srwtr
027803060406     C   88              EXCEPT    salto
027900991118     C                   MOVEL     'S'           WSTP1             1
028100920304     C                   END
028200920304     C                   END
028401060404     ** Aggiorno il flag di stampato.
028402060404     C     ArbT2LRRN     CHAIN     FiArbT0F
028403060404     C                   IF        %FOUND
028404060404     C                   MOVEL     'S'           ARBFST
028405060404     C                   UPDATE    FiArbT00
028406060404     C                   ENDIF
028600060404     C                   ENDdo
029000020927
029100080117     C*    WSTP1         IFEQ      'S'
029101080117     C*    WSTP          orEQ      'S'
029102080117     C   OF              EXCEPT    TESTA1
029200921029     C                   EXCEPT    FINE1
029300080117     C*                  END
029400921029     C*
029500910521     C                   SETON                                        LR
029501060914      *---------------------------------------------------------------*
029502060914     C     srwtr         BEGSR
029503060914      *---------------------------------------------------------------*
029504060914     c                   eval      wvbaas = ARBAAS
029505060914     c                   eval      wvbLNP = ARBLNP
029506060914     c                   eval      wvbLNA = blpLNA
029507060914     c                   eval      wvbNRS = ARBNRS
029508060914     c                   eval      wvbNSP = ARBNSP
029509060914     c                   eval      wvbDTV = ARBDTV
029510060928     c                   eval      wvborV = ARBorV
029511060928     c                   eval      wvbpru = ARBpru
029512060928     c                   eval      wvbcbop= ARBCBo
029513060928     c                   eval      wvbcbod= ARBCBN
029515060914     C                   MOVEL     dcvb          wvbcvbd
029516060914     c                   eval      WVBMTVD= d962des
029517060914     c                   clear                   wvbnot
029518060914     c   88              eval      WVBNOT  = %trimr(d962txt)
029519060928     c                   eval      wvbgia = 'N'
029528060914     c                   eval      wvbimpp= ARBift
029529060914     c                   eval      wvbdivp= ARBdiv
029530060914     c                   eval      wvbimpd= AR6ift
029531060914     c                   eval      wvbdivd= AR6div
029532060914     c                   clear                   wvbtic
029533060914     c                   clear                   wvbdes
029535060914     c                   write     fnsvbw00
029536060914     c                   endsr
029700911121      *---------------------------------------------------------------*
029800060404     C     srdecmtv      BEGSR
029801060404      *---------------------------------------------------------------*
029802060405     c                   clear                   fnlr96ds2
029803060405     c                   eval      d962aas = ARBAAS
029804060405     c                   eval      d962LNP = ARBLNP
029805060405     c                   eval      d962NRS = ARBNRS
029806060405     c                   eval      d962NSP = ARBNSP
029807060405     c                   eval      d962ORV = ARBORV
029808060405     c                   eval      d962DTV = ARBDTV
029809060405     c                   eval      d962cvb = ARBcvb
029810060405     c                   movel(p)  fnlr96ds2     kpjbu
029811060405     c                   call      'FNLR96R2'
029812060405     c                   parm                    kpjba
029813060405     c                   movel(p)  kpjbu         fnlr96ds2
029814060404     c                   endsr
029815060404      *---------------------------------------------------------------*
029816060404     C     *INZSR        BEGSR
029817060404      *---------------------------------------------------------------*
029900000000     C     *ENTRY        PLIST
030000000000     C                   PARM                    KPJBA
030001060529     c                   movel     kpjbu         param
030100900516      *
030200941129     C                   TIME                    WHHUDT           14 0
030300941129     C                   MOVEL     WHHUDT        ORA               6 0
030400941129     C                   MOVE      WHHUDT        G08DAT
030500941129     C                   MOVE      *ZEROS        G08INV
030600941129     C                   MOVEL     *BLANK        G08ERR
030700941129     C                   CALL      'XSRDA8'
030800941129     C                   PARM                    WLBDA8
030900941129     C                   Z-ADD     G08DAT        GMAUD8            8 0
031000950118     C                   MOVEL     *ALL'-'       TRATT           156
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
037900911119      *- CHIAVE X TABEL00F -------------------------------------------*
038000911106     C     KTAB          KLIST
038100911106     C                   KFLD                    CODUT
038200911106     C                   KFLD                    CODTAB            2
038300911106     C                   KFLD                    KEYTAB            8
038400920304     C     KTAB1         KLIST
038500920304     C                   KFLD                    CODUT
038600920304     C                   KFLD                    CODTAB
038700941129      *
038800060404     C     Kblp          KLIST
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
041700910521     C                   ENDSR
043700020927
050600060404     OPRTF198   E            TESTA1         1 01
050700920304     O                       RSUT                20
050800060529     O                       parmpo              28
050900941129     O                                           57 'ELENCO VARIAZIONI'
051000060404     O                                         +  1 'PRE-PAGATI'
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
052900941129     O          E            TESTA1         1
053000941128     O                                           10 'VARIAZ.'
053100941129     O                                           25 'CD DESCRIZIONE'
053200941129     O                                           38 'LNP'
053300941129     O                                           42 'LNA'
053400941129     O                                           46 'AA'
053500941129     O                                           50 'NR'
053600941129     O                                           58 'SPEDIZ.'
053700060405     O                                           72 'IMP.PRIMA VAR.'
053800060405     O                                           94 'IMPORTO FATT.'
053900991012     O                                          110 'N.FATT.'
054000991012     O                                          118 'DATA'
054001060404     O                                          128 'MOTIVO'
054600941129     O          E            TESTA1         1
054700920304     O                       TRATT             +  0
054800920304      *
054900060405     O          E            DETTA1      1
055000941129     O                       DATA1               10 '  /  /    '
055100941129     O                       ARBCVB              13
055200941129     O                       DCVB                34
055300941129     O                       ARBLNP              38
055400060404     O                       blpLNA              42
055500941129     O                       ARBAAS        Z     47
055600941129     O                       ARBNRS              50
055700941129     O                       ARBNSP        Z     58
055800991012     O                       ARBIFT        2     76
055900991012     O                       ARBDIV              80
056000991012     O                       AR6IFT        2     98
056100991012     O                       AR6DIV             102
056200060405     O                       AR6NFT        Z    110
056300991012     O                       GMADFT             121 '  /  /    '
056301060405     o                       d962des           +  1
056302060404     O          E            DETTA2      1
056303060405     o                       d962TXT           +  0
056304060405     O          E            salto       1
056900921029     O          E            FINE1       1
057000941129     O                                           70 'FINE STAMPA'
057100921029     O                                         +  1 'VARIAZIONI'
057200060404     O                                         +  1 'PRE-PAGATI'
057300911119      *
057400950118**  CMDP
057500060404OVRPRTF FILE(PRTF198) USRDTA(VARIA_PRE) SAVE(*YES)
