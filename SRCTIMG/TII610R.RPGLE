000200110719     H DEBUG DECEDIT('0,') DATEDIT(*ymd/) Option(*nodebugio)
000300110803     ftmmoc03l  uf   e           k disk
000301110803     ftmsmp01l  uf   e           k disk
000406110809     fxx2liv2p  uf   e             disk
000407110809     fprtf198   o    f  198        printer infds(stamp1)
000500030110
000501110719     d kpjba         e ds
000502110804     D soc001        E DS                  EXTNAME(xsoc001ds)
000503110804     D xsocds          DS          1000
000504110719
001100030108     d dataiso         s               d   datfmt(*iso)
001200030108     d dataeur         s               d   datfmt(*eur)
001201110805     d notaerr         s            100
001202110719     D WLBDAT          DS
001203110719     D  G06DAT                 1      6  0
001204110719     D  G06INV                 7     12  0
001205110719     D  G06ERR                13     13
001206110719     D  G06TGI                14     18  0
001900030109     d
001901110803     d stamp1          ds
001902110803     d  lin1                 367    368b 0
002000000607      *--------------------------------------------------------------------
002100000607     c
002200110719      * lettura file ordini da trasmettere per prelievi a logistica
002300110809     c     *start        setll     xx2liv2p
002301110719     c                   do        *hival
002302110803     c                   read      xx2liv2p
002303110803     c                   if        %eof(xx2liv2p)
002304110719     c                   leave
002305110719     c                   endif
002306110803     c                   movel     x2spec        wcomo9            9
002307110803     c                   testn                   wcomo9               30
002308110805     c  n30              eval      notaerr = 'Numero Assoluto ritornato da log-
002309110805     c                             istica non corretto'
002310110803     c  n30              except    errAssol
002311110803     c  n30              iter
002312110803     c                   movel     wcomo9        numriga           9 0
002313110803     c
002314110803      * riga per riga verifica se è stata evasa
002315110803     C     numriga       chain     tmmoc03l
002316110803     c                   if        not %found(tmmoc03l)
002317110805     c                   eval      notaerr = 'Non trovato numero riga corrispo-
002318110805     c                             ndente su file ordini verificare'
002319110803     c                   except    errCorr
002320110803     c                   iter
002321110803     c                   endif
002322110803     c                   select
002323110803      *riga non evasa
002324110803     c                   when      x2qluc = 0
002325110803     c                   exsr      rstsal
002326110804     c                   delete    xx2liv2p
002327110803
002328110803      *riga evasa
002329110803     c                   when      x2qluc = mocqta
002330110803     c                   exsr      aggsal
002331110804     c                   delete    xx2liv2p
002332110803     c                   other
002333110803      *quantità non corrispondenti errore
002334110805     c                   eval      notaerr = 'Quantità ritornata non  corrispo-
002335110805     c                             ndente a quella dell ordine'
002336110803     c                   except    errQta
002337110803     c                   iter
002338110803     c                   endsl
002339110803
002340110803     c                   enddo
002341110803     c
002900020521     c                   seton                                        lr
002901110803     C**----------------------------------------------------
002902110803      * aggiorna saldi
002903110803     C*-----------------------------------------------------
002904110803     C     aggsal        begsr
002905110803
002907110804     C     keysal        CHAIN     tmsmp01l
002908110804     c                   if        %found(tmsmp01l)
002909110804     C     smpqim        sub       mocqta        smpqim
002910110804     C     smpqu2        add       mocqta        smpqu2
002911110804     C     smpqgf        sub       mocqta        smpqgf
002912110804     C                   move      xsccut        SMPUTE                         *
002913110804     C                   move      *date         SMPDMN                         *
002930110804     C                   update    tmsmp000
002931110804     c                   endif
002932110804     C                   move      xsccut        mocUTE                         *
002933110804     C                   move      *date         mocDMN                         *
002934110804     C                   z-add     x2nbl         mocnbl
002935110804     C                   move      *date         mocdbl
002937110803     c                   move      'FA'          mocfas
002938110803     c                   update    tmmoc000
002939110803     C                   ENDSR
002940110803     C***------------------------------------------------***
002941110803      * riga non evasa storno sul saldo
002942110803     C*-----------------------------------------------------
002943110803     C     Rstsal        begsr                                                  *
002950110804     C     KEYSAL        CHAIN     tmsmp01L
002951110804     C                   if        %found(tmsmp01l)
002976110803     C*---                                                 *
002977110809     C                   Z-ADD     mocqta        QTSALD           11 2          *
002978110809     C*    mocQTA        MULT(H)   AMPFCQ        QTSALD                         *
002980110804     C     SMPQPR        sub       QTSALD        SMPQPR                         *
002984110804     c     SMPQIM        ADD       QTSALD        SMPQIM                         *
002993110804     C**--- AGGIORNO SALDI
002994110803     C                   MOVE      ' '           SMPATT                         *
002995110803     C                   move      xsccut        SMPUTE                         *
002996110804     C                   move      *date         SMPDMN                         *
002997110804     C                   UPDATE    tmsmp000                                     *
002998110804     C**--- riporta riga in stato di ordinato
002999110804     C                   move      xsccut        mocUTE                         *
003000110804     C                   move      *date         mocDMN                         *
003001110804     C                   Z-ADD     0             MOCNBL
003002110804     C                   Z-ADD     0             MOCDBL
003003110804     C                   Z-ADD     0             MOCCRG
003004110804     C                   Z-ADD     0             MOCnpc
003005110804     c                   move      'OR'          mocfas
003006110803     c                   update    tmmoc000
003007110804     C                   endif                                                  *
003008110803     CSR   ENDSAL        ENDSR                                                  *
009700020521      *-------------------------------------------------
009800020521     c     *inzsr        begsr
009900020521      *-------------------------------------------------
010000020626     c     *entry        plist
010100110719     c                   parm                    kpjba
010200030414
010201110804     C*---------- RICERCA DITTA :
010202110809     c                   movel     '201'         socpas            3
010203110804     C                   MOVEL     'SOC001'      TIPXSC
010204110804     C                   MOVEL     *blank        SOCXSC
010205110804     C                   EXSR      REPSOC
010206110804     C     RTNXSC        IFNE      '1'
010207110804     C                   MOVEL     XSOCDS        SOC001
010208110804     c                   end
010209110804     C     KEYSAL        KLIST                                                  *
010210110804     C                   KFLD                    ANNo              4 0          *
010211110804     C                   KFLD                    mocmag                         *
010212110804     C                   KFLD                    mocmat                         *
010213110804     c                   move      *year         anno
014600020521     c                   endsr
014800110804     C*----------------------------------------------------*
014900110804     C* Reperimento dati società
015000110804     C*----------------------------------------------------*
015100110804     C     REPSOC        BEGSR
015200110804     C*
015300110809     C                   CALL      'XSOC'
015400110804     C                   PARM                    TIPXSC            6
015500110804     C                   PARM      socpas        SOCXSC            3
015600110804     C                   PARM                    CDSXSC            9 0
015700110804     C                   PARM                    MODXSC            3
015800110804     C                   PARM      *blanks       RTNXSC            1
015900110804     C                   PARM                    XSOCDS
016000110804     C                   PARM                    KPJBA
016100110804     C*
016200110804     C                   ENDSR
016300110804      *-------------------------------------------------
016400110805     Oprtf198   E            TESTA            02
016500110805     O                                           90 'Anomalie riscontrate nel r-
016501110805     o                                              itorno ordini clienti '
016600110805     O                                         +  2 'del'
016700110805     o                       *date         y   +  2
016800110805     o          e            Errassol    2
016900110805     o                       x2art               20
016901110805     o                       x2ncmd            +  1
016902110805     o                       x2spec            +  1
017000110805     o                       notaerr            190
017100110805     o          e            ErrCorr     2
017200110805     o                       x2art               20
017300110805     o                       x2ncmd            +  1
017400110805     o                       x2spec            +  1
017500110805     o                       notaerr            190
017600110805     o          e            ErrQta      2
017700110805     o                       x2art               20
017800110805     o                       x2ncmd            +  1
017900110805     o                       x2spec            +  1
018000110805     o                       notaerr            190
