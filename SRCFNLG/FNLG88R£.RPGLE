000100051109     h DECEDIT('0,') DATEDIT(*DMY.)
000200051005     FFNLG88D   CF   E             WORKSTN
000300051108     F                                     SFILE(LG88S02:NRR)
000400050316     Ftigcp02L  UF   E           K DISK    commit infds(gcpinf)
000500050215     Ftigap01L  IF   E           K DISK
000600051124     Ffnlbl01L  IF   E           K DISK
000700051005     FtiGNP01L  iF   E           K DISK
000800051005     FFNARB01L  IF   E           K DISK
000900060213     FFiAR401L  IF   E           K DISK
001000051005     FFIAR601L  IF   E           K DISK
001100051115     FFIAR901L  IF   E           K DISK
001200900418     FTABEL00F  IF   E           K DISK
001300941003     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001400051005     FFiar501L  IF   E           K DISK
001500941003     D*--------------------------------------------------------------*
001600051006     d dataiso         s               d   datfmt(*iso)
001700051007     d trul82ds      e ds
001800070503     D fnlrn5ds      E DS
001900051007     D FNLR48ds      E DS                  inz
002000051007     D trul90ds      E DS                  inz
002100051007     D DSARBD        E DS                  inz
002200051007     D DSARBG        E DS                  inz
002300051007     D DARBT         E DS                  inz
002400051007     D DSARBK        E DS                  inz
002500051124     D DSfnarb       E DS                  inz extname(fnarb00f)
002600051124     D DSfnarb£      E DS                  inz extname(fnarb00f) prefix(£)
002700051007     D DSgcp         E DS                  extname(tigcp00f)
002800051007     D Ptrp            S               *
002900051007     D                                     INZ(%ADDR(gcplnp))
003000051007     D spepar          S             16
003100051007     D                                     BASED(Ptrp)
003200051007     D Ptra            S               *
003300051007     D                                     INZ(%ADDR(gcplnpa))
003400051007     D spearr          S             16
003500051007     D                                     BASED(Ptra)
003600050316     d gcpinf          ds
003700050316     d  gcpnrr               397    400i 0
003800090520     d* tipi servizio
003900090520     D C5e             S              1    DIM(10)
004000090520     D d5e             S             15    DIM(10)
004100020225     d* MESSAGGI DI ERRORE
004200051007     D ERR             S             70    DIM(8) CTDATA PERRCD(1)
004300900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004400051006     d Wdata8          DS
004500051006     d  dadata                 1      8  0
004600051006     d  adata                  9     16  0
004700051006     d  GioLav                17     21  0
004800941003     D WLBDA8          DS
004900941003     D  G02DAT                 1      8  0
005000941003     D  G02INV                 9     16  0
005100941003     D  G02ERR                17     17
005200941003     D  G02TGI                18     22  0
005300950213     D                 DS
005400950213     D  GGMAX                 38     39  0
005500950213     D  MESSAG                 1     70
005600960123     D FNLG82        E DS                  EXTNAME(FNLG82DS)
005700020729     D OG143         E DS
005800070503     D OG148         E DS
005900900508     D DS3A          E DS
006000950213     D DS2G          E DS
006100051108     D DS7l          E DS
006200060116     D DS3K          E DS
006300040126     D dar5gen       E DS
006400060116     D fnlg08ds      E DS
006500900418     D CNCR80        E DS
006600900418     D KPJBA         E DS
006700060116     D savkpjbu        S                   like(kpjbu)
006800081114     D PARAM1          DS
006900081114     D  PA1AAS                14     17  0
007000081114     D  PA1LNP                18     20  0
007100081114     D  PA1NRS                21     22  0
007200081114     D  PA1NSP                23     29  0
007300081114     D  PA1F03                30     30
007400081114     D  PA1FLG                31     31
007500941003     I*--------------------------------------------------------------*
007600941003     IAZORG01L  AA
007700941003     I                             P    4    5 0ORGFIL
007800941003     I                                  6    6  ORGFVA
007900971112     I                                 11   11  ORGFL1
008000941003     I                                 14   33  ORGDES
008100020729     I                               3776 3800  ORGDE3
008200070503     I                               3901 3925  ORGDE8
008300941003     C*--------------------------------------------------------------*
008400941003     C                   SETOFF                                       509028
008500900418     C                   EXSR      PULVID
008600900619     C*
008700050316     C     KGIA          CHAIN(e)  tigcp02L                           40
008800050316     c                   if        %error
008900070521     C     KGIA          CHAIN(n)  tigcp02L                           40
009000050316     c                   exsr      geslck
009100050318     C     KGIA          CHAIN(e)  tigcp02L                           40
009200050316     c                   endif
009300900420     C* SE  NON E ANNULLATA
009400900419     C* CONTROLLO SE SI PUO MODIFICARE O DECODIFICO STATO
009500900418     C     *IN40         IFEQ      '0'
009600051005     c*
009700050318     C     GCPFAS        IFlt      20
009800050318     C     GCPFAS        orgt      31
009900051005     C                   SETON                                        5090
010000051006     C                   eval      VMSG ='NON MANUTENZIONABILE !!'
010100911014     C                   END
010200900420     C*
010300050222     C     gcpriap       COMP      'R'                                    42
010400911014     C*
010500050215     C     Karb          CHAIN     FNARB01L                           43
010600941003     C     ARBATB        IFEQ      '*'
010700941003     C     *IN43         OREQ      '1'
010800941003     C                   SETON                                        285090
010900941003     C                   MOVEL     ERR(1)        $MSG
011000911014     C                   ELSE
011100941104     C     ARBDCM        IFNE      0
011200941003     C                   SETON                                        285090
011300941003     C                   MOVEL     ERR(2)        $MSG
011400941003     C                   ELSE
011500911014     C                   EXSR      ARBVID
011600911014     C                   END
011700941003     C                   END
011800911014     C                   END
011900051005     c*
012000051005     C   50              unlock    tigcp02l
012100900418     C* SE GIACENZA GIA CARICATA CARICO I DATI DELLA GIAC. A VIDEO
012200900418     C     *IN40         IFEQ      '0'
012300900418     C                   EXSR      GCPVID
012400900418     C                   END
012500900418     C*
012600051005     C                   do        *hival
012700040126      * rekord GEN riferimenti ubicazione in magazzino giacenze
012800040126     C                   clear                   dar5gen
012900040126     C                   move      'GEN'         trcar5
013000040126     c     kar5          chain     fiar501l
013100040126     c                   if        %found
013200040126     C                   MOVEL     ar5uni        dar5gen
013300040126     c                   endif
013400040126     c                   movel     §ar5zng       vubima
013500040126
013600051005     C                   EXFMT     LG88D01
013700051006     C* f12= guida
013800051006     C   kl              leave
013900911108     C* SE NON MANUTENZIONABILE ERRORE
014000051005     C   50              iter
014100081114     C* f07= Int bolle
014200081114     C                   if        *inkg
014300081114     c                   movel     kpjbu         savkpjbu
014400081114     c                   z-add     arbaas        pa1aas
014500081114     c                   z-add     arblnp        pa1lnp
014600081114     c                   z-add     arbnrs        pa1nrs
014700081114     c                   z-add     arbnsp        pa1nsp
014800081114     c                   movel     ' '           pa1f03
014900081114     c                   movel     '1'           pa1flg
015000081114     c                   movel     param1        kpjbu
015100081114     c                   call      'FNLR36R'
015200081114     c                   parm                    kpjba
015300081114     c                   movel     kpjbu         param1
015400081114     c                   movel     savkpjbu      kpjbu
015500081114     C                   END
015600081114      *
015700051108     C* f14= Note
015800051108     C   kn              do        *hival
015900051108     c                   if        oknote <> *blank
016000051108     c                   z-add     1             nr2key
016100051108     C                   WRITE     lg88z02
016200051108     C                   EXFMT     lg88c02
016300051108     C   kl              leave
016400051108     c                   else
016500051108     C                   leave
016600051108     c                   endif
016700051108     c                   enddo
016800070503     C* f17= NoteLDV
016900070503if  1c                   if        *inKR = *on
017000070503     c                   clear                   fnlrn5ds
017100070503     C                   movel     kpjbu         WSVKPJ          256
017200070503     c                   eval      in5mod='I'
017300070503     c                   eval      in5aas=vaas
017400070503     c                   eval      in5lnp=vlnp
017500070503     c                   eval      in5nrs=vnrs
017600070503     c                   eval      in5nsp=vnsp
017700070503     C                   movel     'FNLG88R'     in5pgm
017800070503     c                   call      'FNLRN5R'
017900070503     c                   parm                    kpjba
018000070503     c                   parm                    fnlrn5ds
018100070503     C                   movel     WSVKPJ        kpjbu
018200070503e   1c                   endif
018300051005     c* controllo video
018400941007     C                   EXSR      CTRVID
018500051005     c* errore
018600051005     C   19              iter
018700051007     c* conferma
018800051007     c                   if        *inkf
018900051007     c* variazione bolla x data consegna richiesta
019000051007     c                   exsr      varbol
019100051007     C   28              iter
019200051007     C                   EXSR      GCPWTR
019300051007     c                   leave
019400051007     c                   end
019500051005     c                   enddo
019600960123     C*
019700900418     C*
019800051005     c     fine          tag
019900900418     C                   SETON                                        LR
020000900418     C*---------------------------------------------------------------*
020100941003     C*   OPERAZIONI INIZIALI                                         *
020200900418     C*---------------------------------------------------------------*
020300941003     C     *INZSR        BEGSR
020400941003     C*
020500900418     C     *ENTRY        PLIST
020600900418     C                   PARM                    KPJBA
020700941003     C                   MOVEL     KPJBU         FNLG82
020800050316     c                   move      d82comm       gescomm           1
020900941003     C                   MOVE      D82AGC        VAGC
021000941003     C                   MOVE      D82FGC        VFGC
021100941003     C                   MOVE      D82NGC        VNGC
021200941003     C*
021300900418     C                   Z-ADD     1             CODUT
021400900418     C                   CALL      'X§PARUT'
021500900418     C                   PARM                    UTEDSE
021600900418     C     CODUT         CABEQ     -1            FINE
021700900418     C                   MOVEL     RAGUT         RSUT             20
021800900418     C                   MOVEL     RAGUT         RSUT2            25
021900051005     c                   eval      vprogr = 'FNLG88R'
022000900524     C*-                                                   *
022100900524     C                   MOVEL     REC80         CNCR80
022200900418     C*---------------------------------------------------------------*
022300900418     C*  KLIST
022400900418     C     KGIA          KLIST
022500941005     C                   KFLD                    VAGC              4 0
022600900420     C                   KFLD                    VFGC
022700900420     C                   KFLD                    VNGC
022800911014     C                   KFLD                    §FRG
022900900418     C*
023000911017     C     KNOT          KLIST
023100911017     C                   KFLD                    VAGC
023200911017     C                   KFLD                    VFGC
023300911017     C                   KFLD                    VNGC
023400911017     C                   KFLD                    §FRG
023500911017     C                   KFLD                    §FAS
023600050215     C*
023700050215     C     Karb          KLIST
023800050215     C                   KFLD                    GCPAASa
023900050215     C                   KFLD                    GCPLNPa
024000050215     C                   KFLD                    GCPNRSa
024100050215     C                   KFLD                    GCPNSPa
024200051124     C     Karb48        KLIST
024300051125     C                   KFLD                    LBLAAP
024400051125     C                   KFLD                    LBLLPP
024500051125     C                   KFLD                    LBLNRP
024600051125     C                   KFLD                    LBLNSP
024700050215     C     KAR6a         KLIST
024800050215     C                   KFLD                    GCPAASa
024900050215     C                   KFLD                    GCPLNPa
025000050215     C                   KFLD                    GCPNRSa
025100050215     C                   KFLD                    GCPNSPa
025200050215     C                   KFLD                    KTRC
025300051005     C*
025400051005     C     KBOL          KLIST
025500051005     C                   KFLD                    GCPAAS
025600051005     C                   KFLD                    GCPLNP
025700051005     C                   KFLD                    GCPNRS
025800051005     C                   KFLD                    GCPNSP
025900051005     C*
026000051005     C     KBOL4a        KLIST
026100051005     C                   KFLD                    GCPAASa
026200051005     C                   KFLD                    GCPLNPa
026300051005     C                   KFLD                    GCPNRSa
026400051005     C                   KFLD                    GCPNSPa
026500051005     C                   KFLD                    ar4TRC
026600900420     C*
026700900418     C     KTAB          KLIST
026800900418     C                   KFLD                    CODUT
026900900418     C                   KFLD                    TBLCOD
027000900418     C                   KFLD                    TBLKEY
027100040126     C*
027200090520     C     *LIKE         DEFINE    TBLKUT        KKUT
027300090520     C     *LIKE         DEFINE    TBLCOD        KCOD
027400090520     C*
027500090520     C     KTBL2         KLIST
027600090520     C                   KFLD                    KKUT
027700090520     C                   KFLD                    KCOD
027800090520     C                   Z-ADD     1             KKUT
027900040126     C     Kar5          KLIST
028000040126     C                   KFLD                    GCPAAS
028100040126     C                   KFLD                    GCPLNP
028200040126     C                   KFLD                    GCPNRS
028300040126     C                   KFLD                    GCPNSP
028400040126     C                   KFLD                    trcar5
028500900418     C*
028600040126     C     *LIKE         DEFINE    ar5trd        trcar5
028700900420     C     *LIKE         DEFINE    GCPDDM        AMGDDM
028800900427     C     *LIKE         DEFINE    GCPDDM        AMGDTG
028900900427     C     *LIKE         DEFINE    GCPDDM        AMGUDT
029000990913     C     *LIKE         DEFINE    GCPFRG        §FRG
029100911018     C     *LIKE         DEFINE    GCPFAS        §FAS
029200990929     C     *LIKE         DEFINE    AR6TRC        KTRC
029300941010     C*
029400911014     C*
029500911014     C                   MOVE      *ZEROS        §FRG
029600911017     C                   MOVE      20            §FAS
029700900427     C*
029800941005     C                   TIME                    WHHDAT           14 0
029900941003     C                   MOVE      WHHDAT        G02DAT
030000941003     C                   Z-ADD     0             G02INV
030100941003     C                   MOVE      ' '           G02ERR
030200941003     C                   CALL      'XSRDA8'
030300941003     C                   PARM                    WLBDA8
030400941003     C                   MOVE      G02INV        AMGUDT
030500911014     C*
030600950213     C* Carico tabella 2G
030700950213     C                   MOVEL     '2G'          TBLCOD
030800950213     C                   MOVEL     *BLANKS       TBLKEY
030900950213     C                   MOVEL     '1'           TBLKEY
031000950213     C                   CLEAR                   DS2G
031100950213     C     KTAB          CHAIN     TABEL                              31
031200950213     C     *IN31         IFEQ      '0'
031300950213     C     TBLFLG        ANDEQ     ' '
031400950213     C                   MOVEL     TBLUNI        DS2G
031500950213     C                   END
031600941003     C*
031700090520     C* Carico tabella 5e
031800090520     C                   Z-ADD     0             X                 3 0
031900090520     C                   MOVEL     '5E'          KCOD
032000090520     C     KTBL2         CHAIN     TABEL                              31
032100090520     C     *IN31         DOWEQ     '0'
032200090520     C     X             ANDLT     10
032300090520     C     TBLFLG        IFEQ      ' '
032400090520     C                   ADD       1             X
032500090520     C                   MOVEL     TBLKEY        c5e(X)
032600090520     C                   MOVEL     TBLUNI        d5e(X)
032700090520     C                   END
032800090520     C     KTBL2         READE     TABEL                                  31
032900090520     C                   END
033000900418     C                   ENDSR
033100900418     C*---------------------------------------------------------------*
033200900418     C*   P U L V I D                                                 *
033300900418     C*---------------------------------------------------------------*
033400900418     C     PULVID        BEGSR
033500900419     C                   MOVEL     *ZEROS        VNRS
033600900419     C                   MOVEL     *ZEROS        VNSP
033700911023     C                   MOVEL     *BLANK        VMSG
033800900418     C                   MOVEL     *ZEROS        VDSP
033900900418     C                   MOVEL     *BLANK        VTBL
034000900418     C                   MOVEL     *BLANK        DTBL
034100900508     C                   MOVEL     *ZEROS        VLNP
034200900418     C                   MOVEL     *ZEROS        VLNA
034300900418     C                   MOVEL     *BLANK        DLNA
034400900418     C                   MOVEL     *BLANK        VNAS
034500900418     C                   Z-ADD     *ZEROS        VIFT
034600991119     C                   MOVEL     *BLANKS       V1CDFA
034700900418     C                   Z-ADD     *ZEROS        VNCL
034800900418     C                   Z-ADD     *ZEROS        VPKG
034900900418     C                   Z-ADD     *ZEROS        VASS
035000911017     C                   Z-ADD     *ZEROS        VVOL
035100900418     C                   MOVEL     *BLANK        VRSD
035200900418     C                   MOVEL     *BLANK        VIND
035300900418     C                   MOVEL     *BLANK        VLOD
035400900418     C                   MOVEL     *BLANK        VPRD
035500941003     C                   MOVEL     *BLANKS       VCAD
035600900419     C                   MOVEL     *ZEROS        VSCM
035700900418     C                   MOVEL     *BLANK        VRSM
035800900418     C                   MOVEL     *BLANK        VINM
035900900418     C                   MOVEL     *BLANK        VLOM
036000900418     C                   MOVEL     *BLANK        VPRM
036100941003     C                   MOVEL     *BLANKS       VCAM
036200900418     C                   MOVEL     *ZEROS        VDTG
036300900419     C                   MOVEL     *ZEROS        VDIG
036400900419     C                   MOVEL     *BLANKS       DFGC
036500911017     C                   MOVEL     *ZEROS        VCMC
036600911017     C                   MOVEL     *BLANKS       DCMC
036700911017     C                   MOVEL     *BLANKS       VDMC
036800941003     C                   MOVEL     *BLANKS       V1CDIV
036900941003     C                   MOVEL     *BLANKS       V1CRD2
037000941005     C                   MOVEL     *BLANKS       V1CNZD
037100941005     C                   MOVEL     *BLANKS       V1CNZM
037200941003     C                   MOVEL     *BLANKS       V1CRMA
037300941003     C                   MOVEL     *ZEROS        V1CDED
037400051005     C                   MOVEL     *BLANKS       V1Cno1
037500051005     C                   MOVEL     *BLANKS       V1Cno2
037600051005     C                   MOVEL     *BLANKS       V1Cno3
037700900418     C                   ENDSR
037800911014     C*---------------------------------------------------------------*
037900911014     C*   A R B V I D                                                 *
038000911014     C*---------------------------------------------------------------*
038100911014     C     ARBVID        BEGSR
038200941003     C                   MOVE      ARBMGS        G02INV
038300941003     C                   MOVEL     ARBAAS        G02INV
038400941003     C                   Z-ADD     0             G02DAT
038500941003     C                   MOVEL     '3'           G02ERR
038600941003     C                   CALL      'XSRDA8'
038700941003     C                   PARM                    WLBDA8
038800941003     C                   MOVEL     G02DAT        VDSP
038900941003     C* TIPO SPEDIZIONE
039000941003     C                   MOVEL     *BLANKS       DTSP
039100090520     C                   Z-ADD     1             X
039200090520     C     arbtsp        LOOKUP    C5e(X)                                 32
039300090520     C     *IN32         IFEQ      '1'
039400090520     C                   MOVEl     D5e(X)        Dtsp
039500090520     C                   END
039600911014     C* TIPO BOLLA
039700941003     C                   MOVEL     ARBCBO        VTBL
039800911014     C                   MOVEL     '3A'          TBLCOD
039900911014     C                   MOVEL     *BLANK        TBLKEY
040000911014     C                   MOVEL     VTBL          TBLKEY
040100911014     C     KTAB          CHAIN     TABEL                              21
040200911014     C   21              MOVEL     *ALL'?'       DTBL
040300911014     C  N21              MOVEL     TBLUNI        DS3A
040400020225     c* porto della bolla
040500960208     C  N21              MOVEL     §3ATB1        EXPORT            1
040600911014     C  N21              MOVEL     §3ADES        DTBL
040700911014     C* LINEA ARRIVO
040800911014     C                   MOVEL     ARBLNA        VLNA
040900911014     C     VLNA          CHAIN     AZORG01L                           21
041000911014     C  N21ORGFVA        COMP      '*'                                    21
041100911014     C   21              MOVEL     *ALL'?'       DLNA
041200911014     C  N21              MOVEL     ORGDES        DLNA
041300911014     C*
041400911014     C                   MOVEL     ARBNAS        VNAS
041500911014     C                   Z-ADD     ARBNCL        VNCL
041600941003     C                   Z-ADD     ARBVLF        VVOL
041700941005     C                   Z-ADD     ARBPKF        VPKG
041800941003     C* Reperisco importi fatture e C/Assegno
041900990929     C     EXPORT        IFEQ      'A'
042000990929     C                   MOVEL     '1'           KTRC
042100990929     C                   CLEAR                   AR6IFT
042200050215     C     KAR6a         CHAIN     FIAR601L                           31
042300990929     C                   Z-ADD     AR6IFT        VIFT
042400991119     C                   MOVEL     AR6DIV        V1CDFA
042500990929     C                   END
042600051115     C     Karb          CHAIN     FIAR901L                           31
042700941003     C                   Z-ADD     AR9CAS        VASS
042800941003     C                   MOVEL     AR9VCA        V1CDIV
042900911014     C* DESTINATARIO
043000911014     C                   MOVEL     ARBRSD        VRSD
043100941003     C* Controllo se esiste estensione descrizione destinatario
043200051005     C                   MOVEL     'D'           ar4TRC
043300941003     C                   MOVEL     *BLANKS       AR4NOT
043400060213     C     KBOL4a        CHAIN     FiAR401L                           31
043500941003     C                   MOVEL     AR4NOT        V1CRD2
043600911014     C                   MOVEL     ARBIND        VIND
043700911014     C                   MOVEL     ARBLOD        VLOD
043800911014     C                   MOVEL     ARBPRD        VPRD
043900911014     C                   MOVEL     ARBCAD        VCAD
044000941003     C                   MOVEL     ARBNZD        V1CNZD
044100911014     C* MITTENTE
044200911014     C     ARBCCM        IFNE      *ZEROS
044300911014     C                   MOVEL     ARBCCM        VSCM
044400911014     C                   ELSE
044500911014     C                   MOVEL     ARBKSC        VSCM
044600911014     C                   END
044700911014     C                   MOVEL     ARBRSM        VRSM
044800911014     C                   MOVEL     ARBINM        VINM
044900911014     C                   MOVEL     ARBLOM        VLOM
045000911014     C                   MOVEL     ARBPRM        VPRM
045100911014     C                   MOVEL     ARBCAM        VCAM
045200941003     C                   MOVEL     ARBNZM        V1CNZM
045300941003     C                   MOVEL     ARBRMN        VRIF
045400941003     C*
045500911014     C                   ENDSR
045600900418     C*---------------------------------------------------------------*
045700900418     C*   G C P V I D                                                 *
045800900418     C*---------------------------------------------------------------*
045900900418     C     GCPVID        BEGSR
046000050216     C                   MOVE      GCPLNPa       VLNP
046100050216     C                   MOVE      GCPAASa       VAAS
046200050216     C                   MOVE      GCPNRSa       VNRS
046300050216     C                   MOVE      GCPNSPa       VNSP
046400050216
046500900508     C                   MOVE      GCPSCM        VSCM
046600900504     C                   MOVE      VSCM          §QUATR            4
046700900504     C     §QUATR        IFEQ      *ALL'8'
046800900504     C     §QUATR        OREQ      *ALL'9'
046900900418     C* MITTENTE DA APPOGGIO
047000050216     C     KBOL          CHAIN     tiGAP01L                           21
047100900418     C     *IN21         IFEQ      '0'
047200900418     C                   MOVEL     GAPRSM        VRSM
047300900418     C                   MOVEL     GAPINM        VINM
047400900418     C                   MOVEL     GAPLOM        VLOM
047500900418     C                   MOVEL     GAPPRM        VPRM
047600900418     C                   MOVEL     GAPCAM        VCAM
047700941003     C                   MOVEL     GAPNZM        V1CNZM
047800900418     C                   END
047900051005     C                   END
048000900418     C* CODICE MOTIVO GIACENZA
048100900418     C                   MOVE      GCPCMC        VCMC
048200900418     C                   MOVEL     '2A'          TBLCOD
048300900418     C                   MOVEL     *BLANK        TBLKEY
048400900418     C                   MOVEL     VCMC          TBLKEY
048500900418     C     KTAB          CHAIN     TABEL                              21
048600900418     C   21              MOVEL     *ALL'?'       DCMC
048700900418     C  N21              MOVEL     TBLUNI        DCMC
048800911219     C* ULTERIORE MOTIVO
048900911219     C                   Z-ADD     10            §FAS
049000050215     C     KNOT          CHAIN     tignp01L                           21
049100911219     C  N21              MOVEL     GNPDMC        VDMC
049200911219     C   21              MOVEL     *BLANKS       VDMC
049300911219     C                   Z-ADD     20            §FAS
049400900419     C* DATA APERTURA GIACENZA
049500941005     C                   MOVE      GCPMGC        G02INV
049600941003     C                   MOVEL     GCPAAS        G02INV
049700941003     C                   Z-ADD     0             G02DAT
049800941003     C                   MOVEL     '3'           G02ERR
049900941003     C                   CALL      'XSRDA8'
050000941003     C                   PARM                    WLBDA8
050100941010     C                   MOVE      G02INV        AMGDTG
050200941010     C                   MOVEL     G02DAT        VDTG
050300900419     C                   MOVE      GCPNGC        VNGC
050400900419     C* FIL. CHE HA APERTO LA GIACENZA
050500900419     C     GCPFGC        IFNE      *ZEROS
050600900419     C                   MOVE      GCPFGC        VFGC
050700900419     C     VFGC          CHAIN     AZORG01L                           21
050800900419     C  N21ORGFVA        COMP      '*'                                    21
050900900419     C   21              MOVEL     *ALL'?'       DFGC
051000971112     C   21              MOVEL     *BLANKS       GIAEST
051100900419     C  N21              MOVEL     ORGDES        DFGC
051200020729     C                   clear                   OG143
051300020729     C  n21              MOVEL     ORGDE3        OG143
051400070503     C                   clear                   OG148
051500070503     C  n21              MOVEL     ORGDE8        OG148
051600070503     c     §oggeot       comp      'S'                                    91
051700020729     C                   MOVEL     *blank        GIAEST            1
051800020729     C  N21§OgNTW        ifeq      'FED'
051900020729     C     §OgNTW        oreq      'EEX'
052000020729     C     §OgNTW        oreq      'EUP'
052100020729     C                   MOVEL     'E'           GIAEST
052200020729     c                   end
052300900419     C                   END
052400900419     C*
052500900419     C* DATA ULTIMA RIAPERTURA
052600050222     c                   if        gcpriap = 'R'
052700941003     C                   Z-ADD     GCPDUR        G02INV
052800941003     C                   Z-ADD     0             G02DAT
052900941003     C                   MOVEL     '3'           G02ERR
053000941003     C                   CALL      'XSRDA8'
053100941003     C                   PARM                    WLBDA8
053200941003     C                   MOVEL     G02DAT        VDIG
053300950530     C                   MOVE      G02INV        AMGDUR            8 0
053400050222     c                   else
053500050222     C                   z-add     0             VDIG
053600050222     c                   end
053700051005     C* DATA DISPOSIZIONI
053800051005     C* -----------------
053900051006     C                   z-add     gcpddm        G02inv
054000051006     C                   Z-ADD     0             G02DAT
054100051006     C                   MOVEL     '3'           G02ERR
054200051005     C                   CALL      'XSRDA8'
054300051005     C                   PARM                    WLBDA8
054400051006     C                   MOVEL     G02DAT        VDdm
054500051006     C                   MOVE      G02inv        AMGDDM
054600051005     C                   Z-ADD     G02TGI        WGGDDM            5 0
054700911109     C*
054800051005     C     KNOT          SETLL     tignp01L
054900051005     c                   do        *hival
055000051005     C     KNOT          reade     tignp01L
055100051005     c                   if        %eof(tignp01l)
055200051005     c                   leave
055300051005     c                   end
055400051005     c                   if        gnpdmc = 'RICONSEGNARE STESSO INDIRIZZO-
055500051006     C                              CON APPUNTAMENTO'
055600051005     c                   eval      v1cno1 = gnpdmc
055700051005     c                   z-add     1             conta             1 0
055800051005     c                   iter
055900051005     c                   end
056000051005     c                   if        conta = 1
056100051005     c                   eval      v1cno2 = gnpdmc
056200051005     c                   z-add     2             conta
056300051005     c                   end
056400051005     c                   if        conta = 2
056500051005     c                   eval      v1cno3 = gnpdmc
056600051005     c                   end
056700051005     c                   enddo
056800051108     c                   if        conta = 0
056900051108     c                   eval      v1cno1 = 'VEDI NOTE !!!!'
057000051108     c                   exsr      caricanote
057100051108     c                   endif
057200051005     c*
057300900418     C                   ENDSR
057400900418     C*---------------------------------------------------------------*
057500900418     C*   C T R V I D                                                 *
057600900418     C*---------------------------------------------------------------*
057700900418     C     CTRVID        BEGSR
057800051006     C                   SETOFF                                       195228
057900051006     c                   clear                   $msg
058000051005     c                   do
058100050415     C* DATA  ESEguibilita DISPOSIZIONI
058200941003     C* ------------------------------
058300950228     C                   Z-ADD     0             AMGDED
058400941006     C     V1CDED        IFNE      0
058500941003     C                   MOVE      V1CDED        G02DAT
058600941003     C                   MOVEL     ' '           G02ERR
058700941003     C                   CALL      'XSRDA8'
058800941003     C                   PARM                    WLBDA8
058900941003     C     G02ERR        IFEQ      '1'
059000941003     C                   MOVEL     ERR(3)        $MSG
059100941003     C                   SETON                                        521928
059200051005     C                   leave
059300941003     C                   END
059400941005     C                   MOVE      G02INV        AMGDED            8 0
059500941006     C                   MOVE      G02DAT        V1CDED
059600950213     C                   Z-ADD     G02TGI        WGGDED            5 0
059700941003     C     AMGDED        IFLT      AMGDTG
059800941003     C                   SETON                                        521928
059900051006     C                   MOVEL     ERR(4)        $MSG
060000051005     C                   leave
060100941003     C                   END
060200941003     C     AMGDED        IFLT      AMGDDM
060300051006     C                   MOVEL     ERR(5)        $MSG
060400051006     C                   SETON                                        522819
060500051005     C                   leave
060600941003     C                   END
060700950213     C     WGGDED        SUB       WGGDDM        WGGDIF            5 0
060800950213     C     WGGDIF        IFGT      §2GESE
060900051006     C                   MOVEL     ERR(6)        MESSAG
061000950213     C                   Z-ADD     §2GESE        GGMAX
061100950213     C                   MOVEL     MESSAG        $MSG
061200950213     C                   SETON                                        521928
061300051005     C                   leave
061400950213     C                   END
061500051006     C     AMGDed        IFlT      AMGUDT
061600051006     C                   SETON                                        521928
061700051006     C                   MOVEL     ERR(7)        $MSG
061800051006     C                   leave
061900051006     C                   END
062000051005     c                   else
062100051006     C                   MOVEL     ERR(3)        $msg
062200051005     C                   SETON                                        521928
062300941006     C                   END
062400051005     C                   ENDdo
062500960206     C*
062600051005     C                   ENDSR
062700900418     C*---------------------------------------------------------------*
062800900418     C*   G C P W T R                                                 *
062900900418     C*---------------------------------------------------------------*
063000900418     C     GCPWTR        BEGSR
063100051007     C* tolgo un gg lavorativo alla data eseguibilità solo se consegna IL
063200051007     c                   if        v1ctcr = ' '
063300051006     c                   move      amgded        dadata
063400051006     c                   move      amgded        adata
063500051006     c                   CALL      'XSRLAV8'
063600051006     c                   PARM                    Wdata8
063700051006     c                   z-add     giolav        giolavs           1 0
063800051006     c                   move      amgded        dataiso
063900051006    3c                   do        *hival
064000051006     c     dataiso       subdur    1:*d          dataiso
064100051006     c                   move      dataiso       dadata
064200051006     c                   move      amgded        adata
064300051006     c                   CALL      'XSRLAV8'
064400051006     c                   PARM                    Wdata8
064500051006    4c                   if        giolav = 0 or giolav > giolavs
064600051006     c                   leave
064700051006    4c                   endif
064800051006    3c                   enddo
064900051006     c                   move      dataiso       gcpded
065000051007     c                   else
065100051007     c                   move      amgded        gcpded
065200051007     c                   end
065300050215     C  N40              UPDATE    tigcp000
065400050316     c                   if        gescomm = 'S'
065500050316     c                   commit
065600050316     c                   endif
065700060116      * verifico se inviare le info al cliente relative all'appuntamento
065800060116     c                   exsr      gesVAGapp
065900900418     C                   ENDSR
066000060116     C*----------------------------------------------------*
066100060116     C     gesVAGapp     begsr
066200060116     C*----------------------------------------------------*
066300060116     C* VERIFICO SE PER IL CLIENTE MITTENTE E' PREVISTO LO SCARICO
066400060116     c
066500060116     C                   movel     '3K'          tblcod
066600060116     C                   movel     *blanks       tblkey
066700060116     C                   movel     gcpscm        tblkey
066800060116     C     ktab          chain     tabel                              67
066900060116     C  n67              do
067000060116     C                   movel     tbluni        ds3k
067100060904     C                   if        §3kapp = 'S'
067200060116     c                   clear                   fnlg08ds
067300060116     c                   eval      D08AGC= gcpagc
067400060116     c                   eval      D08FGC= gcpfgc
067500060116     c                   eval      D08NGC= gcpngc
067600060116     c                   eval      D08uni= §3kgks
067700060116     c                   eval      D08GSD= §3kGSD
067800060116     c                   eval      D08est= giaest
067900060116     c                   eval      D08pgm = 'FNLG88R'
068000060116     c                   eval      D08commit= 'S'
068100060116     c                   eval      D08tpr= 'A'
068200060116     c                   eval      D08app= 'S'
068300060116     c                   movel     kpjbu         savkpjbu
068400060116     c                   movel(p)  fnlg08ds      kpjbu
068500060116     c                   call      'FNLG08R'                            29
068600060116     c                   parm                    kpjba
068700060116     c                   movel     kpjbu         fnlg08ds
068800060116     c                   movel     savkpjbu      kpjbu
068900060116     C*
069000060116     C                   END
069100060116     C                   ENDdo
069200060116     C                   endsr
069300050316     C*----------------------------------------------------*
069400050316     C     geslck        begsr
069500050316     C*----------------------------------------------------*
069600070521     c                   clear                   trul82ds
069700050316     c                   Eval      ul82§rrn = gcpnrr
069800050316     c                   Eval      UL82§FIL = 'TIGCP00F'
069900050316     c                   Eval      UL82§WIN = 'S'
070000050316     c                   Eval      UL82§F7  = 'S'
070100050316     c                   Eval      UL82§num = 3
070200050316     c                   Eval      UL82§att = 3
070300050404     c                   Eval      UL82§mss = 'Si sta bloccando la giacenza Nr.'
070400050404     c                             + %editc(vagc:'Z') +  '  '+ %editc(vfgc:'Z')
070500050404     c                             + '  ' +  %editc(vngc:'Z') + ' Terminare il -
070600050404     c                             lavoro'
070700050404     c                   Eval      UL82§msw = 'La giacenza Nr. '
070800050404     c                             + %editc(vagc:'Z') +  '  '+ %editc(vfgc:'Z')
070900050404     c                             + '  ' +  %editc(vngc:'Z') +
071000050404     c                             ' non è manutenzionabile'
071100050404
071200050316      * Effettuo la chiamata al *PGM d utilità
071300050316     c                   Call(e)   'TRUL82R'
071400050316     c                   Parm                    Trul82ds
071500050316     c                   if        ul82§sts = 'A'
071600050316     c                   goto      fine
071700050316     c                   endif
071800050316
071900050316     C                   ENDSR
072000051007     C*----------------------------------------------------*
072100051007     C*  GESTIONE RICHIAMO PGM VARIAZIONE BOLLE
072200051007     C*----------------------------------------------------*
072300051007     C     VARBOL        BEGSR
072400051007     C*
072500051124     c* mi salvo arb figlia nel caso mi dovessi leggere la precedente
072600051124     c                   eval      dsfnarb£ = dsfnarb
072700051007     C* Carico la variazione sulla bolla a video tranne nei casi
072800051007     C* di bolla figlia non Assegnato
072900051007     C                   CLEAR                   FNLR48ds
073000051007     C                   CLEAR                   DSARBD
073100051007     C                   CLEAR                   DSARBG
073200051007     C                   Z-ADD     gcpLNPa       D48LNP
073300051007     C                   Z-ADD     gcpAASa       D48AAS
073400051007     C                   Z-ADD     gcpNRSa       D48NRS
073500051007     C                   Z-ADD     gcpNSPa       D48NSP
073600051125     c* se bolla locale metto la data cons. richiesta sulla mamma
073700051124     c     karb          chain     fnlbl01l
073800051124     C                   If        %found(fnlbl01l) and lbllan = lbllap
073900051124     c     karb48        chain     fnarb01l
074000051125     C                   If        %found(fnarb01l) and arbcca <> '2' and
074100051125     C                             arbcca <> '6'
074200051125     C                   Z-ADD     LBLAAP        D48AAS
074300051125     C                   Z-ADD     LBLLPP        D48LNP
074400051125     C                   Z-ADD     LBLNRP        D48NRS
074500051125     C                   Z-ADD     LBLNSP        D48NSP
074600051128     c                   end
074700051128     c                   eval      dsfnarb = dsfnarb£
074800051125     c                   end
074900051007     C*
075000051007     C                   MOVEL     'E'           D48FFR
075100051007     C                   MOVEL     arbcbo        D48CBO
075200051007     C                   MOVEL     'G'           D48TRC
075300051007     C                   MOVEL     'A'           D48TBO
075400051007      * variata data consegna richiesta
075500051007      *  in questo caso viene forzata la causale 'CD'
075600051007     c                   z-add     amgded        §BGDCR
075700051007     c                   move      v1ctcr        §BGTCR
075800051109     c                   movel     arbGC1        §BGGC1
075900051109     c                   movel     arbGC2        §BGGC2
076000051109     C                   MOVEL     arbtc1        §BGTC1
076100051109     C                   MOVEL     arbtc2        §BGTC2
076200051108      * se consegna particolare supermercato imposta causale CS
076300051109     c                   if        arbtc1 = 'S' or arbtc2 = 'S'
076400051109     c                   Clear                   Ds7l
076500051109     c                   Movel     '7L'          tblcod
076600051109     c                   Movel(p)  'CS'          tblKey
076700051109     c     kTab          Chain     Tabel00f                           96
076800051109     c                   If        Not *In96 and TblFlg <> ' '
076900051109     c                   Eval      *In96 = *On
077000051109     c                   EndIf
077100051109     c  n96              Movel     TblUni        Ds7l
077200051109     c                   If        Not *In96 and §7LSup <> 'N'
077300051109     c                   Movel     'CS'          D48Cvb
077400051109     c                   Movel     'V'           D48Ffr
077500051109     c                   Movel     'S'           D48F12
077600051109     c                   EndIf
077700051109      * no cons.partic.'S'
077800051108     c                   else
077900051007     C                   MOVEL     'CD'          D48CVB
078000051108     c                   endif
078100051007
078200051007     C                   MOVEL     FNLR48ds      KPJBU
078300051007     C                   CALL      'FNLR48R'
078400051007     C                   PARM                    KPJBA
078500051007     C                   PARM                    DSARBD
078600051007     C                   PARM                    DSARBK
078700051007     C                   PARM                    DARBT
078800051007     C                   PARM                    DSARBG
078900051007     C                   PARM                    trul90ds
079000051007     C                   MOVEL     KPJBU         FNLR48ds
079100051007     C     D48ERR        IFne      *BLANKS
079200051007     C     D48f12        oreq      '1'
079300051007     C                   SETON                                        28
079400051007     C                   MOVEL     ERR(8)        $MSG
079500051007     C     D48MSG        IFNE      *BLANKS
079600051007     C                   MOVEL     D48MSG        $MSG
079700051007     C                   END
079800051007     C                   END
079900051007     C*
080000051007     C                   ENDSR
080100051108     C*
080200051108     C*---------------------------------------------------------------*
080300051108     C* PULIZIA SUBFILE
080400051108     C     puliz         begsr
080500051108     C*---------------------------------------------------------------*
080600051108     C                   seton                                        70
080700051108     C                   write     lg88c02
080800051108     C                   z-add     0             nrr               4 0
080900051108     C                   setoff                                       70
081000051108     C                   endsr
081100051108     C*----------------------------------------------------*
081200051108     C     caricanote    begsr
081300051108     C*----------------------------------------------------*
081400051108     c                   exsr      puliz
081500051108     c                   clear                   oknote            1
081600051108     C     KNOT          SETLL     tignp01L
081700051108     c                   do        *hival
081800051108     C     KNOT          reade     tignp01L
081900051108     c                   if        %eof(tignp01l)
082000051108     c                   leave
082100051108     c                   end
082200051108     c                   move      'X'           oknote
082300051108     c                   eval      vdmc1  = gnpdmc
082400051108     c                   add       1             nrr
082500051108     c                   write     lg88s02
082600051108     c                   enddo
082700051108      *
082800051108     C                   ENDSR
082900050316      *----------------------------------------------------------
083000941003** ERR
083100941003Bolla inesistente                                                     01
083200941003Bolla già consegnata                                                  02
083300941003Data errata                                                           03
083400051006Data esecuzione disposizioni minore data apertura giacenza            04
083500051006Data esecuzione disposizioni minore data immissione                   05
083600051006Data eseguibilità superiore di oltre xx giorni data imm. disposizioni 06
083700051006Data esecuzione disposizioni minore di oggi                           07
083800051007Attenzione !! Variazione bolla non effettuata per conflitto           08
