000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200941021     H* FNLR90R *----------------------------------------------------*
000300160519     H*       - Aggiornamento in partenza data di consegna e
000400160519     H*         trasmissione a sede
000500911014     H*--------------------------------------------------------------*
000600020920     FFNARB72L  UF   E           K DISK
000700990803     FFNEVB01L  IF   E           K DISK
000800970314     FFNLBL01L  IF   E           K DISK
000900941025     FFNLBL02L  IF   E           K DISK
001000941025     F                                     RENAME(FNLBL000:FNLBLMA)
001100010103     FFIARBC46  O    E             DISK    USROPN
001200160526     FFNART01L  UF   E           K DISK
001300160519     FFNblt01L  UF   E           K DISK
001400160527     FFNblp01L  UF   E           K DISK
001500160520     FFiar401L  IF   E           K DISK
001600160616     Ffnarb01l  uF   E           K DISK    rename(fnarb000:fnarb001) usropn
001700941026     FTABEL00F  IF   E           K DISK    USROPN
001800970307     D*--------------------------------------------------------------*
001900160520     D CLI             S              7  0 DIM(9990)                            COD.CLI DA TRASM.
002000950126     D* CODICI BOLLE DA NON INVIARE IN SEDE
002100950126     D CDB             S              2    DIM(40)                              COD. BOLLE
002200970314     D* schiera x cariacmaneto codici eventi IDD
002300080905     D IDD             S              3    DIM(200)                             Cod.eventi IDD
002400950411     D* SCHIERE X QCMDEXEC
002500160520     D CMDC            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC CHKOBJ
002600160520     D CMDA            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC ADDPFM
002700160520     D CMDO            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC OVRDBF
002800020920     D CMDL            S             43    DIM(1) CTDATA PERRCD(1)              QCAEXEC ALCOBJ
002900020920     D CMDD            S             43    DIM(1) CTDATA PERRCD(1)              QCAEXEC DLCOBJ
003000160511     d savflp          s                   like(arbflp)
003100970307     D*--------------------------------------------------------------*
003200970307     D*  DS
003300970307     D*--------------------------------------------------------------*
003400911014     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003500160520     d
003600941025     D WLBDA8          DS
003700941025     D  G08DAT                 1      8  0
003800941025     D  G08INV                 9     16  0
003900941025     D  G08ERR                17     17
004000941025     D  G08TGI                18     22  0
004100160520     d
004200160527     D DSCMZ         E DS
004300970805     D DS2A          E DS
004400950126     D DS3A          E DS
004500160520     D DS3K          E DS
004600911014     D CNCR80        E DS
004700970925     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
004800911014     D KPJBA         E DS
004900911014     D  LIBSYS                92    101
005000911112      *
005100020920     D TIBS50        E DS                  EXTNAME(TIBS55ds)
005200160520
005300971120     D PARAM5          DS
005400911015     D  LEGFIL                 1     10
005500020925     D  MBRFIL                11     20
005600160520
005700020920     D PARAM7a         DS
005800020920     D  nmi7a                  1      7
005900020920     D  fgs7a                  8     10
006000160616     d
006100160616     D PARAM7b         DS
006200160616     D  pa7nsped               1     16
006300160616     D  pa7rich               17     17
006400160616     D  pa7err                18     18
006500160520
006600020920     d   §membro       ds
006700020920     d  §emme                  1      1    inz('M')
006800020925     d  xfle                   2      4s 0
006900020920     d  §sed                   5      7s 0 inz(046)
007000020920     d  §mbr                   2      7
007100160616     d
007200160616     d  kpaas          s                   like(arbaas)
007300160616     d  kplnp          s                   like(arblnp)
007400160616     d  kpnrs          s                   like(arbnrs)
007500160616     d  kpnsp          s                   like(arbnsp)
007600160616     d  Singola        s              1
007700911014     C*---------------------------------------------------------------*
007800941216     C* PREPARAZIONE ARCHIVIO DATI CONSEGNA  + DETTAGLIO SEGNACOLLI
007900160616     c
008000160616     c                   select
008100160616     c* Elaboro una bolla soltanto
008200160616    1c                   when      %parms=2 and kpnsp>0
008300160616     c                   exsr      Ela_bolla
008400160616    1c
008500950102     C* TRASMETTO SOLO SE HO MANDATO INTROITO IMPOSTATO
008600160616    1C                   when      KNMI>0
008700930421     C                   EXSR      ARBCSR
008800941025     C*
008900941025      * RILEGGO X TRASMETTERE ALLA BOLLA ORIGINALE
009000941025     C                   EXSR      ARBORI
009100160616     c                   endsl
009200160616
009300941025     C*
009400930421     C* I N V I O  A SEDE
009500930421     C*
009600160616    1c                   if        %open(fiarbc46)
009700010103     C                   CLOSE     FIARBC46
009800160616
009900160617    2C***  NRSED         IFGT      *ZEROS
010000160617     c
010100160617     c                   if        KNMI>0
010200911111     C                   EXSR      INVSED
010300160617    2C                   ENDif
010400010103      * Disalloco FIARBC46
010500020925     C                   MOVEL(p)  CMDD(1)       COMMAN
010600020925     c                   eval      %subst(comman:36:6) = §mbr
010700950411     C                   CALL      'QCMDEXC'                            60
010800950411     C                   PARM                    COMMAN
010900020920     C                   PARM                    LUNG43
011000950411     C*
011100160616    1C                   ENDIF
011200160616
011300930415      *
011400950411     C     FINE          TAG
011500160520     C*
011600971120     C                   CLEAR                   TIBS50
011700971121     C                   MOVEL     'C'           I50TLA
011800020920     C                   CALL      'TIBS55R'
011900971120     C                   PARM                    TIBS50
012000971120     C*
012100911015     C                   SETON                                        LR
012200930421     C***--------------------------------------------***
012300930421     C* PREPARAZIONE ARCHIVIO DATI CONSEGNA
012400930421     C***--------------------------------------------***
012500930421     C     ARBCSR        BEGSR
012600930421     C                   SETOFF                                       10
012700160616     c
012800020920     C     karb72        SETLL     FNARB72L
012900930421     C*
013000930421     C* APRO FILE X LA SEDE
013100930421     C                   EXSR      CAEX46
013200930421     C*
013300160519    1C     *IN10         DOUEQ     '1'
013400020920     C     karb72        READE     FNARB72L                             1510
013500930421     C  N15
013600160519    2CANN10              DO
013700160616     c
013800160616     c                   exsr      EventiB
013900160616      *
014000941024     C                   MOVEL     *BLANKS       ARBFAG
014100160527     C                   MOVEL     *BLANK        HAFIGL            1
014200160527     C                   MOVEL     *BLANK        HAMAMM            1
014300160527     C                   MOVEL     *BLANK        HAORIG            1
014400160527     C                   MOVEL     *BLANK        ARBFAG
014500160519      *
014600160527      * elaboro la bolla per :
014700160527      * - consegna totale
014800160527      * - consegna parziale
014900160527      * - evento IDD
015000160519    3C     ARBDCM        IFGT      *ZEROS
015100930524     C     ARBDCP        ORGT      *ZEROS
015200970805     C     WTREVE        OREQ      'S'
015300160527     c
015400160616     c                   exsr      Sd99_lbl
015500160616
015600160616     c                   exsr      Agg_ART
015700160616
015800160616     c                   exsr      Agg_ARBBLP
015900930524      *
016000160616    3C                   endif
016100160616    2C                   ENDDO
016200160616    1C                   ENDDO
016300930421      *
016400930421     C                   ENDSR
016500160616      * ----------------------------------------------------  EVENTO IDD
016600160616     c     EventiB       BEGSR
016700160616      * ----------------------------------------------------  EVENTO IDD
016800160616      * Controllo se sulla bolla esiste evento di IDD
016900160616     C                   MOVEL     ' '           WTREVE            1
017000160616    3C     ARBCCA        IFNE      '5'
017100160616     C                   Z-ADD     ARBAAS        KAAS
017200160616     C                   Z-ADD     ARBLNP        KLNP
017300160616     C                   Z-ADD     ARBNRS        KNRS
017400160616     C                   Z-ADD     ARBNSP        KNSP
017500160616     C* Loop su eventi fino a fine o trovato evento
017600160616     C     KEVB          SETGT     FNEVB01L
017700160616     C     KEVB          READPE    FNEVB01L                               34
017800160616    4C     *IN34         DOWEQ     '0'
017900160616     C                   Z-ADD     1             W
018000160616     C     EVBCEV        LOOKUP    IDD(W)                                 34
018100160616    5C     *IN34         IFEQ      '1'
018200160616     C                   MOVEL     'S'           WTREVE
018300160616     C                   ELSE
018400160616     C     KEVB          READPE    FNEVB01L                               34
018500160616    5C                   END
018600160616    4C                   ENDdo
018700160616     C*
018800160616    3C                   ENDif
018900160616     c                   ENDSR
019000160616      * -------------------------------------------------------------------
019100160616     c     SD99_lbl      BEGSR
019200160616      * --------------------------------------------------------------------
019300160616      * SE SPEDIZIONE CONSEGNATA CALCOLO TEMPI DELIVERY
019400160616     C                   MOVE      *ZEROS        ARBNCI
019500160616     C                   MOVE      *ZEROS        ARBNPI
019600160616     C                   MOVE      *ZEROS        ARBDCE
019700160616     C                   MOVE      *ZEROS        ARBNRC
019800160616     c                   clear                   tnsd99
019900160616     C                   MOVEL     *BLANKS       ARBFAG
020000160616     C                   MOVEL     *BLANK        HAFIGL            1
020100160616     C                   MOVEL     *BLANK        HAMAMM            1
020200160616     C                   MOVEL     *BLANK        HAORIG            1
020300160616     C                   MOVEL     *BLANK        ARBFAG
020400160616      *
020500160616      * in caso di consegna parziale, non mi serve verificare i legami bolla
020600160616      * e nemmeno l'affidabilità
020700160616    4C     ARBDCM        IFGT      *ZEROS
020800160616     C     ARBCCA        OREQ      '5'
020900160616     C     WTREVE        OREQ      'S'
021000160616      * GUARDO COSA DEVO TRASMETTERE ALLA PARTENZA SE CI SONO LEGAMI
021100160616      * - LA BOLLA HA UNA FIGLIA CON LNA DIVERSA (DIROTT./RESO)
021200160616     C                   MOVEL     *BLANK        HAFIGL            1
021300160616     C                   MOVEL     *BLANK        HAMAMM            1
021400160616     C                   MOVEL     *BLANK        HAORIG            1
021500160616     C                   MOVEL     *BLANK        ARBFAG
021600160616     C     KARB          SETGT     FNLBL02L
021700160616     C     KARB          READPE    FNLBL02L                               22
021800160616    5C     *IN22         DOWEQ     '0'
021900160616     C     ARBLNA        IFNE      LBLLAN
022000160616     C                   MOVEL     'S'           HAFIGL
022100160616     C                   ELSE
022200160616     C* Se la bolla ha una figlia con la stessa lna devo controllare
022300160616     C* se quest'ultima è chiusa con IDD
022400160616     c* ES 26/05: non capendo il significato di queste specifiche
022500160616     c*  le asterisco. non ho trovato il caso di bolla figlia con stessa LNA
022600160616     c*  e con dettaglio colli per leggerlo per aggiornare la mamma.
022700160616     c*  il cambio di porto, unico caso che ci è venuto in mente, non ha dettaglio colli
022800160616     C****               MOVEL     'S'           WCTIDD
022900160616     C                   END
023000160616     c
023100160616     C     KARB          READE     FNLBL02L                               22
023200160616    5C                   ENDDO
023300160616      *
023400160616      * - LA BOLLA HA UNA MAMMA + GUARDO LNA BOLLA ORIGINALE
023500160616     C     KARB          CHAIN     FNLBL01L                           22
023600160616    5C     *IN22         IFEQ      '0'
023700160616     C                   MOVEL     'S'           HAMAMM
023800160616     C     LBLLAO        IFNE      LBLLAP
023900160616     C     LBLLAO        ORNE      LBLLAN
024000160616     C                   MOVEL     'S'           HAORIG
024100160616     C                   END
024200160616     c* occorre trasmettere la data consegna anche alla bolla originale
024300160616     c* se la figlia è un reso con stessa linea arrivo
024400160616     C     LBLLAO        IFeq      LBLLAP
024500160616     C     LBLLAO        andeq     LBLLAN
024600160616     c     arbfbr        andeq     'S'
024700160616     C                   MOVEL     'S'           HAORIG
024800160616     c                   end
024900160616    5C                   ENDIF
025000160616      * SE HA FIGLIA ALTRA FIL.MA NON HA MAMMA è LA 1°
025100160616      * NON TRASMETTO DATA CONSEGNA
025200160616    5C     HAFIGL        IFEQ      'S'
025300160616     C     HAMAMM        ANDEQ     ' '
025400160616     C     ARBCCA        ANDNE     '2'
025500160616     C     ARBCCA        ANDNE     '6'
025600160616     C                   MOVEL     'D'           ARBFAG                         BOLLA DIROTTATA
025700160616    5C                   END
025800160616      *-----------------------------------------------------*
025900160616      * CALCOLO TEMPI DELIVERY
026000160616     C     ARBDCM        IFGT      0
026100160616     C                   MOVEL     ' '           D98TLA
026200160616     C                   MOVEL     'A'           D98TBO
026300160616     C                   MOVE      ARBAAS        D98AAS
026400160616     C                   MOVE      ARBLNP        D98LNP
026500160616     C                   MOVE      ARBNRS        D98NRS
026600160616     C                   MOVE      ARBNSP        D98NSP
026700160616     C                   CALL      'TNSD99R'
026800160616     C                   PARM                    TNSD99
026900160616     C                   END
027000160616    4C                   ENDif
027100160616     c
027200160616     c                   ENDSR
027300160616      *-----------------------------------------------------*
027400160616     c     Agg_art       BEGSR
027500160616      *-----------------------------------------------------*
027600160616      * ORA AGGIORNO PER OGNI COLLO DI ART FNBLT, IGNORANDO GLI SFLAGGATI MA
027700160616      *  AGGIORNANDO TUTTI I RECORD
027800160616     c                   setoff                                         11
027900160616     C     KART          SETLL     FNART01L
028000160616     C     KART          READE     FNART01L                             1511
028100160616     C                   DOW       NOT *IN15 AND NOT *IN11
028200160616     c                   exsr      Agg_blt
028300160616     c* se sflaggato,
028400160616     C*  FLEGGO COME TRASMESSI SEGNACOLLI SE NO BOLLA LEGATA
028500160616     c* ( solo quelli slafaggati, ma comunque li aggiorno tutti )
028600160616     c                   if        artftr=' ' and (haorig<>'S' or
028700160616     c                             hafigl<>' ')
028800160616     C                   MOVE      'T'           ARTFTR
028900160616     C                   EXCEPT    AGGT0
029000160616     C                   endif
029100160616
029200160616     C     KART          READE     FNART01L                             1511
029300160616     c                   enddo
029400160616     c                   ENDSR
029500160616      *-----------------------------------------------------*
029600160616     c     Agg_ARBBLP    BEGSR
029700160616      *-----------------------------------------------------*
029800160616     c***
029900160616     c* AGGIORNAMENTO FNBLP / FNARB --> solo nessun errore in lettura segnacolli
030000160616   4ac                   if        not *in15
030100160616     c* imposto tempi di delivery
030200160616     C                   Z-ADD     D98NCI        ARBNCI                         NON CONF.CONS. INT.
030300160616     C                   Z-ADD     D98NPI        ARBNPI                         NON CONF.PART. INT.
030400160616     C                   Z-ADD     D98DCE        ARBDCE                         DATA CONS. PREV.CLI.
030500160616     C                   Z-ADD     D98NRC        ARBNRC                         NON CONF. RESA CLI.
030600160616      *
030700160616    5C     ARBDCM        IFGT      *ZEROS
030800160616     C     ARBDCP        ORGT      *ZEROS
030900160616     c                   exsr      Agg_BLP
031000160616    5C                   END
031100160616     c***
031200160616      * A SEDE SOLO SE CONSEGNATE
031300160616     c***
031400160616    5C     ARBDCM        IFGT      0
031500160616     C* CONTROLLO SE LA BOLLA NON è DA INVIARE IN SEDE
031600160616     C                   Z-ADD     1             §F
031700160616     C                   SETOFF                                       03
031800160616      * BOLLE DA NON INVIARE
031900160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
032000160616    6c                   if        not *in03
032100160616     c* utilizzo il campo ARBFLP (fil.transito) x aggiornare la zona in sede
032200160616     c                   eval      savflp = arbflp
032300160616     c                   eval      arbflp = arbznc
032400160617     C****               ADD       1             NRSED             7 0
032500160616     C                   WRITE     FIARBC00                                     SEDE
032600160616     c                   eval      arbflp = savflp
032700160616    6C                   ENDIF
032800160616    5C                   ENDIF
032900160616      *
033000160616      * HA BOLLA ORIG.CON LINEA ARRIVO DIVERSA E NON HA FIGLIA CON
033100160616      * LNA DIVERSA : è L'ULTIMA BOLLA DELLA CATENA QUINDI DEVO TRASM.
033200160616      * ALLA BOLLA ORIGIN. LA DATA CONSEGNA , NON AGGIORNO ARBDT1 E
033300160616      * IMPOSTO FT1=O X RILEGGERLO SUCCESSIVAMENTE
033400160616    5C     HAORIG        IFEQ      'S'
033500160616     C     HAFIGL        ANDEQ     ' '
033600160616     C                   MOVEL     'O'           ARBFT1
033700160616     C                   MOVE      *ZEROS        ARBDT1
033800160616     C                   ELSE
033900160616     C                   MOVE      'T'           ARBFT1
034000160616     C                   Z-ADD     AMGUDT        ARBDT1
034100160616    5C                   ENDIF
034200160616      *
034300160616    5C     ARBDCM        IFGT      0
034400160616     C*                  MOVE      'T'           ARBFT2
034500160616     C*                  Z-ADD     AMGUDT        ARBDT2
034600160616    5C                   ENDIF
034700160616      *
034800160616     c                   if        singola=' '
034900160616     C                   EXCEPT    AGGB0
035000160616     c                   else
035100160616     C                   EXCEPT    AGGB1
035200160616     c                   endif
035300160616     c
035400160616   4aC                   endif
035500160616     c                   ENDSR
035600160519     C**-------------------------------------------------------------**
035700160519     C** Aggiorna dettaglio colli partenza
035800160519     C**-------------------------------------------------------------**
035900160519     c     Agg_blt       BEGSR
036000160527     c* Lettura sequenziale per aggiornamento dett colli BOLLA ORIGINALE
036100160527     c                   if        arbfag='O'
036200160527     c
036300160527     c                   if        primalet=' '
036400160527     c     kblpo         setll     fnblt01l
036500160527     c     kblpo         reade     fnblt01l
036600160527     c                   eval      primalet='1'
036700160527     c                   else
036800160527     c     kblpo         reade     fnblt01l
036900160527     c                   endif
037000160527     c
037100160527     c                   else
037200160527     c*
037300160527     c* lettura per segnacollo negli altri casi
037400160519     c     kblt          chain     fnblt01l
037500160527     c                   endif
037600160527     c
037700160519     c                   eval      bltfl1=artfl1
037800160526     c                   if        arbfag<>'D'
037900160519     c                   eval      bltdcm=artdcm
038000160526     c                   endif
038100160519     c                   eval      bltdet=artdet
038200160519     c                   eval      bltdut=artdut
038300160519     c                   eval      bltdam=artdam
038400160520     c                   eval      bltflp=artflp
038500160520     C     BLTFTR        IFNE      'C'
038600160520     C                   MOVEL     'R'           BLTFTR
038700160520     C                   MOVEL     AMGUDT        BLTDTR
038800160520     C                   END
038900160527     c                   if        arbfag='O' and not %eof(fnblt01l)
039000160527     c                   update    fnblt000
039100160527     c                   endif
039200160527     c
039300160527     c                   if        arbfag<>'O' and %found(fnblt01l)
039400160519     c                   update    fnblt000
039500160519     c                   endif
039600160527
039700160519     c                   ENDSR
039800160519     C**-------------------------------------------------------------**
039900160519     C** Aggiorna bolla partenza
040000160519     C**-------------------------------------------------------------**
040100160519     c     Agg_blp       BEGSR
040200160526     c     karb          chain     fnblp01l
040300160519     c                   if        %found(fnblp01l)
040400160520     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
040500160520     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
040600160520     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
040700160520     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
040800160520     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
040900160520     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
041000160520     C     ARBFLP        IFGT      0
041100160520     C                   MOVE      ARBFLP        BLPFLP                         FILIALE TRANSITO
041200160520     C                   ENDIF
041300160520     C     BLPDET        IFEQ      0
041400160520     C                   MOVE      ARBDET        BLPDET
041500160520     C                   ENDIF
041600160520     C     BLPDAM        IFEQ      0
041700160520     C                   MOVE      ARBDAM        BLPDAM
041800160520     C                   ENDIF
041900160520     C     BLPDUT        IFEQ      0
042000160520     C                   MOVE      ARBDUT        BLPDUT
042100160520     C                   ENDIF
042200160520     C                   MOVE      ARBNDC        BLPNDC                         NR.DIST.CONS
042300160520     C                   MOVE      ARBDDC        BLPDDC                         DATA "  CONS
042400160520     C     BLPCCA        IFNE      '5'
042500160520     C                   MOVE      ARBCCA        BLPCCA                         T.MANC.CONS.
042600160520     C                   END
042700160520     C     BLPCCA        IFNE      '5'
042800160520     C     BLPDCP        OREQ      0
042900160520     C                   MOVE      ARBDCP        BLPDCP                         DATA 1° CONS
043000160520     C                   END
043100160520     C                   MOVE      ARBTMC        BLPTMC                         T.MANC.CONS.
043200160520     c
043300160520     c                   exsr      DATCLI
043400160520     C*
043500160520     c* se bolla dirottata non aggiorno la data e ora consegna merce
043600160520     c*  dovrò aggiornare con la bolla figlia
043700160520     c                   if        arbfag<>'D'
043800160519     c                   eval      blpdcm=arbdcm
043900160519     c                   eval      blphmc=arbhmc
044000160520     c                   endif
044100160520     c
044200160519     c                   update    fnblp000
044300160519     c                   endif
044400160519     c                   ENDSR
044500930421     C**-------------------------------------------------------------**
044600010103     C** QCAEXEC SUL MBR DI FIARBC46 PER LA SEDE
044700930421     C**-------------------------------------------------------------**
044800930421     C     CAEX46        BEGSR
044900930421     C** FILIALE PER APERTURA MEMBRO
045000930421     C** CONTROLLO SE ESISTE IL MEMBRO
045100930421     C                   SETOFF                                       60
045200160520     C                   MOVEL(P)  CMDC(1)       COMMAN           80
045300020920     c                   eval      %subst(comman:42:6) = §mbr
045400930421     C                   CALL      'QCMDEXC'                            60
045500930421     C                   PARM                    COMMAN
045600020920     C                   PARM                    LUNG48
045700930421     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
045800930421     C   60              DO
045900160520     C                   MOVEL(p)  CMDA(1)       COMMAN
046000020920     c                   eval      %subst(comman:42:6) = §mbr
046100930421     C                   CALL      'QCMDEXC'
046200930421     C                   PARM                    COMMAN
046300020920     C                   PARM                    LUNG48
046400930421     C                   END
046500930421     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
046600160520     C                   MOVEL(P)  CMDO(1)       COMMAN
046700020920     c                   eval      %subst(comman:42:6) = §mbr
046800930421     C                   CALL      'QCMDEXC'
046900930421     C                   PARM                    COMMAN
047000020920     C                   PARM                    LUNG48
047100010103     C* Provo ad allocare FIARBC46 X 3 volte
047200950411     C                   MOVEL     'N'           WALLOC            1
047300950411     C                   Z-ADD     1             XX                2 0
047400020920     C                   MOVEL(p)  CMDL(1)       COMMAN
047500020920     c                   eval      %subst(comman:36:6) = §mbr
047600950412     C     WALLOC        DOWEQ     'N'
047700950411     C     XX            ANDLE     3
047800950411     C                   CALL      'QCMDEXC'                            60
047900950411     C                   PARM                    COMMAN
048000020920     C                   PARM                    LUNG43
048100950411     C     *IN60         IFEQ      '0'
048200950411     C                   MOVEL     'S'           WALLOC                         Allocazione O.K.
048300950411     C                   END
048400950411     C                   ADD       1             XX
048500950411     C                   END
048600950411     C*  Se non sono riuscita ad allocarlo vado a fine pgm. e
048700950411     C*  risottometto in coda la trasmissione date di consegna
048800950411     C     WALLOC        IFEQ      'N'
048900160616     c                   if        singola=' '
049000020920     C                   MOVEL(p)  param7a       KPJBU
049100950411     C                   MOVEL     'LR90'        KCOAZ
049200950411     C                   CALL      'BCH10'
049300950411     C                   PARM                    KPJBA
049400950411     C                   GOTO      FINE
049500160616     c                   else
049600160616     c                   eval      pa7err='E'
049700160616     c                   endif
049800160616     c
049900950411     C                   END
050000950411     C*
050100010103     C                   OPEN      FIARBC46
050200160616     c
050300930421     C                   ENDSR
050400941025     C***--------------------------------------------***
050500941025     C* TRASMISS. SOLO DATA CONSEGNA ALLA BOLLA ORIGINALE
050600941025     C***--------------------------------------------***
050700941025     C     ARBORI        BEGSR
050800160527     C                   SETOFF                                       10
050900160527     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
051000160527     c*
051100020920     C     karb72        SETLL     FNARB72L
051200941025     C*
051300160520    1C     *IN10         DOUEQ     '1'
051400020920     C     Karb72        READE     FNARB72L                             1510
051500941025     C  N15
051600160520    2CANN10ARBFT1        IFEQ      'O'
051700941025     C     KARB          CHAIN     FNLBL01L                           22
051800160526     c                   clear                   primalet          1
051900160520      *
052000941025      * TRASMETTO SE C'E' DATA CONSEGNA
052100160520    3C     ARBDCM        IFGT      *ZEROS
052200970805     C     ARBCCA        OREQ      '5'
052300160527     C
052400160527     c* Leggo il dettaglio colli della bolla figlia
052500160527     c*  e aggiorno il detto collo della bolla mamma
052600160527     c                   setoff                                         11
052700160527     C     KART          SETLL     FNART01L
052800160527     C     KART          READE     FNART01L                             1511
052900160527    4C                   DOW       NOT *IN15 AND NOT *IN11
053000160527     c                   exsr      Agg_blt
053100160527
053200160527     c                   if        artftr=' '
053300160527     C                   MOVE      'T'           ARTFTR
053400160527     C                   EXCEPT    AGGT0
053500160527     C                   endif
053600160527     C     KART          READE     FNART01L                             1511
053700160527    4c                   enddo
053800160527     c
053900970306     C*
054000970313      * TRASMETTO TESTATA SE C'E' DATA CONSEGNA
054100160527      * se ART allocato non aggiorno la testata e aspetto il prossimo giro
054200160520    4C     ARBDCM        IFGT      *ZEROS
054300160527     c     *in15         andeq     *off
054400160527     c
054500160526     c     kblpo         chain     fnblp01l
054600160527    5c                   if        %found(fnblp01l)
054700160526     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
054800160526     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
054900160526     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
055000160526     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
055100160526     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
055200160526     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
055300160527    6C     ARBFLP        IFGT      0
055400160526     C                   MOVE      ARBFLP        BLPFLP                         FILIALE TRANSITO
055500160527    6C                   ENDIF
055600160526     C                   MOVE      ARBDET        BLPDET
055700160526     C                   MOVE      ARBDUT        BLPDUT
055800160526     C                   MOVE      ARBdam        BLPdam
055900160526     C* X BOLLA ORIGINALE AGGIORNO TMC CON FLAG CONSEGNA ANOMALA 5
056000160526     C                   MOVE      ARBNDC        BLPNDC                         NR.DIST.CONS
056100160526     C                   MOVE      ARBDDC        BLPDDC                         DATA "  CONS
056200160527    6C     BLPCCA        IFNE      '5'
056300160526     C     BLPDCP        OREQ      0
056400160526     C                   MOVE      ARBDCP        BLPDCP                         DATA 1° CONS
056500160527    6C                   END
056600160526     C                   MOVE      ARBTMC        BLPTMC                         T.MANC.CONS.
056700160526     C                   MOVE      ARBDCM        BLPDCM
056800160526     C                   MOVE      ARBHMC        BLPHMC
056900160526      * Controllo se trasmettere dati cliente
057000160526     C                   EXSR      DATCLI
057100160526     c
057200160526     c                   update    fnblp000
057300160527    5c                   endif
057400160526     c
057500160526     C                   MOVE      LBLLPO        ARBLNP
057600970313     C                   MOVE      LBLAAO        ARBAAS
057700970313     C                   MOVE      LBLNRO        ARBNRS
057800970313     C                   MOVE      LBLNSO        ARBNSP
057900160527     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
058000970313     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
058100950330      * SE NON TRASMETTO A SEDE LA BOLLA FIGLIA NON TRASMETTO
058200950330      * NEANCHE LA BOLLA ORIGINALE
058300950330     C                   Z-ADD     1             §F
058400950330     C                   SETOFF                                       03
058500950330      * BOLLE DA NON INVIARE
058600950330     C     ARBCBO        LOOKUP    CDB(§F)                                03
058700160617     C**N03              ADD       1             NRSED             7 0
058800010103     C  N03              WRITE     FIARBC00                                     SEDE
058900160520    4C                   END
059000941025     C*
059100941025     C                   MOVEL     'T'           ARBFT1
059200941025     C                   MOVE      AMGUDT        ARBDT1
059300160520    3C                   ENDif
059400160520     c
059500160616     c                   if        singola=' '
059600941025     C                   EXCEPT    AGGOR
059700160616     c                   else
059800160616     C                   EXCEPT    AGGOR1
059900160616     c                   endif
060000160616     c
060100160520    2C                   ENDif
060200160520    1C                   ENDdo
060300941025      *
060400941025     C                   ENDSR
060500160616     C***--------------------------------------------***
060600160616     C* Elaboro soltanto la bolla passata
060700160616     C***--------------------------------------------***
060800160616     c     Ela_bolla     BEGSR
060900160616
061000160616     c                   eval      singola='S'
061100160616     c
061200160616    1c                   if        not %open(fnarb01l)
061300160616     c                   open      fnarb01l
061400160616    1c                   endif
061500160616     c
061600160616     c     kpbol         chain(e)  fnarb01l
061700160616    1c                   if        %error or not %found(fnarb01l)
061800160616     c                   eval      pa7err='E'
061900160616     c                   leavesr
062000160616    1c                   endif
062100160616     c
062200160616     C* APRO FILE X LA SEDE
062300160616     C                   EXSR      CAEX46
062400160616     c
062500160616    1c                   if        pa7rich<>'C'
062600160616     c                   exsr      EventiB
062700160616    1c                   endif
062800160616     c
062900160616     c* se tipo richiamo = 'C' --> solo ricalcolo affidabilità solo se consegnata
063000160616     c*                            altrimenti in tutti i casi previsti
063100160616
063200160616    0c                   if        (pa7rich='C' and arbdcm>0) or
063300160616     c                             (pa7rich<>'C' and (arbdcm>0 or arbdcp>0
063400160616     c                             or wtreve='S'))
063500160616     c
063600160616     c                   exsr      SD99_LBL
063700160616     c
063800160616    1c                   if        pa7rich<>'C'
063900160616     c                   exsr      Agg_ART
064000160616
064100160616     c                   exsr      Agg_ARBBLP
064200160616     c
064300160616   x1c                   else
064400160616     c
064500160616    2C     D98DCE        IFNE      ARBDCE
064600160616     C     D98NPI        ORNE      ARBNPI
064700160616     C     D98NCI        ORNE      ARBNCI
064800160616     C     D98NRC        ORNE      ARBNRC
064900160616     C                   Z-ADD     D98DCE        ARBDCE
065000160616     C                   Z-ADD     D98NPI        ARBNPI
065100160616     C                   Z-ADD     D98NCI        ARBNCI
065200160616     C                   Z-ADD     D98NRC        ARBNRC
065300160616     C                   MOVEL     'C'           ARBFAG
065400160616     C                   Z-ADD     1             §F
065500160616     C                   SETOFF                                       03
065600160616      * BOLLE DA NON INVIARE
065700160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
065800160616    3c                   if        not *in03
065900160617     C***                ADD       1             NRSED             7 0
066000160616     C                   WRITE     FIARBC00                                     SEDE
066100160616    3C                   ENDIF
066200160616     c
066300160616     c     karb          chain     fnblp01l
066400160616    3c                   if        %found(fnblp01l)
066500160616     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
066600160616     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
066700160616     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
066800160616     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
066900160616     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
067000160616     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
067100160616     c                   update    fnblp000
067200160616    3c                   endif
067300160616
067400160616     c                   update    fnarb001
067500160616
067600160616     c* Se la bolla ha la mamma --> aggiorno anche la bolla originale
067700160616    3c                   if        haorig='S' and hafigl=' '
067800160616     C     KARB          CHAIN     FNLBL01L
067900160616    4c                   if        %found(fnlbl01l)
068000160616     c     kblpo         chain     fnblp01l
068100160616    5c                   if        %found(fnblp01l)
068200160616     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
068300160616     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
068400160616     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
068500160616     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
068600160616     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
068700160616     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
068800160616     c                   update    fnblp000
068900160616    5c                   endif
069000160616     c
069100160616     C                   MOVE      LBLLPO        ARBLNP
069200160616     C                   MOVE      LBLAAO        ARBAAS
069300160616     C                   MOVE      LBLNRO        ARBNRS
069400160616     C                   MOVE      LBLNSO        ARBNSP
069500160616     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
069600160616     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
069700160616      * SE NON TRASMETTO A SEDE LA BOLLA FIGLIA NON TRASMETTO
069800160616      * NEANCHE LA BOLLA ORIGINALE
069900160616     C                   Z-ADD     1             §F
070000160616     C                   SETOFF                                       03
070100160616      * BOLLE DA NON INVIARE
070200160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
070300160617     C**N03              ADD       1             NRSED             7 0
070400160616     C  N03              WRITE     FIARBC00                                     SEDE
070500160616    4c                   endif
070600160616    3c                   endif
070700160616    2c                   endif
070800160616      *
070900160616    1C                   endif
071000160616    0C                   endif
071100160616
071200160616     c                   ENDSR
071300930510      *----------------------------------------------------------------
071400930510      * PULIZIA PARAMETRI X TRUL51C
071500930510      *----------------------------------------------------------------
071600930510     C     TRUL51        BEGSR
071700971120     C                   MOVEL     '*LIBL'       LIBR             10
071800930510     C                   MOVEL     *BLANK        FILE             10
071900930510     C                   MOVEL     *BLANK        MBR              10
072000930510     C                   Z-ADD     0             NRR              10 0
072100930510     C                   ENDSR
072200930415     C**----------------------------------
072300930415     C** LANCIO AZIONI DI TRASMISSIONE IN SEDE
072400930415     C**----------------------------------
072500930415     C     INVSED        BEGSR
072600930510     C                   EXSR      TRUL51
072700010103     C                   MOVEL     'FIARBC46'    FILE
072800020920     C                   MOVEL     §membro       MBR
072900930510     C                   CALL      'TRUL51C'
073000930510     C                   PARM                    LIBR
073100930510     C                   PARM                    FILE
073200930510     C                   PARM                    MBR
073300930510     C                   PARM                    NRR
073400930415     C     NRR           IFGT      0
073500971120     C     NRR           ANDNE     9999999999
073600971120     C* Richiamo TIBS50R per conoscere librerie/S.Informativi
073700971120     C* da/a cui spedire
073800971120     C                   CLEAR                   TIBS50
073900971120     C                   MOVEL     *BLANKS       I50TLA
074000971120     C                   MOVEL     KNSIF         I50PSI
074100971120     C                   MOVEL     SIMFEL        I50PPO
074200971120     C                   MOVEL     '046'         I50APO
074300020920     C                   CALL      'TIBS55R'
074400971120     C                   PARM                    TIBS50
074500971120     C* INVIO
074600930415     C                   EXSR      X25SED
074700930415     C                   END
074800930415     C                   ENDSR
074900911112     C**----------------------------------
075000911112     C** SOTTOMETTE TRASMISSIONE X LA SEDE
075100911112     C**----------------------------------
075200911112     C     X25SED        BEGSR
075300971120     C*
075400971120     C                   CLEAR                   W236
075500911112     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
075600010103     C                   MOVEL     'FIARBC46'    CMZFLD                          DA: >>>
075700160617     C                   MOVEL(p)  §membro       CMZMBD
075800971120     C                   MOVEL     O50PLA        CMZLBD
075900010103     C                   MOVEL     'FIARB46R'    CMZFLA                          A: >>>
076000020920     C                   MOVEL(p)  §membro       CMZMBA
076100020927     C                   MOVEL     O50ALA        CMZLBA
076200020927     C                   MOVEL     'GAITRA'      CMZLBA
076300020927     C                   MOVE      O50ALA        WQUATR            4
076400020927     C     WQUATR        IFEQ      '101 '
076500020927     C                   MOVE      '201 '        CMZLBA
076600020927     C                   ENDIF
076700010305     C                   MOVE      'R'           CMZURG
076800911112     C                   MOVE      'A'           CMZADD                          IN ADD
076900911112     C                   MOVE      'S'           CMZCLR                          CLEAR
077000911112     C                   MOVE      'S'           CMZFTR                          DA TRANS.
077100911112     C                   MOVE      *BLANK        CMZERR                          1= ERR.
077200010103     C                   MOVE      'TR92'        CMZAZN                          AZIONE
077300020927     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
077400020927     C                   MOVEL     'GAITRA'      CMZSYF
077500020927     C                   MOVE      O50ASI        WQUATR
077600020927     C     WQUATR        IFEQ      '101 '
077700020927     C                   MOVE      '201 '        CMZSYF
077800020927     C                   ENDIF
077900911112     C                   MOVE      NRR           CMZNRR                          N.REC.
078000911112     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
078100021007     C                   CALL      'TIBS53C'
078200971120     C                   PARM                    O50LCL
078300911112     C                   PARM                    CMZDST
078400911112     C                   PARM                    CMZFLD
078500911112     C                   PARM                    CMZMBD
078600911112     C                   PARM                    CMZLBD
078700911112     C                   PARM                    CMZFLA
078800911112     C                   PARM                    CMZMBA
078900911112     C                   PARM                    CMZLBA
079000911112     C                   PARM                    CMZORI
079100911112     C                   PARM                    CMZDTI
079200911112     C                   PARM                    CMZURG
079300911112     C                   PARM                    CMZADD
079400911112     C                   PARM                    CMZCLR
079500911112     C                   PARM                    CMZFTR
079600911112     C                   PARM                    CMZERR
079700911112     C                   PARM                    CMZAZN
079800911112     C                   PARM                    CMZSYF
079900911112     C                   PARM                    CMZNRR
080000911112     C                   PARM                    CMZPRR
080100160527     C                   PARM                    W236            236
080200971120     C*
080300911112     C                   ENDSR
080400160520     C**---------------------------------------------------------
080500160520     C** CONTROLLO SE DEVO TRASMETTERE I DATI AL CLIENTE da BLP
080600160520     C**---------------------------------------------------------
080700160520     C     DATCLI        BEGSR
080800160520      * SE IL CLIENTE VUOLE RICEVERE DATI CONSEGNA SFLEGGO CAMPI TRASMISS.
080900160520     C                   SETOFF                                       32
081000160520     C     BLPCCM        IFNE      0
081100160520     C                   Z-ADD     1             W
081200160520     C     BLPCCM        LOOKUP    CLI(W)                                 32
081300160520     C                   END
081400160520     C  N32BLPKSC        IFNE      0
081500160520     C                   Z-ADD     1             W
081600160520     C     BLPKSC        LOOKUP    CLI(W)                                 32
081700160520     C                   END
081800160520     C* SE ANCORA NON TROVATO CHAIN SU ar4 PER VERIFICARE SE ESISTE IL
081900160520      *CODICE DEL CLIENTE NEL CAMPO NOTE
082000160520     C  N32              DO
082100160520     C     Kar4          CHAIN     Fiar401L                           66
082200160520     C                   IF        not *in66
082300160520     C                   MOVEL     ar4NOT        KSCJ              7 0
082400160520     C                   Z-ADD     1             W
082500160520     C     KSCJ          LoOKUP    CLi(W)                                 32
082600160520     C                   END
082700160520     C                   END
082800160520      *
082900160520     C   32              MOVEL     *BLANK        BLPFT3
083000160520     C   32              MOVE      *ZEROS        BLPDT3
083100160520     C*
083200160520     C                   ENDSR
083300941025     C*****************************************************************
083400941025     C*     I N Z S R
083500941025     C*****************************************************************
083600941025     C     *INZSR        BEGSR
083700971120     C*
083800941025     C     *ENTRY        PLIST
083900941025     C                   PARM                    KPJBA
084000160616     C                   PARM                    param7b
084100160616     c
084200020920     C                   MOVEL     KPJBU         param7a
084300160520
084400950102     C     NMI7A         IFGT      '0000000'
084500950102     C                   MOVE      NMI7A         KNMI
084600020920     C                   clear                   Kipp
084700020920     C                   move      fgs7a         Kipp
084800950102     C                   ENDIF
084900160616     c
085000160616     c                   if        %parms=2 and pa7nsped>*zeros
085100160616     c                   monitor
085200160616     c                   eval      kpaas=%int(%subst(pa7nsped:1:4))
085300160616     c                   eval      kplnp=%int(%subst(pa7nsped:5:3))
085400160616     c                   eval      kpnrs=%int(%subst(pa7nsped:8:2))
085500160616     c                   eval      kpnsp=%int(%subst(pa7nsped:10:7))
085600160616     c                   on-error
085700160616     c                   clear                   kpaas
085800160616     c                   clear                   kplnp
085900160616     c                   clear                   kpnrs
086000160616     c                   clear                   kpnsp
086100160616     c                   endmon
086200160616     c                   endif
086300160616     c
086400160520
086500941025     C                   Z-ADD     1             CODUT
086600941025     C                   CALL      'X§PARUT'
086700941025     C                   PARM                    UTEDSE
086800941025     C                   MOVEL     REC80         CNCR80
086900941025     C*
087000020924     C     SIMTPP        IFEQ      '2'
087100020924     C     SIMTPP        OREQ      *BLANKS
087200020924     C                   MOVE      SIMPOU        §FLE              3 0
087300020924     C                   ELSE
087400020924     C     SIMFEL        COMP      0                                      02
087500020924     C  N02              MOVE      simfel        §fle
087600020924     C                   END
087700020925     C                   z-add     SIMFEL        xFLE
087800941025     C*§§
087900941025     C                   TIME                    WHHUDT           14 0
088000941025     C                   MOVE      WHHUDT        G08DAT
088100941025     C                   MOVE      *ZEROS        G08INV
088200941025     C                   MOVEL     *BLANK        G08ERR
088300941025     C                   CALL      'XSRDA8'
088400941025     C                   PARM                    WLBDA8
088500941025     C                   Z-ADD     G08INV        AMGUDT            8 0
088600941025     C*
088700950126     C*CARICO I CODICI BOLLE DA NON INVIARE IN SEDE
088800010305     C                   OPEN      TABEL00F
088900950126     C                   MOVEL     '3A'          COD
089000950126     C                   MOVE      *ZEROS        §F                2 0
089100950126     C     KTAB1         SETLL     TABEL
089200160608     C     KTAB1         READE     TABEL
089300160608     c                   dow       not %eof(tabel00f)
089400160608
089500160608     C     TBLFLG        IFEQ      *BLANK                                        ANNUL.
089600950126     C                   MOVEL     TBLUNI        DS3A
089700950126     C     §3ABSD        IFEQ      'N'                                           NO TRASM.IN SEDE
089800950126     C                   ADD       1             §F
089900950126     C                   MOVEL     TBLKEY        CDB(§F)
090000950126     C                   ENDIF
090100950126     C                   ENDIF
090200160608
090300160608     C     KTAB1         READE     TABEL
090400950126     C                   ENDDO
090500160520
090600160520     C*CARICO gli eventi IDD
090700970805     C                   MOVEL     '2A'          COD
090800970805     C                   MOVE      *ZEROS        §F                2 0
090900970805     C     KTAB1         SETLL     TABEL
091000160608     C     KTAB1         READE     TABEL
091100160608     c                   dow       not %eof(tabel00f)
091200160608
091300160608     C     TBLFLG        IFEQ      *BLANK                                        ANNUL.
091400970805     C                   MOVEL     TBLUNI        DS2A
091500970805     C     §2AIDD        IFNE      *BLANKS                                       NO TRASM.IN SEDE
091600970805     C                   ADD       1             §F
091700970805     C                   MOVEL     TBLKEY        IDD(§F)
091800970805     C                   ENDIF
091900970805     C                   ENDIF
092000160608     C     KTAB1         READE     TABEL
092100970805     C                   ENDDO
092200160520     C*                                                    *
092300160520     C* CARICO I CODICI CLIENTI A CUI MANDARE LE DATE DI CONSEGNA
092400160608     C                   MOVEL     '3K'          COD
092500160520     C                   MOVE      *ZEROS        W                 4 0
092600160520     C     Ktab1         SETLL     TABEL00F
092700160520     C     Ktab1         READE     TABEL00F                               31
092800160608     c                   dow       not %eof(tabel00f)
092900160608
093000160520     C                   MOVEL     TBLUNI        DS3K
093100160520     C                   ADD       1             W
093200160520     C                   MOVEL     TBLKEY        CLI(W)
093300160520      *
093400160608     C     Ktab1         READE     TABEL00F                               31
093500160608     C                   ENDdo
093600160608     c
093700941026     C                   CLOSE     TABEL00F
093800941025     C*
093900941025     C     KART          KLIST
094000941216     C                   KFLD                    ARBAAS
094100160527     C                   KFLD                    ARBLNP
094200941025     C                   KFLD                    ARBNRS
094300941025     C                   KFLD                    ARBNSP
094400160616     C     Kpbol         KLIST
094500160616     C                   KFLD                    kpAAS
094600160616     C                   KFLD                    kpLNP
094700160616     C                   KFLD                    kpNRS
094800160616     C                   KFLD                    kpNSP
094900160519     C     Kblt          KLIST
095000160519     C                   KFLD                    artAAS
095100160527     C                   KFLD                    artLNP
095200160519     C                   KFLD                    artNRS
095300160519     C                   KFLD                    artNSP
095400160519     C                   KFLD                    artNSC
095500970805     C     KEVB          KLIST
095600990803     C                   KFLD                    KAAS
095700970805     C                   KFLD                    KLNP
095800970805     C                   KFLD                    KNRS
095900970805     C                   KFLD                    KNSP
096000941025     C*
096100970805     C     KARBF         KLIST
096200970805     C                   KFLD                    LBLAAN
096300970805     C                   KFLD                    LBLLPN
096400970805     C                   KFLD                    LBLNRN
096500970805     C                   KFLD                    LBLNSN
096600970805     C     KARBM         KLIST
096700970805     C                   KFLD                    LBLAAP
096800970805     C                   KFLD                    LBLLPP
096900970805     C                   KFLD                    LBLNRP
097000970805     C                   KFLD                    LBLNSP
097100160526     C     Kblpo         KLIST
097200160526     C                   KFLD                    LBLAAo
097300160526     C                   KFLD                    LBLLPo
097400160526     C                   KFLD                    LBLNRo
097500160526     C                   KFLD                    LBLNSo
097600941025     C     KARB          KLIST
097700941025     C                   KFLD                    ARBAAS
097800941025     C                   KFLD                    ARBLNP
097900941025     C                   KFLD                    ARBNRS
098000941025     C                   KFLD                    ARBNSP
098100941025     C*
098200950126     C     KTAB1         KLIST
098300950126     C                   KFLD                    CODUT
098400950126     C                   KFLD                    COD
098500020920     C*
098600020920     C     Karb72        KLIST
098700020920     C                   KFLD                    kipp
098800020920     C                   KFLD                    knmi
098900160520     C     Kar4          KLIST
099000160520     C                   KFLD                    blpAAS
099100160520     C                   KFLD                    blpLNP
099200160520     C                   KFLD                    blpNRS
099300160520     C                   KFLD                    blpNSP
099400160520     C                   KFLD                    TRECz             1
099500160520     c                   move      'Z'           trecz
099600971120     C*
099700941025     C* DEFINIZIONE CHIAVI
099800941025     C*
099900941025     C     *LIKE         DEFINE    TBLCOD        COD
100000941216     C     *LIKE         DEFINE    ARBNMI        KNMI
100100020920     C     *LIKE         DEFINE    ARBipp        Kipp
100200990803     C     *LIKE         DEFINE    EVBAAS        KAAS
100300970805     C     *LIKE         DEFINE    EVBLNP        KLNP
100400970805     C     *LIKE         DEFINE    EVBNRS        KNRS
100500970805     C     *LIKE         DEFINE    EVBNSP        KNSP
100600020920     C                   Z-ADD     43            LUNG43           15 5
100700020920     C                   Z-ADD     48            LUNG48           15 5
100800970314     C*
100900941025     C                   ENDSR
101000941021     OFNARB000  E            AGGB0
101100941025     O                       ARBNCI
101200941025     O                       ARBNPI
101300930521     O                       ARBDCE
101400941025     O                       ARBNRC
101500930414     O                       ARBFT1
101600941025     O                       ARBDT1
101700941025     OFNARB000  E            AGGOR
101800941025     O                       ARBFT1
101900941025     O                       ARBDT1
102000160520     o
102100941021     OFNART000  E            AGGT0
102200961113     O                       ARTFTR
102300160616     OFNARB001  E            AGGB1
102400160616     O                       ARBNCI
102500160616     O                       ARBNPI
102600160616     O                       ARBDCE
102700160616     O                       ARBNRC
102800160616     O                       ARBFT1
102900160616     O                       ARBDT1
103000160616     OFNARB001  E            AGGOR1
103100160616     O                       ARBFT1
103200160616     O                       ARBDT1
103300010103**         CMDC
103400020920CHKOBJ OBJ(FIARBC46) OBJTYPE(*FILE) MBR(M000000)
103500911111**         CMDA
103600020920ADDPFM FILE(FIARBC46)               MBR(M000000)
103700911111**         CMDO
103800020920OVRDBF FILE(FIARBC46)               MBR(M000000)
103900950411**         CMDL
104000020920ALCOBJ OBJ((FIARBC46 *FILE *EXCL  M000000))
104100950411**         CMDD
104200020920DLCOBJ OBJ((FIARBC46 *FILE *EXCL  M000000))
