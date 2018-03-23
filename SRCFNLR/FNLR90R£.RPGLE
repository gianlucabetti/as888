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
001300161114     FFiART01L  UF   E           K DISK    prefix(new:3)
001400160519     FFNblt01L  UF   E           K DISK
001500160527     FFNblp01L  UF   E           K DISK
001600160520     FFiar401L  IF   E           K DISK
001700160616     Ffnarb01l  uF   E           K DISK    rename(fnarb000:fnarb001) usropn
001800941026     FTABEL00F  IF   E           K DISK    USROPN
001900970307     D*--------------------------------------------------------------*
002000160520     D CLI             S              7  0 DIM(9990)                            COD.CLI DA TRASM.
002100950126     D* CODICI BOLLE DA NON INVIARE IN SEDE
002200950126     D CDB             S              2    DIM(40)                              COD. BOLLE
002300970314     D* schiera x cariacmaneto codici eventi IDD
002400080905     D IDD             S              3    DIM(200)                             Cod.eventi IDD
002500950411     D* SCHIERE X QCMDEXEC
002600160520     D CMDC            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC CHKOBJ
002700160520     D CMDA            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC ADDPFM
002800160520     D CMDO            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC OVRDBF
002900020920     D CMDL            S             43    DIM(1) CTDATA PERRCD(1)              QCAEXEC ALCOBJ
003000020920     D CMDD            S             43    DIM(1) CTDATA PERRCD(1)              QCAEXEC DLCOBJ
003100160511     d savflp          s                   like(arbflp)
003200970307     D*--------------------------------------------------------------*
003300970307     D*  DS
003400970307     D*--------------------------------------------------------------*
003500911014     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003600160520     d
003700941025     D WLBDA8          DS
003800941025     D  G08DAT                 1      8  0
003900941025     D  G08INV                 9     16  0
004000941025     D  G08ERR                17     17
004100941025     D  G08TGI                18     22  0
004200160520     d
004300160527     D DSCMZ         E DS
004400970805     D DS2A          E DS
004500950126     D DS3A          E DS
004600160520     D DS3K          E DS
004700911014     D CNCR80        E DS
004800970925     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
004900911014     D KPJBA         E DS
005000911014     D  LIBSYS                92    101
005100911112      *
005200020920     D TIBS50        E DS                  EXTNAME(TIBS55ds)
005300161115     D fnlru39ds     E DS
005400161114     d
005500161114     d n_fiartds     e ds                  extname(fiart00F) prefix(new:3)
005600160520
005700971120     D PARAM5          DS
005800911015     D  LEGFIL                 1     10
005900020925     D  MBRFIL                11     20
006000160520
006100020920     D PARAM7a         DS
006200020920     D  nmi7a                  1      7
006300020920     D  fgs7a                  8     10
006400160616     d
006500160616     D PARAM7b         DS
006600160616     D  pa7nsped               1     16
006700160616     D  pa7rich               17     17
006800160616     D  pa7err                18     18
006900160520
007000020920     d   §membro       ds
007100020920     d  §emme                  1      1    inz('M')
007200020925     d  xfle                   2      4s 0
007300020920     d  §sed                   5      7s 0 inz(046)
007400020920     d  §mbr                   2      7
007500160616     d
007600160616     d  kpaas          s                   like(arbaas)
007700160616     d  kplnp          s                   like(arblnp)
007800160616     d  kpnrs          s                   like(arbnrs)
007900160616     d  kpnsp          s                   like(arbnsp)
008000160616     d  Singola        s              1
008100161115     d  figl_flp       s                   like(newflp)
008200161115     d  figl_det       s                   like(newdet)
008300161115     d  figl_dut       s                   like(newdut)
008400161115     d  figl_pet       s                   like(newpet)
008500161115     d  figl_put       s                   like(newput)
008600161115     d  figl_flnap     s                   like(newflnap)
008700161115     d  figl_dam       s                   like(newdam)
008800161115     d  figl_dia       s                   like(newdia)
008900161115     d  figl_nap       s                   like(newnap)
009000161115     d  figl_ham       s                   like(newham)
009100161115     d  figl_damf      s                   like(newdamf)
009200161115     d  figl_diaf      s                   like(newdiaf)
009300161115     d  figl_hamf      s                   like(newhamf)
009400161115     d  figl_napf      s                   like(newnapf)
009500161115     d  figl_cca       s                   like(newcca)
009600161115     d  figl_dcm       s                   like(newdcm)
009700161115     d  figl_hcm       s                   like(newhcm)
009800911014     C*---------------------------------------------------------------*
009900941216     C* PREPARAZIONE ARCHIVIO DATI CONSEGNA  + DETTAGLIO SEGNACOLLI
010000160616     c
010100160616     c                   select
010200160616     c* Elaboro una bolla soltanto
010300160616    1c                   when      %parms=2 and kpnsp>0
010400160616     c                   exsr      Ela_bolla
010500160616    1c
010600950102     C* TRASMETTO SOLO SE HO MANDATO INTROITO IMPOSTATO
010700160616    1C                   when      KNMI>0
010800930421     C                   EXSR      ARBCSR
010900941025     C*
011000941025      * RILEGGO X TRASMETTERE ALLA BOLLA ORIGINALE
011100941025     C                   EXSR      ARBORI
011200160616     c                   endsl
011300160616
011400941025     C*
011500930421     C* I N V I O  A SEDE
011600930421     C*
011700160616    1c                   if        %open(fiarbc46)
011800010103     C                   CLOSE     FIARBC46
011900160616
012000160617    2C***  NRSED         IFGT      *ZEROS
012100160617     c
012200160617     c                   if        KNMI>0
012300911111     C                   EXSR      INVSED
012400160617    2C                   ENDif
012500010103      * Disalloco FIARBC46
012600020925     C                   MOVEL(p)  CMDD(1)       COMMAN
012700020925     c                   eval      %subst(comman:36:6) = §mbr
012800950411     C                   CALL      'QCMDEXC'                            60
012900950411     C                   PARM                    COMMAN
013000020920     C                   PARM                    LUNG43
013100950411     C*
013200160616    1C                   ENDIF
013300160616
013400930415      *
013500950411     C     FINE          TAG
013600160520     C*
013700971120     C                   CLEAR                   TIBS50
013800971121     C                   MOVEL     'C'           I50TLA
013900020920     C                   CALL      'TIBS55R'
014000971120     C                   PARM                    TIBS50
014100161116     c
014200161116     c                   clear                   fnlru39ds
014300161116     c                   movel     'C'           i38tla
014400161116     c                   call      'FNLRU39R'
014500161116     c                   parm                    fnlru39ds
014600971120     C*
014700911015     C                   SETON                                        LR
014800930421     C***--------------------------------------------***
014900930421     C* PREPARAZIONE ARCHIVIO DATI CONSEGNA
015000930421     C***--------------------------------------------***
015100930421     C     ARBCSR        BEGSR
015200930421     C                   SETOFF                                       10
015300160616     c
015400020920     C     karb72        SETLL     FNARB72L
015500930421     C*
015600930421     C* APRO FILE X LA SEDE
015700930421     C                   EXSR      CAEX46
015800930421     C*
015900160519    1C     *IN10         DOUEQ     '1'
016000020920     C     karb72        READE     FNARB72L                             1510
016100930421     C  N15
016200160519    2CANN10              DO
016300160616     c
016400160616     c                   exsr      EventiB
016500160616      *
016600941024     C                   MOVEL     *BLANKS       ARBFAG
016700160527     C                   MOVEL     *BLANK        HAFIGL            1
016800160527     C                   MOVEL     *BLANK        HAMAMM            1
016900160527     C                   MOVEL     *BLANK        HAORIG            1
017000160527     C                   MOVEL     *BLANK        ARBFAG
017100160519      *
017200160527      * elaboro la bolla per :
017300160527      * - consegna totale
017400160527      * - consegna parziale
017500160527      * - evento IDD
017600160519    3C     ARBDCM        IFGT      *ZEROS
017700930524     C     ARBDCP        ORGT      *ZEROS
017800970805     C     WTREVE        OREQ      'S'
017900160527     c
018000160616     c                   exsr      Sd99_lbl
018100160616
018200160616     c                   exsr      Agg_ART
018300160616
018400160616     c                   exsr      Agg_ARBBLP
018500930524      *
018600160616    3C                   endif
018700160616    2C                   ENDDO
018800160616    1C                   ENDDO
018900930421      *
019000930421     C                   ENDSR
019100160616      * ----------------------------------------------------  EVENTO IDD
019200160616     c     EventiB       BEGSR
019300160616      * ----------------------------------------------------  EVENTO IDD
019400160616      * Controllo se sulla bolla esiste evento di IDD
019500160616     C                   MOVEL     ' '           WTREVE            1
019600160616    3C     ARBCCA        IFNE      '5'
019700160616     C                   Z-ADD     ARBAAS        KAAS
019800160616     C                   Z-ADD     ARBLNP        KLNP
019900160616     C                   Z-ADD     ARBNRS        KNRS
020000160616     C                   Z-ADD     ARBNSP        KNSP
020100160616     C* Loop su eventi fino a fine o trovato evento
020200160616     C     KEVB          SETGT     FNEVB01L
020300160616     C     KEVB          READPE    FNEVB01L                               34
020400160616    4C     *IN34         DOWEQ     '0'
020500160616     C                   Z-ADD     1             W
020600160616     C     EVBCEV        LOOKUP    IDD(W)                                 34
020700160616    5C     *IN34         IFEQ      '1'
020800160616     C                   MOVEL     'S'           WTREVE
020900160616     C                   ELSE
021000160616     C     KEVB          READPE    FNEVB01L                               34
021100160616    5C                   END
021200160616    4C                   ENDdo
021300160616     C*
021400160616    3C                   ENDif
021500160616     c                   ENDSR
021600160616      * -------------------------------------------------------------------
021700160616     c     SD99_lbl      BEGSR
021800160616      * --------------------------------------------------------------------
021900160616      * SE SPEDIZIONE CONSEGNATA CALCOLO TEMPI DELIVERY
022000160616     C                   MOVE      *ZEROS        ARBNCI
022100160616     C                   MOVE      *ZEROS        ARBNPI
022200160616     C                   MOVE      *ZEROS        ARBDCE
022300160616     C                   MOVE      *ZEROS        ARBNRC
022400160616     c                   clear                   tnsd99
022500160616     C                   MOVEL     *BLANKS       ARBFAG
022600160616     C                   MOVEL     *BLANK        HAFIGL            1
022700160616     C                   MOVEL     *BLANK        HAMAMM            1
022800160616     C                   MOVEL     *BLANK        HAORIG            1
022900160616     C                   MOVEL     *BLANK        ARBFAG
023000160616      *
023100160616      * in caso di consegna parziale, non mi serve verificare i legami bolla
023200160616      * e nemmeno l'affidabilità
023300160616    4C     ARBDCM        IFGT      *ZEROS
023400160616     C     ARBCCA        OREQ      '5'
023500160616     C     WTREVE        OREQ      'S'
023600160616      * GUARDO COSA DEVO TRASMETTERE ALLA PARTENZA SE CI SONO LEGAMI
023700160616      * - LA BOLLA HA UNA FIGLIA CON LNA DIVERSA (DIROTT./RESO)
023800160616     C                   MOVEL     *BLANK        HAFIGL            1
023900160616     C                   MOVEL     *BLANK        HAMAMM            1
024000160616     C                   MOVEL     *BLANK        HAORIG            1
024100160616     C                   MOVEL     *BLANK        ARBFAG
024200160616     C     KARB          SETGT     FNLBL02L
024300160616     C     KARB          READPE    FNLBL02L                               22
024400160616    5C     *IN22         DOWEQ     '0'
024500160616     C     ARBLNA        IFNE      LBLLAN
024600160616     C                   MOVEL     'S'           HAFIGL
024700160616     C                   ELSE
024800160616     C* Se la bolla ha una figlia con la stessa lna devo controllare
024900160616     C* se quest'ultima è chiusa con IDD
025000160616     c* ES 26/05: non capendo il significato di queste specifiche
025100160616     c*  le asterisco. non ho trovato il caso di bolla figlia con stessa LNA
025200160616     c*  e con dettaglio colli per leggerlo per aggiornare la mamma.
025300160616     c*  il cambio di porto, unico caso che ci è venuto in mente, non ha dettaglio colli
025400160616     C****               MOVEL     'S'           WCTIDD
025500160616     C                   END
025600160616     c
025700160616     C     KARB          READE     FNLBL02L                               22
025800160616    5C                   ENDDO
025900160616      *
026000160616      * - LA BOLLA HA UNA MAMMA + GUARDO LNA BOLLA ORIGINALE
026100160616     C     KARB          CHAIN     FNLBL01L                           22
026200160616    5C     *IN22         IFEQ      '0'
026300160616     C                   MOVEL     'S'           HAMAMM
026400160616     C     LBLLAO        IFNE      LBLLAP
026500160616     C     LBLLAO        ORNE      LBLLAN
026600160616     C                   MOVEL     'S'           HAORIG
026700160616     C                   END
026800160616     c* occorre trasmettere la data consegna anche alla bolla originale
026900160616     c* se la figlia è un reso con stessa linea arrivo
027000160616     C     LBLLAO        IFeq      LBLLAP
027100160616     C     LBLLAO        andeq     LBLLAN
027200160616     c     arbfbr        andeq     'S'
027300160616     C                   MOVEL     'S'           HAORIG
027400160616     c                   end
027500160616    5C                   ENDIF
027600160616      * SE HA FIGLIA ALTRA FIL.MA NON HA MAMMA è LA 1°
027700160616      * NON TRASMETTO DATA CONSEGNA
027800160616    5C     HAFIGL        IFEQ      'S'
027900160616     C     HAMAMM        ANDEQ     ' '
028000160616     C     ARBCCA        ANDNE     '2'
028100160616     C     ARBCCA        ANDNE     '6'
028200160616     C                   MOVEL     'D'           ARBFAG                         BOLLA DIROTTATA
028300160616    5C                   END
028400160616      *-----------------------------------------------------*
028500160616      * CALCOLO TEMPI DELIVERY
028600160616     C     ARBDCM        IFGT      0
028700160616     C                   MOVEL     ' '           D98TLA
028800160616     C                   MOVEL     'A'           D98TBO
028900160616     C                   MOVE      ARBAAS        D98AAS
029000160616     C                   MOVE      ARBLNP        D98LNP
029100160616     C                   MOVE      ARBNRS        D98NRS
029200160616     C                   MOVE      ARBNSP        D98NSP
029300160616     C                   CALL      'TNSD99R'
029400160616     C                   PARM                    TNSD99
029500160616     C                   END
029600160616    4C                   ENDif
029700160616     c
029800160616     c                   ENDSR
029900160616      *-----------------------------------------------------*
030000160616     c     Agg_art       BEGSR
030100160616      *-----------------------------------------------------*
030200160616      * ORA AGGIORNO PER OGNI COLLO DI ART FNBLT, IGNORANDO GLI SFLAGGATI MA
030300160616      *  AGGIORNANDO TUTTI I RECORD
030400160616     c                   setoff                                         11
030500160616     C     KART          SETLL     FNART01L
030600160616     C     KART          READE     FNART01L                             1511
030700160616     C                   DOW       NOT *IN15 AND NOT *IN11
030800160616     c                   exsr      Agg_blt
030900160616     c* se sflaggato,
031000160616     C*  FLEGGO COME TRASMESSI SEGNACOLLI SE NO BOLLA LEGATA
031100160616     c* ( solo quelli slafaggati, ma comunque li aggiorno tutti )
031200160616     c                   if        artftr=' ' and (haorig<>'S' or
031300160616     c                             hafigl<>' ')
031400160616     C                   MOVE      'T'           ARTFTR
031500160616     C                   EXCEPT    AGGT0
031600160616     C                   endif
031700160616
031800160616     C     KART          READE     FNART01L                             1511
031900160616     c                   enddo
032000160616     c                   ENDSR
032100160616      *-----------------------------------------------------*
032200160616     c     Agg_ARBBLP    BEGSR
032300160616      *-----------------------------------------------------*
032400160616     c***
032500160616     c* AGGIORNAMENTO FNBLP / FNARB --> solo nessun errore in lettura segnacolli
032600160616   4ac                   if        not *in15
032700160616     c* imposto tempi di delivery
032800160616     C                   Z-ADD     D98NCI        ARBNCI                         NON CONF.CONS. INT.
032900160616     C                   Z-ADD     D98NPI        ARBNPI                         NON CONF.PART. INT.
033000160616     C                   Z-ADD     D98DCE        ARBDCE                         DATA CONS. PREV.CLI.
033100160616     C                   Z-ADD     D98NRC        ARBNRC                         NON CONF. RESA CLI.
033200160616      *
033300160616    5C     ARBDCM        IFGT      *ZEROS
033400160616     C     ARBDCP        ORGT      *ZEROS
033500160616     c                   exsr      Agg_BLP
033600160616    5C                   END
033700160616     c***
033800160616      * A SEDE SOLO SE CONSEGNATE
033900160616     c***
034000160616    5C     ARBDCM        IFGT      0
034100160616     C* CONTROLLO SE LA BOLLA NON è DA INVIARE IN SEDE
034200160616     C                   Z-ADD     1             §F
034300160616     C                   SETOFF                                       03
034400160616      * BOLLE DA NON INVIARE
034500160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
034600160616    6c                   if        not *in03
034700160616     c* utilizzo il campo ARBFLP (fil.transito) x aggiornare la zona in sede
034800160616     c                   eval      savflp = arbflp
034900160616     c                   eval      arbflp = arbznc
035000160617     C****               ADD       1             NRSED             7 0
035100160616     C                   WRITE     FIARBC00                                     SEDE
035200160616     c                   eval      arbflp = savflp
035300160616    6C                   ENDIF
035400160616    5C                   ENDIF
035500160616      *
035600160616      * HA BOLLA ORIG.CON LINEA ARRIVO DIVERSA E NON HA FIGLIA CON
035700160616      * LNA DIVERSA : è L'ULTIMA BOLLA DELLA CATENA QUINDI DEVO TRASM.
035800160616      * ALLA BOLLA ORIGIN. LA DATA CONSEGNA , NON AGGIORNO ARBDT1 E
035900160616      * IMPOSTO FT1=O X RILEGGERLO SUCCESSIVAMENTE
036000160616    5C     HAORIG        IFEQ      'S'
036100160616     C     HAFIGL        ANDEQ     ' '
036200160616     C                   MOVEL     'O'           ARBFT1
036300160616     C                   MOVE      *ZEROS        ARBDT1
036400160616     C                   ELSE
036500160616     C                   MOVE      'T'           ARBFT1
036600160616     C                   Z-ADD     AMGUDT        ARBDT1
036700160616    5C                   ENDIF
036800160616      *
036900160616    5C     ARBDCM        IFGT      0
037000160616     C*                  MOVE      'T'           ARBFT2
037100160616     C*                  Z-ADD     AMGUDT        ARBDT2
037200160616    5C                   ENDIF
037300160616      *
037400160616     c                   if        singola=' '
037500160616     C                   EXCEPT    AGGB0
037600160616     c                   else
037700160616     C                   EXCEPT    AGGB1
037800160616     c                   endif
037900160616     c
038000160616   4aC                   endif
038100160616     c                   ENDSR
038200160519     C**-------------------------------------------------------------**
038300160519     C** Aggiorna dettaglio colli partenza
038400160519     C**-------------------------------------------------------------**
038500160519     c     Agg_blt       BEGSR
038600160527     c* Lettura sequenziale per aggiornamento dett colli BOLLA ORIGINALE
038700160527     c                   if        arbfag='O'
038800160527     c
038900160527     c                   if        primalet=' '
039000160527     c     kblpo         setll     fnblt01l
039100160527     c     kblpo         reade     fnblt01l
039200160527     c                   eval      primalet='1'
039300160527     c                   else
039400160527     c     kblpo         reade     fnblt01l
039500160527     c                   endif
039600160527     c
039700160527     c                   else
039800160527     c*
039900160527     c* lettura per segnacollo negli altri casi
040000160519     c     kblt          chain     fnblt01l
040100160527     c                   endif
040200160527     c
040300160519     c                   eval      bltfl1=artfl1
040400160526     c                   if        arbfag<>'D'
040500160519     c                   eval      bltdcm=artdcm
040600160526     c                   endif
040700160519     c                   eval      bltdet=artdet
040800160519     c                   eval      bltdut=artdut
040900160519     c                   eval      bltdam=artdam
041000160520     c                   eval      bltflp=artflp
041100160520     C     BLTFTR        IFNE      'C'
041200160520     C                   MOVEL     'R'           BLTFTR
041300160520     C                   MOVEL     AMGUDT        BLTDTR
041400160520     C                   END
041500160527     c                   if        arbfag='O' and not %eof(fnblt01l)
041600160527     c                   update    fnblt000
041700170322     c
041800170322     c* Non aggiorno fiart unico per RESO
041900170322     c                   if        blpcca<>'2' and blpcca<>'6'
042000161115     c                   exsr      Agg_ARTuni
042100160527     c                   endif
042200170322     c                   endif
042300160527     c
042400160527     c                   if        arbfag<>'O' and %found(fnblt01l)
042500160519     c                   update    fnblt000
042600161114     c
042700161114     c                   exsr      Agg_ARTuni
042800161114     c*
042900160519     c                   endif
043000160527
043100160519     c                   ENDSR
043200161114     c
043300161114     C**-------------------------------------------------------------**
043400161114     c**  Aggiorno FIART00f UNICO
043500161114     C**-------------------------------------------------------------**
043600161114     c     Agg_artuni    BEGSR
043700161114     C* SE STO AGGIORNANDO I COLLI della bolla figlia, devo
043800161114     c* leggere il FIART unico della figlia e memorizzarmi i campi che andranno
043900161114     c*  riaggiornati sulla bolla originale
044000161114     c                   if        arbfag='O'
044100161115     c     kblt          chain     fiart01l
044200161114     c                   if        %found(fiart01l)
044300161115     c* memorizzo i campi che mi servono
044400161115     c                   eval      figl_flp=newflp
044500161115     c                   eval      figl_det=newdet
044600161115     c                   eval      figl_pet=newpet
044700161115     c                   eval      figl_dut=newdut
044800161115     c                   eval      figl_put=newput
044900161115     c                   eval      figl_flnap=newFLNAP
045000161115     c                   eval      figl_dam=newdam
045100161115     c                   eval      figl_dia=newdia
045200161115     c                   eval      figl_nap=newnap
045300161115     c                   eval      figl_ham=newham
045400161115     c                   eval      figl_damf=newdamf
045500161115     c                   eval      figl_diaf=newdiaf
045600161115     c                   eval      figl_hamf=newhamf
045700161115     c                   eval      figl_napf=newnapf
045800161115     c                   eval      figl_cca =newcca
045900161115     c                   eval      figl_dcm =newdcm
046000161115     c                   eval      figl_hcm =newhcm
046100161114     c                   endif
046200161114     c                   endif
046300161114
046400161114     c* Aggiorno anche fiart unico
046500161115     c     kbltO         chain     fiart01l
046600161114     c                   if        %found(fiart01l)
046700161114     c                   clear                   fnlru39ds
046800161114     c                   clear                   n_fiartds
046900161114     c                   eval      i38aas=bltaas
047000161114     c                   eval      i38lnp=bltlnp
047100161114     c                   eval      i38nrs=bltnrs
047200161114     c                   eval      i38nsp=bltnsp
047300161114     c                   eval      i38nsc=bltnsc
047400161114     c                   call      'FNLRU39R'
047500161114     c                   parm                    fnlru39ds
047600161114     c                   parm                    n_fiartds
047700161115     c
047800161114     c                   if        arbfag='O'
047900161115     c                   eval      newflp=figl_flp
048000161115     c                   eval      newdet=figl_det
048100161115     c                   eval      newpet=figl_pet
048200161115     c                   eval      newdut=figl_dut
048300161115     c                   eval      newput=figl_put
048400161115     c                   eval      newflnap=figl_FLNAP
048500161115     c                   eval      newdam=figl_dam
048600161115     c                   eval      newdia=figl_dia
048700161115     c                   eval      newnap=figl_nap
048800161115     c                   eval      newham=figl_ham
048900161115     c                   eval      newdamf=figl_damf
049000161115     c                   eval      newdiaf=figl_diaf
049100161115     c                   eval      newhamf=figl_hamf
049200161115     c                   eval      newnapf=figl_napf
049300161115     c                   eval      newcca =figl_cca
049400161115     c                   eval      newdcm =figl_dcm
049500161115     c                   eval      newhcm =figl_hcm
049600161115     c                   endif
049700161115     c
049800161114     c                   update    fiart000
049900161114     c                   endif
050000161114     c                   ENDSR
050100160519     C**-------------------------------------------------------------**
050200160519     C** Aggiorna bolla partenza
050300160519     C**-------------------------------------------------------------**
050400160519     c     Agg_blp       BEGSR
050500160526     c     karb          chain     fnblp01l
050600160519     c                   if        %found(fnblp01l)
050700160520     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
050800160520     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
050900160520     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
051000160520     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
051100160520     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
051200160520     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
051300160520     C     ARBFLP        IFGT      0
051400160520     C                   MOVE      ARBFLP        BLPFLP                         FILIALE TRANSITO
051500160520     C                   ENDIF
051600160520     C     BLPDET        IFEQ      0
051700160520     C                   MOVE      ARBDET        BLPDET
051800160520     C                   ENDIF
051900160520     C     BLPDAM        IFEQ      0
052000160520     C                   MOVE      ARBDAM        BLPDAM
052100160520     C                   ENDIF
052200160520     C     BLPDUT        IFEQ      0
052300160520     C                   MOVE      ARBDUT        BLPDUT
052400160520     C                   ENDIF
052500160520     C                   MOVE      ARBNDC        BLPNDC                         NR.DIST.CONS
052600160520     C                   MOVE      ARBDDC        BLPDDC                         DATA "  CONS
052700160520     C     BLPCCA        IFNE      '5'
052800160520     C                   MOVE      ARBCCA        BLPCCA                         T.MANC.CONS.
052900160520     C                   END
053000160520     C     BLPCCA        IFNE      '5'
053100160520     C     BLPDCP        OREQ      0
053200160520     C                   MOVE      ARBDCP        BLPDCP                         DATA 1° CONS
053300160520     C                   END
053400160520     C                   MOVE      ARBTMC        BLPTMC                         T.MANC.CONS.
053500160520     c
053600160520     c                   exsr      DATCLI
053700160520     C*
053800160520     c* se bolla dirottata non aggiorno la data e ora consegna merce
053900160520     c*  dovrò aggiornare con la bolla figlia
054000160520     c                   if        arbfag<>'D'
054100160519     c                   eval      blpdcm=arbdcm
054200160519     c                   eval      blphmc=arbhmc
054300160520     c                   endif
054400160520     c
054500160519     c                   update    fnblp000
054600160519     c                   endif
054700160519     c                   ENDSR
054800930421     C**-------------------------------------------------------------**
054900010103     C** QCAEXEC SUL MBR DI FIARBC46 PER LA SEDE
055000930421     C**-------------------------------------------------------------**
055100930421     C     CAEX46        BEGSR
055200930421     C** FILIALE PER APERTURA MEMBRO
055300930421     C** CONTROLLO SE ESISTE IL MEMBRO
055400930421     C                   SETOFF                                       60
055500160520     C                   MOVEL(P)  CMDC(1)       COMMAN           80
055600020920     c                   eval      %subst(comman:42:6) = §mbr
055700930421     C                   CALL      'QCMDEXC'                            60
055800930421     C                   PARM                    COMMAN
055900020920     C                   PARM                    LUNG48
056000930421     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
056100930421     C   60              DO
056200160520     C                   MOVEL(p)  CMDA(1)       COMMAN
056300020920     c                   eval      %subst(comman:42:6) = §mbr
056400930421     C                   CALL      'QCMDEXC'
056500930421     C                   PARM                    COMMAN
056600020920     C                   PARM                    LUNG48
056700930421     C                   END
056800930421     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
056900160520     C                   MOVEL(P)  CMDO(1)       COMMAN
057000020920     c                   eval      %subst(comman:42:6) = §mbr
057100930421     C                   CALL      'QCMDEXC'
057200930421     C                   PARM                    COMMAN
057300020920     C                   PARM                    LUNG48
057400010103     C* Provo ad allocare FIARBC46 X 3 volte
057500950411     C                   MOVEL     'N'           WALLOC            1
057600950411     C                   Z-ADD     1             XX                2 0
057700020920     C                   MOVEL(p)  CMDL(1)       COMMAN
057800020920     c                   eval      %subst(comman:36:6) = §mbr
057900950412     C     WALLOC        DOWEQ     'N'
058000950411     C     XX            ANDLE     3
058100950411     C                   CALL      'QCMDEXC'                            60
058200950411     C                   PARM                    COMMAN
058300020920     C                   PARM                    LUNG43
058400950411     C     *IN60         IFEQ      '0'
058500950411     C                   MOVEL     'S'           WALLOC                         Allocazione O.K.
058600950411     C                   END
058700950411     C                   ADD       1             XX
058800950411     C                   END
058900950411     C*  Se non sono riuscita ad allocarlo vado a fine pgm. e
059000950411     C*  risottometto in coda la trasmissione date di consegna
059100950411     C     WALLOC        IFEQ      'N'
059200160616     c                   if        singola=' '
059300020920     C                   MOVEL(p)  param7a       KPJBU
059400950411     C                   MOVEL     'LR90'        KCOAZ
059500950411     C                   CALL      'BCH10'
059600950411     C                   PARM                    KPJBA
059700950411     C                   GOTO      FINE
059800160616     c                   else
059900160616     c                   eval      pa7err='E'
060000160616     c                   endif
060100160616     c
060200950411     C                   END
060300950411     C*
060400010103     C                   OPEN      FIARBC46
060500160616     c
060600930421     C                   ENDSR
060700941025     C***--------------------------------------------***
060800941025     C* TRASMISS. SOLO DATA CONSEGNA ALLA BOLLA ORIGINALE
060900941025     C***--------------------------------------------***
061000941025     C     ARBORI        BEGSR
061100160527     C                   SETOFF                                       10
061200160527     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
061300160527     c*
061400020920     C     karb72        SETLL     FNARB72L
061500941025     C*
061600160520    1C     *IN10         DOUEQ     '1'
061700020920     C     Karb72        READE     FNARB72L                             1510
061800941025     C  N15
061900160520    2CANN10ARBFT1        IFEQ      'O'
062000941025     C     KARB          CHAIN     FNLBL01L                           22
062100160526     c                   clear                   primalet          1
062200160520      *
062300941025      * TRASMETTO SE C'E' DATA CONSEGNA
062400160520    3C     ARBDCM        IFGT      *ZEROS
062500970805     C     ARBCCA        OREQ      '5'
062600170322
062700170322     c* anticipo la chian per avere la consegna anomala
062800170322     c* in caso di reso infatti NON devo aggioranre il FIART unico
062900170322     c     kblpo         chain     fnblp01l
063000170322     c                   if        not %found(fnblp01l)
063100170322     c                   clear                   blpcca
063200170322     c                   endif
063300160527     C
063400160527     c* Leggo il dettaglio colli della bolla figlia
063500160527     c*  e aggiorno il detto collo della bolla mamma
063600160527     c                   setoff                                         11
063700160527     C     KART          SETLL     FNART01L
063800160527     C     KART          READE     FNART01L                             1511
063900160527    4C                   DOW       NOT *IN15 AND NOT *IN11
064000160527     c                   exsr      Agg_blt
064100160527
064200160527     c                   if        artftr=' '
064300160527     C                   MOVE      'T'           ARTFTR
064400160527     C                   EXCEPT    AGGT0
064500160527     C                   endif
064600160527     C     KART          READE     FNART01L                             1511
064700160527    4c                   enddo
064800970306     C*
064900970313      * TRASMETTO TESTATA SE C'E' DATA CONSEGNA
065000160527      * se ART allocato non aggiorno la testata e aspetto il prossimo giro
065100160520    4C     ARBDCM        IFGT      *ZEROS
065200160527     c     *in15         andeq     *off
065300160527     c
065400170322     c***  kblpo         chain     fnblp01l
065500160527    5c                   if        %found(fnblp01l)
065600160526     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
065700160526     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
065800160526     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
065900160526     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
066000160526     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
066100160526     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
066200160527    6C     ARBFLP        IFGT      0
066300160526     C                   MOVE      ARBFLP        BLPFLP                         FILIALE TRANSITO
066400160527    6C                   ENDIF
066500160526     C                   MOVE      ARBDET        BLPDET
066600160526     C                   MOVE      ARBDUT        BLPDUT
066700160526     C                   MOVE      ARBdam        BLPdam
066800160526     C* X BOLLA ORIGINALE AGGIORNO TMC CON FLAG CONSEGNA ANOMALA 5
066900160526     C                   MOVE      ARBNDC        BLPNDC                         NR.DIST.CONS
067000160526     C                   MOVE      ARBDDC        BLPDDC                         DATA "  CONS
067100160527    6C     BLPCCA        IFNE      '5'
067200160526     C     BLPDCP        OREQ      0
067300160526     C                   MOVE      ARBDCP        BLPDCP                         DATA 1° CONS
067400160527    6C                   END
067500160526     C                   MOVE      ARBTMC        BLPTMC                         T.MANC.CONS.
067600160526     C                   MOVE      ARBDCM        BLPDCM
067700160526     C                   MOVE      ARBHMC        BLPHMC
067800160526      * Controllo se trasmettere dati cliente
067900160526     C                   EXSR      DATCLI
068000160526     c
068100160526     c                   update    fnblp000
068200160527    5c                   endif
068300160526     c
068400160526     C                   MOVE      LBLLPO        ARBLNP
068500970313     C                   MOVE      LBLAAO        ARBAAS
068600970313     C                   MOVE      LBLNRO        ARBNRS
068700970313     C                   MOVE      LBLNSO        ARBNSP
068800160527     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
068900970313     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
069000950330      * SE NON TRASMETTO A SEDE LA BOLLA FIGLIA NON TRASMETTO
069100950330      * NEANCHE LA BOLLA ORIGINALE
069200950330     C                   Z-ADD     1             §F
069300950330     C                   SETOFF                                       03
069400950330      * BOLLE DA NON INVIARE
069500950330     C     ARBCBO        LOOKUP    CDB(§F)                                03
069600160617     C**N03              ADD       1             NRSED             7 0
069700010103     C  N03              WRITE     FIARBC00                                     SEDE
069800170322     c
069900170322   x4c                   else
070000170322     c                   unlock    fnblp01l
070100160520    4C                   END
070200941025     C*
070300941025     C                   MOVEL     'T'           ARBFT1
070400941025     C                   MOVE      AMGUDT        ARBDT1
070500160520    3C                   ENDif
070600160520     c
070700170322     c  n15              if        singola=' '
070800941025     C                   EXCEPT    AGGOR
070900160616     c                   else
071000160616     C                   EXCEPT    AGGOR1
071100160616     c                   endif
071200160616     c
071300160520    2C                   ENDif
071400160520    1C                   ENDdo
071500941025      *
071600941025     C                   ENDSR
071700160616     C***--------------------------------------------***
071800160616     C* Elaboro soltanto la bolla passata
071900160616     C***--------------------------------------------***
072000160616     c     Ela_bolla     BEGSR
072100160616
072200160616     c                   eval      singola='S'
072300160616     c
072400160616    1c                   if        not %open(fnarb01l)
072500160616     c                   open      fnarb01l
072600160616    1c                   endif
072700160616     c
072800160616     c     kpbol         chain(e)  fnarb01l
072900160616    1c                   if        %error or not %found(fnarb01l)
073000160616     c                   eval      pa7err='E'
073100160616     c                   leavesr
073200160616    1c                   endif
073300160616     c
073400160616     C* APRO FILE X LA SEDE
073500160616     C                   EXSR      CAEX46
073600160616     c
073700160616    1c                   if        pa7rich<>'C'
073800160616     c                   exsr      EventiB
073900160616    1c                   endif
074000160616     c
074100160616     c* se tipo richiamo = 'C' --> solo ricalcolo affidabilità solo se consegnata
074200160616     c*                            altrimenti in tutti i casi previsti
074300160616
074400160616    0c                   if        (pa7rich='C' and arbdcm>0) or
074500160616     c                             (pa7rich<>'C' and (arbdcm>0 or arbdcp>0
074600160616     c                             or wtreve='S'))
074700160616     c
074800160616     c                   exsr      SD99_LBL
074900160616     c
075000160616    1c                   if        pa7rich<>'C'
075100160616     c                   exsr      Agg_ART
075200160616
075300160616     c                   exsr      Agg_ARBBLP
075400160616     c
075500160616   x1c                   else
075600160616     c
075700160616    2C     D98DCE        IFNE      ARBDCE
075800160616     C     D98NPI        ORNE      ARBNPI
075900160616     C     D98NCI        ORNE      ARBNCI
076000160616     C     D98NRC        ORNE      ARBNRC
076100160616     C                   Z-ADD     D98DCE        ARBDCE
076200160616     C                   Z-ADD     D98NPI        ARBNPI
076300160616     C                   Z-ADD     D98NCI        ARBNCI
076400160616     C                   Z-ADD     D98NRC        ARBNRC
076500160616     C                   MOVEL     'C'           ARBFAG
076600160616     C                   Z-ADD     1             §F
076700160616     C                   SETOFF                                       03
076800160616      * BOLLE DA NON INVIARE
076900160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
077000160616    3c                   if        not *in03
077100160617     C***                ADD       1             NRSED             7 0
077200160616     C                   WRITE     FIARBC00                                     SEDE
077300160616    3C                   ENDIF
077400160616     c
077500160616     c     karb          chain     fnblp01l
077600160616    3c                   if        %found(fnblp01l)
077700160616     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
077800160616     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
077900160616     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
078000160616     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
078100160616     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
078200160616     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
078300160616     c                   update    fnblp000
078400160616    3c                   endif
078500160616
078600160616     c                   update    fnarb001
078700160616
078800160616     c* Se la bolla ha la mamma --> aggiorno anche la bolla originale
078900160616    3c                   if        haorig='S' and hafigl=' '
079000160616     C     KARB          CHAIN     FNLBL01L
079100160616    4c                   if        %found(fnlbl01l)
079200160616     c     kblpo         chain     fnblp01l
079300160616    5c                   if        %found(fnblp01l)
079400160616     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
079500160616     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
079600160616     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
079700160616     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
079800160616     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
079900160616     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
080000160616     c                   update    fnblp000
080100160616    5c                   endif
080200160616     c
080300160616     C                   MOVE      LBLLPO        ARBLNP
080400160616     C                   MOVE      LBLAAO        ARBAAS
080500160616     C                   MOVE      LBLNRO        ARBNRS
080600160616     C                   MOVE      LBLNSO        ARBNSP
080700160616     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
080800160616     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
080900160616      * SE NON TRASMETTO A SEDE LA BOLLA FIGLIA NON TRASMETTO
081000160616      * NEANCHE LA BOLLA ORIGINALE
081100160616     C                   Z-ADD     1             §F
081200160616     C                   SETOFF                                       03
081300160616      * BOLLE DA NON INVIARE
081400160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
081500160617     C**N03              ADD       1             NRSED             7 0
081600160616     C  N03              WRITE     FIARBC00                                     SEDE
081700160616    4c                   endif
081800160616    3c                   endif
081900160616    2c                   endif
082000160616      *
082100160616    1C                   endif
082200160616    0C                   endif
082300160616
082400160616     c                   ENDSR
082500930510      *----------------------------------------------------------------
082600930510      * PULIZIA PARAMETRI X TRUL51C
082700930510      *----------------------------------------------------------------
082800930510     C     TRUL51        BEGSR
082900971120     C                   MOVEL     '*LIBL'       LIBR             10
083000930510     C                   MOVEL     *BLANK        FILE             10
083100930510     C                   MOVEL     *BLANK        MBR              10
083200930510     C                   Z-ADD     0             NRR              10 0
083300930510     C                   ENDSR
083400930415     C**----------------------------------
083500930415     C** LANCIO AZIONI DI TRASMISSIONE IN SEDE
083600930415     C**----------------------------------
083700930415     C     INVSED        BEGSR
083800930510     C                   EXSR      TRUL51
083900010103     C                   MOVEL     'FIARBC46'    FILE
084000020920     C                   MOVEL     §membro       MBR
084100930510     C                   CALL      'TRUL51C'
084200930510     C                   PARM                    LIBR
084300930510     C                   PARM                    FILE
084400930510     C                   PARM                    MBR
084500930510     C                   PARM                    NRR
084600930415     C     NRR           IFGT      0
084700971120     C     NRR           ANDNE     9999999999
084800971120     C* Richiamo TIBS50R per conoscere librerie/S.Informativi
084900971120     C* da/a cui spedire
085000971120     C                   CLEAR                   TIBS50
085100971120     C                   MOVEL     *BLANKS       I50TLA
085200971120     C                   MOVEL     KNSIF         I50PSI
085300971120     C                   MOVEL     SIMFEL        I50PPO
085400971120     C                   MOVEL     '046'         I50APO
085500020920     C                   CALL      'TIBS55R'
085600971120     C                   PARM                    TIBS50
085700971120     C* INVIO
085800930415     C                   EXSR      X25SED
085900930415     C                   END
086000930415     C                   ENDSR
086100911112     C**----------------------------------
086200911112     C** SOTTOMETTE TRASMISSIONE X LA SEDE
086300911112     C**----------------------------------
086400911112     C     X25SED        BEGSR
086500971120     C*
086600971120     C                   CLEAR                   W236
086700911112     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
086800010103     C                   MOVEL     'FIARBC46'    CMZFLD                          DA: >>>
086900160617     C                   MOVEL(p)  §membro       CMZMBD
087000971120     C                   MOVEL     O50PLA        CMZLBD
087100010103     C                   MOVEL     'FIARB46R'    CMZFLA                          A: >>>
087200020920     C                   MOVEL(p)  §membro       CMZMBA
087300020927     C                   MOVEL     O50ALA        CMZLBA
087400020927     C                   MOVEL     'GAITRA'      CMZLBA
087500020927     C                   MOVE      O50ALA        WQUATR            4
087600020927     C     WQUATR        IFEQ      '101 '
087700020927     C                   MOVE      '201 '        CMZLBA
087800020927     C                   ENDIF
087900010305     C                   MOVE      'R'           CMZURG
088000911112     C                   MOVE      'A'           CMZADD                          IN ADD
088100911112     C                   MOVE      'S'           CMZCLR                          CLEAR
088200911112     C                   MOVE      'S'           CMZFTR                          DA TRANS.
088300911112     C                   MOVE      *BLANK        CMZERR                          1= ERR.
088400010103     C                   MOVE      'TR92'        CMZAZN                          AZIONE
088500020927     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
088600020927     C                   MOVEL     'GAITRA'      CMZSYF
088700020927     C                   MOVE      O50ASI        WQUATR
088800020927     C     WQUATR        IFEQ      '101 '
088900020927     C                   MOVE      '201 '        CMZSYF
089000020927     C                   ENDIF
089100911112     C                   MOVE      NRR           CMZNRR                          N.REC.
089200911112     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
089300021007     C                   CALL      'TIBS53C'
089400971120     C                   PARM                    O50LCL
089500911112     C                   PARM                    CMZDST
089600911112     C                   PARM                    CMZFLD
089700911112     C                   PARM                    CMZMBD
089800911112     C                   PARM                    CMZLBD
089900911112     C                   PARM                    CMZFLA
090000911112     C                   PARM                    CMZMBA
090100911112     C                   PARM                    CMZLBA
090200911112     C                   PARM                    CMZORI
090300911112     C                   PARM                    CMZDTI
090400911112     C                   PARM                    CMZURG
090500911112     C                   PARM                    CMZADD
090600911112     C                   PARM                    CMZCLR
090700911112     C                   PARM                    CMZFTR
090800911112     C                   PARM                    CMZERR
090900911112     C                   PARM                    CMZAZN
091000911112     C                   PARM                    CMZSYF
091100911112     C                   PARM                    CMZNRR
091200911112     C                   PARM                    CMZPRR
091300160527     C                   PARM                    W236            236
091400971120     C*
091500911112     C                   ENDSR
091600160520     C**---------------------------------------------------------
091700160520     C** CONTROLLO SE DEVO TRASMETTERE I DATI AL CLIENTE da BLP
091800160520     C**---------------------------------------------------------
091900160520     C     DATCLI        BEGSR
092000160520      * SE IL CLIENTE VUOLE RICEVERE DATI CONSEGNA SFLEGGO CAMPI TRASMISS.
092100160520     C                   SETOFF                                       32
092200160520     C     BLPCCM        IFNE      0
092300160520     C                   Z-ADD     1             W
092400160520     C     BLPCCM        LOOKUP    CLI(W)                                 32
092500160520     C                   END
092600160520     C  N32BLPKSC        IFNE      0
092700160520     C                   Z-ADD     1             W
092800160520     C     BLPKSC        LOOKUP    CLI(W)                                 32
092900160520     C                   END
093000160520     C* SE ANCORA NON TROVATO CHAIN SU ar4 PER VERIFICARE SE ESISTE IL
093100160520      *CODICE DEL CLIENTE NEL CAMPO NOTE
093200160520     C  N32              DO
093300160520     C     Kar4          CHAIN     Fiar401L                           66
093400160520     C                   IF        not *in66
093500160520     C                   MOVEL     ar4NOT        KSCJ              7 0
093600160520     C                   Z-ADD     1             W
093700160520     C     KSCJ          LoOKUP    CLi(W)                                 32
093800160520     C                   END
093900160520     C                   END
094000160520      *
094100160520     C   32              MOVEL     *BLANK        BLPFT3
094200160520     C   32              MOVE      *ZEROS        BLPDT3
094300160520     C*
094400160520     C                   ENDSR
094500941025     C*****************************************************************
094600941025     C*     I N Z S R
094700941025     C*****************************************************************
094800941025     C     *INZSR        BEGSR
094900971120     C*
095000941025     C     *ENTRY        PLIST
095100941025     C                   PARM                    KPJBA
095200160616     C                   PARM                    param7b
095300160616     c
095400020920     C                   MOVEL     KPJBU         param7a
095500160520
095600950102     C     NMI7A         IFGT      '0000000'
095700950102     C                   MOVE      NMI7A         KNMI
095800020920     C                   clear                   Kipp
095900020920     C                   move      fgs7a         Kipp
096000950102     C                   ENDIF
096100160616     c
096200160616     c                   if        %parms=2 and pa7nsped>*zeros
096300160616     c                   monitor
096400160616     c                   eval      kpaas=%int(%subst(pa7nsped:1:4))
096500160616     c                   eval      kplnp=%int(%subst(pa7nsped:5:3))
096600160616     c                   eval      kpnrs=%int(%subst(pa7nsped:8:2))
096700160616     c                   eval      kpnsp=%int(%subst(pa7nsped:10:7))
096800160616     c                   on-error
096900160616     c                   clear                   kpaas
097000160616     c                   clear                   kplnp
097100160616     c                   clear                   kpnrs
097200160616     c                   clear                   kpnsp
097300160616     c                   endmon
097400160616     c                   endif
097500160616     c
097600160520
097700941025     C                   Z-ADD     1             CODUT
097800941025     C                   CALL      'X§PARUT'
097900941025     C                   PARM                    UTEDSE
098000941025     C                   MOVEL     REC80         CNCR80
098100941025     C*
098200020924     C     SIMTPP        IFEQ      '2'
098300020924     C     SIMTPP        OREQ      *BLANKS
098400020924     C                   MOVE      SIMPOU        §FLE              3 0
098500020924     C                   ELSE
098600020924     C     SIMFEL        COMP      0                                      02
098700020924     C  N02              MOVE      simfel        §fle
098800020924     C                   END
098900020925     C                   z-add     SIMFEL        xFLE
099000941025     C*§§
099100941025     C                   TIME                    WHHUDT           14 0
099200941025     C                   MOVE      WHHUDT        G08DAT
099300941025     C                   MOVE      *ZEROS        G08INV
099400941025     C                   MOVEL     *BLANK        G08ERR
099500941025     C                   CALL      'XSRDA8'
099600941025     C                   PARM                    WLBDA8
099700941025     C                   Z-ADD     G08INV        AMGUDT            8 0
099800941025     C*
099900950126     C*CARICO I CODICI BOLLE DA NON INVIARE IN SEDE
100000010305     C                   OPEN      TABEL00F
100100950126     C                   MOVEL     '3A'          COD
100200950126     C                   MOVE      *ZEROS        §F                2 0
100300950126     C     KTAB1         SETLL     TABEL
100400160608     C     KTAB1         READE     TABEL
100500160608     c                   dow       not %eof(tabel00f)
100600160608
100700160608     C     TBLFLG        IFEQ      *BLANK                                        ANNUL.
100800950126     C                   MOVEL     TBLUNI        DS3A
100900950126     C     §3ABSD        IFEQ      'N'                                           NO TRASM.IN SEDE
101000950126     C                   ADD       1             §F
101100950126     C                   MOVEL     TBLKEY        CDB(§F)
101200950126     C                   ENDIF
101300950126     C                   ENDIF
101400160608
101500160608     C     KTAB1         READE     TABEL
101600950126     C                   ENDDO
101700160520
101800160520     C*CARICO gli eventi IDD
101900970805     C                   MOVEL     '2A'          COD
102000970805     C                   MOVE      *ZEROS        §F                2 0
102100970805     C     KTAB1         SETLL     TABEL
102200160608     C     KTAB1         READE     TABEL
102300160608     c                   dow       not %eof(tabel00f)
102400160608
102500160608     C     TBLFLG        IFEQ      *BLANK                                        ANNUL.
102600970805     C                   MOVEL     TBLUNI        DS2A
102700970805     C     §2AIDD        IFNE      *BLANKS                                       NO TRASM.IN SEDE
102800970805     C                   ADD       1             §F
102900970805     C                   MOVEL     TBLKEY        IDD(§F)
103000970805     C                   ENDIF
103100970805     C                   ENDIF
103200160608     C     KTAB1         READE     TABEL
103300970805     C                   ENDDO
103400160520     C*                                                    *
103500160520     C* CARICO I CODICI CLIENTI A CUI MANDARE LE DATE DI CONSEGNA
103600160608     C                   MOVEL     '3K'          COD
103700160520     C                   MOVE      *ZEROS        W                 4 0
103800160520     C     Ktab1         SETLL     TABEL00F
103900160520     C     Ktab1         READE     TABEL00F                               31
104000160608     c                   dow       not %eof(tabel00f)
104100160608
104200160520     C                   MOVEL     TBLUNI        DS3K
104300160520     C                   ADD       1             W
104400160520     C                   MOVEL     TBLKEY        CLI(W)
104500160520      *
104600160608     C     Ktab1         READE     TABEL00F                               31
104700160608     C                   ENDdo
104800160608     c
104900941026     C                   CLOSE     TABEL00F
105000941025     C*
105100941025     C     KART          KLIST
105200941216     C                   KFLD                    ARBAAS
105300160527     C                   KFLD                    ARBLNP
105400941025     C                   KFLD                    ARBNRS
105500941025     C                   KFLD                    ARBNSP
105600160616     C     Kpbol         KLIST
105700160616     C                   KFLD                    kpAAS
105800160616     C                   KFLD                    kpLNP
105900160616     C                   KFLD                    kpNRS
106000160616     C                   KFLD                    kpNSP
106100160519     C     Kblt          KLIST
106200160519     C                   KFLD                    artAAS
106300160527     C                   KFLD                    artLNP
106400160519     C                   KFLD                    artNRS
106500160519     C                   KFLD                    artNSP
106600160519     C                   KFLD                    artNSC
106700161114     C     KbltO         KLIST
106800161114     C                   KFLD                    bltAAS
106900161114     C                   KFLD                    bltLNP
107000161114     C                   KFLD                    bltNRS
107100161114     C                   KFLD                    bltNSP
107200161114     C                   KFLD                    bltNSC
107300970805     C     KEVB          KLIST
107400990803     C                   KFLD                    KAAS
107500970805     C                   KFLD                    KLNP
107600970805     C                   KFLD                    KNRS
107700970805     C                   KFLD                    KNSP
107800941025     C*
107900970805     C     KARBF         KLIST
108000970805     C                   KFLD                    LBLAAN
108100970805     C                   KFLD                    LBLLPN
108200970805     C                   KFLD                    LBLNRN
108300970805     C                   KFLD                    LBLNSN
108400970805     C     KARBM         KLIST
108500970805     C                   KFLD                    LBLAAP
108600970805     C                   KFLD                    LBLLPP
108700970805     C                   KFLD                    LBLNRP
108800970805     C                   KFLD                    LBLNSP
108900160526     C     Kblpo         KLIST
109000160526     C                   KFLD                    LBLAAo
109100160526     C                   KFLD                    LBLLPo
109200160526     C                   KFLD                    LBLNRo
109300160526     C                   KFLD                    LBLNSo
109400941025     C     KARB          KLIST
109500941025     C                   KFLD                    ARBAAS
109600941025     C                   KFLD                    ARBLNP
109700941025     C                   KFLD                    ARBNRS
109800941025     C                   KFLD                    ARBNSP
109900941025     C*
110000950126     C     KTAB1         KLIST
110100950126     C                   KFLD                    CODUT
110200950126     C                   KFLD                    COD
110300020920     C*
110400020920     C     Karb72        KLIST
110500020920     C                   KFLD                    kipp
110600020920     C                   KFLD                    knmi
110700160520     C     Kar4          KLIST
110800160520     C                   KFLD                    blpAAS
110900160520     C                   KFLD                    blpLNP
111000160520     C                   KFLD                    blpNRS
111100160520     C                   KFLD                    blpNSP
111200160520     C                   KFLD                    TRECz             1
111300160520     c                   move      'Z'           trecz
111400971120     C*
111500941025     C* DEFINIZIONE CHIAVI
111600941025     C*
111700941025     C     *LIKE         DEFINE    TBLCOD        COD
111800941216     C     *LIKE         DEFINE    ARBNMI        KNMI
111900020920     C     *LIKE         DEFINE    ARBipp        Kipp
112000990803     C     *LIKE         DEFINE    EVBAAS        KAAS
112100970805     C     *LIKE         DEFINE    EVBLNP        KLNP
112200970805     C     *LIKE         DEFINE    EVBNRS        KNRS
112300970805     C     *LIKE         DEFINE    EVBNSP        KNSP
112400020920     C                   Z-ADD     43            LUNG43           15 5
112500020920     C                   Z-ADD     48            LUNG48           15 5
112600970314     C*
112700941025     C                   ENDSR
112800941021     OFNARB000  E            AGGB0
112900941025     O                       ARBNCI
113000941025     O                       ARBNPI
113100930521     O                       ARBDCE
113200941025     O                       ARBNRC
113300930414     O                       ARBFT1
113400941025     O                       ARBDT1
113500941025     OFNARB000  E            AGGOR
113600941025     O                       ARBFT1
113700941025     O                       ARBDT1
113800160520     o
113900941021     OFNART000  E            AGGT0
114000961113     O                       ARTFTR
114100160616     OFNARB001  E            AGGB1
114200160616     O                       ARBNCI
114300160616     O                       ARBNPI
114400160616     O                       ARBDCE
114500160616     O                       ARBNRC
114600160616     O                       ARBFT1
114700160616     O                       ARBDT1
114800160616     OFNARB001  E            AGGOR1
114900160616     O                       ARBFT1
115000160616     O                       ARBDT1
115100010103**         CMDC
115200020920CHKOBJ OBJ(FIARBC46) OBJTYPE(*FILE) MBR(M000000)
115300911111**         CMDA
115400020920ADDPFM FILE(FIARBC46)               MBR(M000000)
115500911111**         CMDO
115600020920OVRDBF FILE(FIARBC46)               MBR(M000000)
115700950411**         CMDL
115800020920ALCOBJ OBJ((FIARBC46 *FILE *EXCL  M000000))
115900950411**         CMDD
116000020920DLCOBJ OBJ((FIARBC46 *FILE *EXCL  M000000))
