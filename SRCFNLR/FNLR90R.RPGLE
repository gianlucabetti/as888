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
007600180201     d  s_newflo       s                   like(newflo)
007700180201     d  kpaas          s                   like(arbaas)
007800160616     d  kplnp          s                   like(arblnp)
007900160616     d  kpnrs          s                   like(arbnrs)
008000160616     d  kpnsp          s                   like(arbnsp)
008100160616     d  Singola        s              1
008200161115     d  figl_flp       s                   like(newflp)
008300161115     d  figl_det       s                   like(newdet)
008400161115     d  figl_dut       s                   like(newdut)
008500161115     d  figl_pet       s                   like(newpet)
008600161115     d  figl_put       s                   like(newput)
008700161115     d  figl_flnap     s                   like(newflnap)
008800161115     d  figl_dam       s                   like(newdam)
008900161115     d  figl_dia       s                   like(newdia)
009000161115     d  figl_nap       s                   like(newnap)
009100161115     d  figl_ham       s                   like(newham)
009200161115     d  figl_damf      s                   like(newdamf)
009300161115     d  figl_diaf      s                   like(newdiaf)
009400161115     d  figl_hamf      s                   like(newhamf)
009500161115     d  figl_napf      s                   like(newnapf)
009600161115     d  figl_cca       s                   like(newcca)
009700161115     d  figl_dcm       s                   like(newdcm)
009800161115     d  figl_hcm       s                   like(newhcm)
009900911014     C*---------------------------------------------------------------*
010000941216     C* PREPARAZIONE ARCHIVIO DATI CONSEGNA  + DETTAGLIO SEGNACOLLI
010100160616     c
010200160616     c                   select
010300160616     c* Elaboro una bolla soltanto
010400160616    1c                   when      %parms=2 and kpnsp>0
010500160616     c                   exsr      Ela_bolla
010600160616    1c
010700950102     C* TRASMETTO SOLO SE HO MANDATO INTROITO IMPOSTATO
010800160616    1C                   when      KNMI>0
010900930421     C                   EXSR      ARBCSR
011000941025     C*
011100941025      * RILEGGO X TRASMETTERE ALLA BOLLA ORIGINALE
011200941025     C                   EXSR      ARBORI
011300160616     c                   endsl
011400160616
011500941025     C*
011600930421     C* I N V I O  A SEDE
011700930421     C*
011800160616    1c                   if        %open(fiarbc46)
011900010103     C                   CLOSE     FIARBC46
012000160616
012100160617    2C***  NRSED         IFGT      *ZEROS
012200160617     c
012300160617     c                   if        KNMI>0
012400911111     C                   EXSR      INVSED
012500160617    2C                   ENDif
012600010103      * Disalloco FIARBC46
012700020925     C                   MOVEL(p)  CMDD(1)       COMMAN
012800020925     c                   eval      %subst(comman:36:6) = §mbr
012900950411     C                   CALL      'QCMDEXC'                            60
013000950411     C                   PARM                    COMMAN
013100020920     C                   PARM                    LUNG43
013200950411     C*
013300160616    1C                   ENDIF
013400160616
013500930415      *
013600950411     C     FINE          TAG
013700160520     C*
013800971120     C                   CLEAR                   TIBS50
013900971121     C                   MOVEL     'C'           I50TLA
014000020920     C                   CALL      'TIBS55R'
014100971120     C                   PARM                    TIBS50
014200161116     c
014300161116     c                   clear                   fnlru39ds
014400161116     c                   movel     'C'           i38tla
014500161116     c                   call      'FNLRU39R'
014600161116     c                   parm                    fnlru39ds
014700971120     C*
014800911015     C                   SETON                                        LR
014900930421     C***--------------------------------------------***
015000930421     C* PREPARAZIONE ARCHIVIO DATI CONSEGNA
015100930421     C***--------------------------------------------***
015200930421     C     ARBCSR        BEGSR
015300930421     C                   SETOFF                                       10
015400160616     c
015500020920     C     karb72        SETLL     FNARB72L
015600930421     C*
015700930421     C* APRO FILE X LA SEDE
015800930421     C                   EXSR      CAEX46
015900930421     C*
016000160519    1C     *IN10         DOUEQ     '1'
016100020920     C     karb72        READE     FNARB72L                             1510
016200930421     C  N15
016300160519    2CANN10              DO
016400160616     c
016500160616     c                   exsr      EventiB
016600160616      *
016700941024     C                   MOVEL     *BLANKS       ARBFAG
016800160527     C                   MOVEL     *BLANK        HAFIGL            1
016900160527     C                   MOVEL     *BLANK        HAMAMM            1
017000160527     C                   MOVEL     *BLANK        HAORIG            1
017100160527     C                   MOVEL     *BLANK        ARBFAG
017200160519      *
017300160527      * elaboro la bolla per :
017400160527      * - consegna totale
017500160527      * - consegna parziale
017600160527      * - evento IDD
017700160519    3C     ARBDCM        IFGT      *ZEROS
017800930524     C     ARBDCP        ORGT      *ZEROS
017900970805     C     WTREVE        OREQ      'S'
018000160527     c
018100160616     c                   exsr      Sd99_lbl
018200160616
018300160616     c                   exsr      Agg_ART
018400160616
018500160616     c                   exsr      Agg_ARBBLP
018600930524      *
018700160616    3C                   endif
018800160616    2C                   ENDDO
018900160616    1C                   ENDDO
019000930421      *
019100930421     C                   ENDSR
019200160616      * ----------------------------------------------------  EVENTO IDD
019300160616     c     EventiB       BEGSR
019400160616      * ----------------------------------------------------  EVENTO IDD
019500160616      * Controllo se sulla bolla esiste evento di IDD
019600160616     C                   MOVEL     ' '           WTREVE            1
019700160616    3C     ARBCCA        IFNE      '5'
019800160616     C                   Z-ADD     ARBAAS        KAAS
019900160616     C                   Z-ADD     ARBLNP        KLNP
020000160616     C                   Z-ADD     ARBNRS        KNRS
020100160616     C                   Z-ADD     ARBNSP        KNSP
020200160616     C* Loop su eventi fino a fine o trovato evento
020300160616     C     KEVB          SETGT     FNEVB01L
020400160616     C     KEVB          READPE    FNEVB01L                               34
020500160616    4C     *IN34         DOWEQ     '0'
020600160616     C                   Z-ADD     1             W
020700160616     C     EVBCEV        LOOKUP    IDD(W)                                 34
020800160616    5C     *IN34         IFEQ      '1'
020900160616     C                   MOVEL     'S'           WTREVE
021000160616     C                   ELSE
021100160616     C     KEVB          READPE    FNEVB01L                               34
021200160616    5C                   END
021300160616    4C                   ENDdo
021400160616     C*
021500160616    3C                   ENDif
021600160616     c                   ENDSR
021700160616      * -------------------------------------------------------------------
021800160616     c     SD99_lbl      BEGSR
021900160616      * --------------------------------------------------------------------
022000160616      * SE SPEDIZIONE CONSEGNATA CALCOLO TEMPI DELIVERY
022100160616     C                   MOVE      *ZEROS        ARBNCI
022200160616     C                   MOVE      *ZEROS        ARBNPI
022300160616     C                   MOVE      *ZEROS        ARBDCE
022400160616     C                   MOVE      *ZEROS        ARBNRC
022500160616     c                   clear                   tnsd99
022600160616     C                   MOVEL     *BLANKS       ARBFAG
022700160616     C                   MOVEL     *BLANK        HAFIGL            1
022800160616     C                   MOVEL     *BLANK        HAMAMM            1
022900160616     C                   MOVEL     *BLANK        HAORIG            1
023000160616     C                   MOVEL     *BLANK        ARBFAG
023100160616      *
023200160616      * in caso di consegna parziale, non mi serve verificare i legami bolla
023300160616      * e nemmeno l'affidabilità
023400160616    4C     ARBDCM        IFGT      *ZEROS
023500160616     C     ARBCCA        OREQ      '5'
023600160616     C     WTREVE        OREQ      'S'
023700160616      * GUARDO COSA DEVO TRASMETTERE ALLA PARTENZA SE CI SONO LEGAMI
023800160616      * - LA BOLLA HA UNA FIGLIA CON LNA DIVERSA (DIROTT./RESO)
023900160616     C                   MOVEL     *BLANK        HAFIGL            1
024000160616     C                   MOVEL     *BLANK        HAMAMM            1
024100160616     C                   MOVEL     *BLANK        HAORIG            1
024200160616     C                   MOVEL     *BLANK        ARBFAG
024300160616     C     KARB          SETGT     FNLBL02L
024400160616     C     KARB          READPE    FNLBL02L                               22
024500160616    5C     *IN22         DOWEQ     '0'
024600160616     C     ARBLNA        IFNE      LBLLAN
024700160616     C                   MOVEL     'S'           HAFIGL
024800160616     C                   ELSE
024900160616     C* Se la bolla ha una figlia con la stessa lna devo controllare
025000160616     C* se quest'ultima è chiusa con IDD
025100160616     c* ES 26/05: non capendo il significato di queste specifiche
025200160616     c*  le asterisco. non ho trovato il caso di bolla figlia con stessa LNA
025300160616     c*  e con dettaglio colli per leggerlo per aggiornare la mamma.
025400160616     c*  il cambio di porto, unico caso che ci è venuto in mente, non ha dettaglio colli
025500160616     C****               MOVEL     'S'           WCTIDD
025600160616     C                   END
025700160616     c
025800160616     C     KARB          READE     FNLBL02L                               22
025900160616    5C                   ENDDO
026000160616      *
026100160616      * - LA BOLLA HA UNA MAMMA + GUARDO LNA BOLLA ORIGINALE
026200160616     C     KARB          CHAIN     FNLBL01L                           22
026300160616    5C     *IN22         IFEQ      '0'
026400160616     C                   MOVEL     'S'           HAMAMM
026500160616     C     LBLLAO        IFNE      LBLLAP
026600160616     C     LBLLAO        ORNE      LBLLAN
026700160616     C                   MOVEL     'S'           HAORIG
026800160616     C                   END
026900160616     c* occorre trasmettere la data consegna anche alla bolla originale
027000160616     c* se la figlia è un reso con stessa linea arrivo
027100160616     C     LBLLAO        IFeq      LBLLAP
027200160616     C     LBLLAO        andeq     LBLLAN
027300160616     c     arbfbr        andeq     'S'
027400160616     C                   MOVEL     'S'           HAORIG
027500160616     c                   end
027600160616    5C                   ENDIF
027700160616      * SE HA FIGLIA ALTRA FIL.MA NON HA MAMMA è LA 1°
027800160616      * NON TRASMETTO DATA CONSEGNA
027900160616    5C     HAFIGL        IFEQ      'S'
028000160616     C     HAMAMM        ANDEQ     ' '
028100160616     C     ARBCCA        ANDNE     '2'
028200160616     C     ARBCCA        ANDNE     '6'
028300160616     C                   MOVEL     'D'           ARBFAG                         BOLLA DIROTTATA
028400160616    5C                   END
028500160616      *-----------------------------------------------------*
028600160616      * CALCOLO TEMPI DELIVERY
028700160616     C     ARBDCM        IFGT      0
028800160616     C                   MOVEL     ' '           D98TLA
028900160616     C                   MOVEL     'A'           D98TBO
029000160616     C                   MOVE      ARBAAS        D98AAS
029100160616     C                   MOVE      ARBLNP        D98LNP
029200160616     C                   MOVE      ARBNRS        D98NRS
029300160616     C                   MOVE      ARBNSP        D98NSP
029400160616     C                   CALL      'TNSD99R'
029500160616     C                   PARM                    TNSD99
029600160616     C                   END
029700160616    4C                   ENDif
029800160616     c
029900160616     c                   ENDSR
030000160616      *-----------------------------------------------------*
030100160616     c     Agg_art       BEGSR
030200160616      *-----------------------------------------------------*
030300160616      * ORA AGGIORNO PER OGNI COLLO DI ART FNBLT, IGNORANDO GLI SFLAGGATI MA
030400160616      *  AGGIORNANDO TUTTI I RECORD
030500160616     c                   setoff                                         11
030600160616     C     KART          SETLL     FNART01L
030700160616     C     KART          READE     FNART01L                             1511
030800160616     C                   DOW       NOT *IN15 AND NOT *IN11
030900160616     c                   exsr      Agg_blt
031000160616     c* se sflaggato,
031100160616     C*  FLEGGO COME TRASMESSI SEGNACOLLI SE NO BOLLA LEGATA
031200160616     c* ( solo quelli slafaggati, ma comunque li aggiorno tutti )
031300160616     c                   if        artftr=' ' and (haorig<>'S' or
031400160616     c                             hafigl<>' ')
031500160616     C                   MOVE      'T'           ARTFTR
031600160616     C                   EXCEPT    AGGT0
031700160616     C                   endif
031800160616
031900160616     C     KART          READE     FNART01L                             1511
032000160616     c                   enddo
032100160616     c                   ENDSR
032200160616      *-----------------------------------------------------*
032300160616     c     Agg_ARBBLP    BEGSR
032400160616      *-----------------------------------------------------*
032500160616     c***
032600160616     c* AGGIORNAMENTO FNBLP / FNARB --> solo nessun errore in lettura segnacolli
032700160616   4ac                   if        not *in15
032800160616     c* imposto tempi di delivery
032900160616     C                   Z-ADD     D98NCI        ARBNCI                         NON CONF.CONS. INT.
033000160616     C                   Z-ADD     D98NPI        ARBNPI                         NON CONF.PART. INT.
033100160616     C                   Z-ADD     D98DCE        ARBDCE                         DATA CONS. PREV.CLI.
033200160616     C                   Z-ADD     D98NRC        ARBNRC                         NON CONF. RESA CLI.
033300160616      *
033400160616    5C     ARBDCM        IFGT      *ZEROS
033500160616     C     ARBDCP        ORGT      *ZEROS
033600160616     c                   exsr      Agg_BLP
033700160616    5C                   END
033800160616     c***
033900160616      * A SEDE SOLO SE CONSEGNATE
034000160616     c***
034100160616    5C     ARBDCM        IFGT      0
034200160616     C* CONTROLLO SE LA BOLLA NON è DA INVIARE IN SEDE
034300160616     C                   Z-ADD     1             §F
034400160616     C                   SETOFF                                       03
034500160616      * BOLLE DA NON INVIARE
034600160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
034700160616    6c                   if        not *in03
034800160616     c* utilizzo il campo ARBFLP (fil.transito) x aggiornare la zona in sede
034900160616     c                   eval      savflp = arbflp
035000160616     c                   eval      arbflp = arbznc
035100160617     C****               ADD       1             NRSED             7 0
035200160616     C                   WRITE     FIARBC00                                     SEDE
035300160616     c                   eval      arbflp = savflp
035400160616    6C                   ENDIF
035500160616    5C                   ENDIF
035600160616      *
035700160616      * HA BOLLA ORIG.CON LINEA ARRIVO DIVERSA E NON HA FIGLIA CON
035800160616      * LNA DIVERSA : è L'ULTIMA BOLLA DELLA CATENA QUINDI DEVO TRASM.
035900160616      * ALLA BOLLA ORIGIN. LA DATA CONSEGNA , NON AGGIORNO ARBDT1 E
036000160616      * IMPOSTO FT1=O X RILEGGERLO SUCCESSIVAMENTE
036100160616    5C     HAORIG        IFEQ      'S'
036200160616     C     HAFIGL        ANDEQ     ' '
036300160616     C                   MOVEL     'O'           ARBFT1
036400160616     C                   MOVE      *ZEROS        ARBDT1
036500160616     C                   ELSE
036600160616     C                   MOVE      'T'           ARBFT1
036700160616     C                   Z-ADD     AMGUDT        ARBDT1
036800160616    5C                   ENDIF
036900160616      *
037000160616    5C     ARBDCM        IFGT      0
037100160616     C*                  MOVE      'T'           ARBFT2
037200160616     C*                  Z-ADD     AMGUDT        ARBDT2
037300160616    5C                   ENDIF
037400160616      *
037500160616     c                   if        singola=' '
037600160616     C                   EXCEPT    AGGB0
037700160616     c                   else
037800160616     C                   EXCEPT    AGGB1
037900160616     c                   endif
038000160616     c
038100160616   4aC                   endif
038200160616     c                   ENDSR
038300160519     C**-------------------------------------------------------------**
038400160519     C** Aggiorna dettaglio colli partenza
038500160519     C**-------------------------------------------------------------**
038600160519     c     Agg_blt       BEGSR
038700160527     c* Lettura sequenziale per aggiornamento dett colli BOLLA ORIGINALE
038800160527     c                   if        arbfag='O'
038900160527     c
039000160527     c                   if        primalet=' '
039100160527     c     kblpo         setll     fnblt01l
039200160527     c     kblpo         reade     fnblt01l
039300160527     c                   eval      primalet='1'
039400160527     c                   else
039500160527     c     kblpo         reade     fnblt01l
039600160527     c                   endif
039700160527     c
039800160527     c                   else
039900160527     c*
040000160527     c* lettura per segnacollo negli altri casi
040100160519     c     kblt          chain     fnblt01l
040200160527     c                   endif
040300160527     c
040400160519     c                   eval      bltfl1=artfl1
040500160526     c                   if        arbfag<>'D'
040600160519     c                   eval      bltdcm=artdcm
040700160526     c                   endif
040800160519     c                   eval      bltdet=artdet
040900160519     c                   eval      bltdut=artdut
041000160519     c                   eval      bltdam=artdam
041100160520     c                   eval      bltflp=artflp
041200160520     C     BLTFTR        IFNE      'C'
041300160520     C                   MOVEL     'R'           BLTFTR
041400160520     C                   MOVEL     AMGUDT        BLTDTR
041500160520     C                   END
041600160527     c                   if        arbfag='O' and not %eof(fnblt01l)
041700160527     c                   update    fnblt000
041800170322     c
041900170322     c* Non aggiorno fiart unico per RESO
042000170322     c                   if        blpcca<>'2' and blpcca<>'6'
042100161115     c                   exsr      Agg_ARTuni
042200160527     c                   endif
042300170322     c                   endif
042400160527     c
042500160527     c                   if        arbfag<>'O' and %found(fnblt01l)
042600160519     c                   update    fnblt000
042700161114     c
042800161114     c                   exsr      Agg_ARTuni
042900161114     c*
043000160519     c                   endif
043100160527
043200160519     c                   ENDSR
043300161114     c
043400161114     C**-------------------------------------------------------------**
043500161114     c**  Aggiorno FIART00f UNICO
043600161114     C**-------------------------------------------------------------**
043700161114     c     Agg_artuni    BEGSR
043800161114     C* SE STO AGGIORNANDO I COLLI della bolla figlia, devo
043900161114     c* leggere il FIART unico della figlia e memorizzarmi i campi che andranno
044000161114     c*  riaggiornati sulla bolla originale
044100161114     c                   if        arbfag='O'
044200161115     c     kblt          chain     fiart01l
044300161114     c                   if        %found(fiart01l)
044400161115     c* memorizzo i campi che mi servono
044500161115     c                   eval      figl_flp=newflp
044600161115     c                   eval      figl_det=newdet
044700161115     c                   eval      figl_pet=newpet
044800161115     c                   eval      figl_dut=newdut
044900161115     c                   eval      figl_put=newput
045000161115     c                   eval      figl_flnap=newFLNAP
045100161115     c                   eval      figl_dam=newdam
045200161115     c                   eval      figl_dia=newdia
045300161115     c                   eval      figl_nap=newnap
045400161115     c                   eval      figl_ham=newham
045500161115     c                   eval      figl_damf=newdamf
045600161115     c                   eval      figl_diaf=newdiaf
045700161115     c                   eval      figl_hamf=newhamf
045800161115     c                   eval      figl_napf=newnapf
045900161115     c                   eval      figl_cca =newcca
046000161115     c                   eval      figl_dcm =newdcm
046100161115     c                   eval      figl_hcm =newhcm
046200161114     c                   endif
046300161114     c                   endif
046400161114
046500161114     c* Aggiorno anche fiart unico
046600161115     c     kbltO         chain     fiart01l
046700161114     c                   if        %found(fiart01l)
046800180201     c* il campo ARTFLO è ora riempito con il tipo apparato.
046900180201     c* non lo devo clearare
047000180201     c*  quindi salvo il campo, e poi loo reimposto uguale
047100180201     c                   eval      s_newflo=newflo
047200161114     c                   clear                   fnlru39ds
047300161114     c                   clear                   n_fiartds
047400161114     c                   eval      i38aas=bltaas
047500161114     c                   eval      i38lnp=bltlnp
047600161114     c                   eval      i38nrs=bltnrs
047700161114     c                   eval      i38nsp=bltnsp
047800161114     c                   eval      i38nsc=bltnsc
047900161114     c                   call      'FNLRU39R'
048000161114     c                   parm                    fnlru39ds
048100161114     c                   parm                    n_fiartds
048200180201     c                   eval      newflo=s_newflo
048300161115     c
048400161114     c                   if        arbfag='O'
048500161115     c                   eval      newflp=figl_flp
048600161115     c                   eval      newdet=figl_det
048700161115     c                   eval      newpet=figl_pet
048800161115     c                   eval      newdut=figl_dut
048900161115     c                   eval      newput=figl_put
049000161115     c                   eval      newflnap=figl_FLNAP
049100161115     c                   eval      newdam=figl_dam
049200161115     c                   eval      newdia=figl_dia
049300161115     c                   eval      newnap=figl_nap
049400161115     c                   eval      newham=figl_ham
049500161115     c                   eval      newdamf=figl_damf
049600161115     c                   eval      newdiaf=figl_diaf
049700161115     c                   eval      newhamf=figl_hamf
049800161115     c                   eval      newnapf=figl_napf
049900161115     c                   eval      newcca =figl_cca
050000161115     c                   eval      newdcm =figl_dcm
050100161115     c                   eval      newhcm =figl_hcm
050200161115     c                   endif
050300161115     c
050400161114     c                   update    fiart000
050500161114     c                   endif
050600161114     c                   ENDSR
050700160519     C**-------------------------------------------------------------**
050800160519     C** Aggiorna bolla partenza
050900160519     C**-------------------------------------------------------------**
051000160519     c     Agg_blp       BEGSR
051100160526     c     karb          chain     fnblp01l
051200160519     c                   if        %found(fnblp01l)
051300160520     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
051400160520     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
051500160520     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
051600160520     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
051700160520     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
051800160520     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
051900160520     C     ARBFLP        IFGT      0
052000160520     C                   MOVE      ARBFLP        BLPFLP                         FILIALE TRANSITO
052100160520     C                   ENDIF
052200160520     C     BLPDET        IFEQ      0
052300160520     C                   MOVE      ARBDET        BLPDET
052400160520     C                   ENDIF
052500160520     C     BLPDAM        IFEQ      0
052600160520     C                   MOVE      ARBDAM        BLPDAM
052700160520     C                   ENDIF
052800160520     C     BLPDUT        IFEQ      0
052900160520     C                   MOVE      ARBDUT        BLPDUT
053000160520     C                   ENDIF
053100160520     C                   MOVE      ARBNDC        BLPNDC                         NR.DIST.CONS
053200160520     C                   MOVE      ARBDDC        BLPDDC                         DATA "  CONS
053300160520     C     BLPCCA        IFNE      '5'
053400160520     C                   MOVE      ARBCCA        BLPCCA                         T.MANC.CONS.
053500160520     C                   END
053600160520     C     BLPCCA        IFNE      '5'
053700160520     C     BLPDCP        OREQ      0
053800160520     C                   MOVE      ARBDCP        BLPDCP                         DATA 1° CONS
053900160520     C                   END
054000160520     C                   MOVE      ARBTMC        BLPTMC                         T.MANC.CONS.
054100160520     c
054200160520     c                   exsr      DATCLI
054300160520     C*
054400160520     c* se bolla dirottata non aggiorno la data e ora consegna merce
054500160520     c*  dovrò aggiornare con la bolla figlia
054600160520     c                   if        arbfag<>'D'
054700160519     c                   eval      blpdcm=arbdcm
054800160519     c                   eval      blphmc=arbhmc
054900160520     c                   endif
055000160520     c
055100160519     c                   update    fnblp000
055200160519     c                   endif
055300160519     c                   ENDSR
055400930421     C**-------------------------------------------------------------**
055500010103     C** QCAEXEC SUL MBR DI FIARBC46 PER LA SEDE
055600930421     C**-------------------------------------------------------------**
055700930421     C     CAEX46        BEGSR
055800930421     C** FILIALE PER APERTURA MEMBRO
055900930421     C** CONTROLLO SE ESISTE IL MEMBRO
056000930421     C                   SETOFF                                       60
056100160520     C                   MOVEL(P)  CMDC(1)       COMMAN           80
056200020920     c                   eval      %subst(comman:42:6) = §mbr
056300930421     C                   CALL      'QCMDEXC'                            60
056400930421     C                   PARM                    COMMAN
056500020920     C                   PARM                    LUNG48
056600930421     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
056700930421     C   60              DO
056800160520     C                   MOVEL(p)  CMDA(1)       COMMAN
056900020920     c                   eval      %subst(comman:42:6) = §mbr
057000930421     C                   CALL      'QCMDEXC'
057100930421     C                   PARM                    COMMAN
057200020920     C                   PARM                    LUNG48
057300930421     C                   END
057400930421     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
057500160520     C                   MOVEL(P)  CMDO(1)       COMMAN
057600020920     c                   eval      %subst(comman:42:6) = §mbr
057700930421     C                   CALL      'QCMDEXC'
057800930421     C                   PARM                    COMMAN
057900020920     C                   PARM                    LUNG48
058000010103     C* Provo ad allocare FIARBC46 X 3 volte
058100950411     C                   MOVEL     'N'           WALLOC            1
058200950411     C                   Z-ADD     1             XX                2 0
058300020920     C                   MOVEL(p)  CMDL(1)       COMMAN
058400020920     c                   eval      %subst(comman:36:6) = §mbr
058500950412     C     WALLOC        DOWEQ     'N'
058600950411     C     XX            ANDLE     3
058700950411     C                   CALL      'QCMDEXC'                            60
058800950411     C                   PARM                    COMMAN
058900020920     C                   PARM                    LUNG43
059000950411     C     *IN60         IFEQ      '0'
059100950411     C                   MOVEL     'S'           WALLOC                         Allocazione O.K.
059200950411     C                   END
059300950411     C                   ADD       1             XX
059400950411     C                   END
059500950411     C*  Se non sono riuscita ad allocarlo vado a fine pgm. e
059600950411     C*  risottometto in coda la trasmissione date di consegna
059700950411     C     WALLOC        IFEQ      'N'
059800160616     c                   if        singola=' '
059900020920     C                   MOVEL(p)  param7a       KPJBU
060000950411     C                   MOVEL     'LR90'        KCOAZ
060100950411     C                   CALL      'BCH10'
060200950411     C                   PARM                    KPJBA
060300950411     C                   GOTO      FINE
060400160616     c                   else
060500160616     c                   eval      pa7err='E'
060600160616     c                   endif
060700160616     c
060800950411     C                   END
060900950411     C*
061000010103     C                   OPEN      FIARBC46
061100160616     c
061200930421     C                   ENDSR
061300941025     C***--------------------------------------------***
061400941025     C* TRASMISS. SOLO DATA CONSEGNA ALLA BOLLA ORIGINALE
061500941025     C***--------------------------------------------***
061600941025     C     ARBORI        BEGSR
061700160527     C                   SETOFF                                       10
061800160527     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
061900160527     c*
062000020920     C     karb72        SETLL     FNARB72L
062100941025     C*
062200160520    1C     *IN10         DOUEQ     '1'
062300020920     C     Karb72        READE     FNARB72L                             1510
062400941025     C  N15
062500160520    2CANN10ARBFT1        IFEQ      'O'
062600941025     C     KARB          CHAIN     FNLBL01L                           22
062700160526     c                   clear                   primalet          1
062800160520      *
062900941025      * TRASMETTO SE C'E' DATA CONSEGNA
063000160520    3C     ARBDCM        IFGT      *ZEROS
063100970805     C     ARBCCA        OREQ      '5'
063200170322
063300170322     c* anticipo la chian per avere la consegna anomala
063400170322     c* in caso di reso infatti NON devo aggioranre il FIART unico
063500170322     c     kblpo         chain     fnblp01l
063600170322     c                   if        not %found(fnblp01l)
063700170322     c                   clear                   blpcca
063800170322     c                   endif
063900160527     C
064000160527     c* Leggo il dettaglio colli della bolla figlia
064100160527     c*  e aggiorno il detto collo della bolla mamma
064200160527     c                   setoff                                         11
064300160527     C     KART          SETLL     FNART01L
064400160527     C     KART          READE     FNART01L                             1511
064500160527    4C                   DOW       NOT *IN15 AND NOT *IN11
064600160527     c                   exsr      Agg_blt
064700160527
064800160527     c                   if        artftr=' '
064900160527     C                   MOVE      'T'           ARTFTR
065000160527     C                   EXCEPT    AGGT0
065100160527     C                   endif
065200160527     C     KART          READE     FNART01L                             1511
065300160527    4c                   enddo
065400970306     C*
065500970313      * TRASMETTO TESTATA SE C'E' DATA CONSEGNA
065600160527      * se ART allocato non aggiorno la testata e aspetto il prossimo giro
065700160520    4C     ARBDCM        IFGT      *ZEROS
065800160527     c     *in15         andeq     *off
065900160527     c
066000170322     c***  kblpo         chain     fnblp01l
066100160527    5c                   if        %found(fnblp01l)
066200160526     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
066300160526     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
066400160526     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
066500160526     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
066600160526     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
066700160526     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
066800160527    6C     ARBFLP        IFGT      0
066900160526     C                   MOVE      ARBFLP        BLPFLP                         FILIALE TRANSITO
067000160527    6C                   ENDIF
067100160526     C                   MOVE      ARBDET        BLPDET
067200160526     C                   MOVE      ARBDUT        BLPDUT
067300160526     C                   MOVE      ARBdam        BLPdam
067400160526     C* X BOLLA ORIGINALE AGGIORNO TMC CON FLAG CONSEGNA ANOMALA 5
067500160526     C                   MOVE      ARBNDC        BLPNDC                         NR.DIST.CONS
067600160526     C                   MOVE      ARBDDC        BLPDDC                         DATA "  CONS
067700160527    6C     BLPCCA        IFNE      '5'
067800160526     C     BLPDCP        OREQ      0
067900160526     C                   MOVE      ARBDCP        BLPDCP                         DATA 1° CONS
068000160527    6C                   END
068100160526     C                   MOVE      ARBTMC        BLPTMC                         T.MANC.CONS.
068200160526     C                   MOVE      ARBDCM        BLPDCM
068300160526     C                   MOVE      ARBHMC        BLPHMC
068400160526      * Controllo se trasmettere dati cliente
068500160526     C                   EXSR      DATCLI
068600160526     c
068700160526     c                   update    fnblp000
068800160527    5c                   endif
068900160526     c
069000160526     C                   MOVE      LBLLPO        ARBLNP
069100970313     C                   MOVE      LBLAAO        ARBAAS
069200970313     C                   MOVE      LBLNRO        ARBNRS
069300970313     C                   MOVE      LBLNSO        ARBNSP
069400160527     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
069500970313     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
069600950330      * SE NON TRASMETTO A SEDE LA BOLLA FIGLIA NON TRASMETTO
069700950330      * NEANCHE LA BOLLA ORIGINALE
069800950330     C                   Z-ADD     1             §F
069900950330     C                   SETOFF                                       03
070000950330      * BOLLE DA NON INVIARE
070100950330     C     ARBCBO        LOOKUP    CDB(§F)                                03
070200160617     C**N03              ADD       1             NRSED             7 0
070300010103     C  N03              WRITE     FIARBC00                                     SEDE
070400170322     c
070500170322   x4c                   else
070600170322     c                   unlock    fnblp01l
070700160520    4C                   END
070800941025     C*
070900941025     C                   MOVEL     'T'           ARBFT1
071000941025     C                   MOVE      AMGUDT        ARBDT1
071100160520    3C                   ENDif
071200160520     c
071300170322     c  n15              if        singola=' '
071400941025     C                   EXCEPT    AGGOR
071500160616     c                   else
071600160616     C                   EXCEPT    AGGOR1
071700160616     c                   endif
071800160616     c
071900160520    2C                   ENDif
072000160520    1C                   ENDdo
072100941025      *
072200941025     C                   ENDSR
072300160616     C***--------------------------------------------***
072400160616     C* Elaboro soltanto la bolla passata
072500160616     C***--------------------------------------------***
072600160616     c     Ela_bolla     BEGSR
072700160616
072800160616     c                   eval      singola='S'
072900160616     c
073000160616    1c                   if        not %open(fnarb01l)
073100160616     c                   open      fnarb01l
073200160616    1c                   endif
073300160616     c
073400160616     c     kpbol         chain(e)  fnarb01l
073500160616    1c                   if        %error or not %found(fnarb01l)
073600160616     c                   eval      pa7err='E'
073700160616     c                   leavesr
073800160616    1c                   endif
073900160616     c
074000160616     C* APRO FILE X LA SEDE
074100160616     C                   EXSR      CAEX46
074200160616     c
074300160616    1c                   if        pa7rich<>'C'
074400160616     c                   exsr      EventiB
074500160616    1c                   endif
074600160616     c
074700160616     c* se tipo richiamo = 'C' --> solo ricalcolo affidabilità solo se consegnata
074800160616     c*                            altrimenti in tutti i casi previsti
074900160616
075000160616    0c                   if        (pa7rich='C' and arbdcm>0) or
075100160616     c                             (pa7rich<>'C' and (arbdcm>0 or arbdcp>0
075200160616     c                             or wtreve='S'))
075300160616     c
075400160616     c                   exsr      SD99_LBL
075500160616     c
075600160616    1c                   if        pa7rich<>'C'
075700160616     c                   exsr      Agg_ART
075800160616
075900160616     c                   exsr      Agg_ARBBLP
076000160616     c
076100160616   x1c                   else
076200160616     c
076300160616    2C     D98DCE        IFNE      ARBDCE
076400160616     C     D98NPI        ORNE      ARBNPI
076500160616     C     D98NCI        ORNE      ARBNCI
076600160616     C     D98NRC        ORNE      ARBNRC
076700160616     C                   Z-ADD     D98DCE        ARBDCE
076800160616     C                   Z-ADD     D98NPI        ARBNPI
076900160616     C                   Z-ADD     D98NCI        ARBNCI
077000160616     C                   Z-ADD     D98NRC        ARBNRC
077100160616     C                   MOVEL     'C'           ARBFAG
077200160616     C                   Z-ADD     1             §F
077300160616     C                   SETOFF                                       03
077400160616      * BOLLE DA NON INVIARE
077500160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
077600160616    3c                   if        not *in03
077700160617     C***                ADD       1             NRSED             7 0
077800160616     C                   WRITE     FIARBC00                                     SEDE
077900160616    3C                   ENDIF
078000160616     c
078100160616     c     karb          chain     fnblp01l
078200160616    3c                   if        %found(fnblp01l)
078300160616     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
078400160616     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
078500160616     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
078600160616     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
078700160616     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
078800160616     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
078900160616     c                   update    fnblp000
079000160616    3c                   endif
079100160616
079200160616     c                   update    fnarb001
079300160616
079400160616     c* Se la bolla ha la mamma --> aggiorno anche la bolla originale
079500160616    3c                   if        haorig='S' and hafigl=' '
079600160616     C     KARB          CHAIN     FNLBL01L
079700160616    4c                   if        %found(fnlbl01l)
079800160616     c     kblpo         chain     fnblp01l
079900160616    5c                   if        %found(fnblp01l)
080000160616     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
080100160616     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
080200160616     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
080300160616     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
080400160616     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
080500160616     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
080600160616     c                   update    fnblp000
080700160616    5c                   endif
080800160616     c
080900160616     C                   MOVE      LBLLPO        ARBLNP
081000160616     C                   MOVE      LBLAAO        ARBAAS
081100160616     C                   MOVE      LBLNRO        ARBNRS
081200160616     C                   MOVE      LBLNSO        ARBNSP
081300160616     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
081400160616     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
081500160616      * SE NON TRASMETTO A SEDE LA BOLLA FIGLIA NON TRASMETTO
081600160616      * NEANCHE LA BOLLA ORIGINALE
081700160616     C                   Z-ADD     1             §F
081800160616     C                   SETOFF                                       03
081900160616      * BOLLE DA NON INVIARE
082000160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
082100160617     C**N03              ADD       1             NRSED             7 0
082200160616     C  N03              WRITE     FIARBC00                                     SEDE
082300160616    4c                   endif
082400160616    3c                   endif
082500160616    2c                   endif
082600160616      *
082700160616    1C                   endif
082800160616    0C                   endif
082900160616
083000160616     c                   ENDSR
083100930510      *----------------------------------------------------------------
083200930510      * PULIZIA PARAMETRI X TRUL51C
083300930510      *----------------------------------------------------------------
083400930510     C     TRUL51        BEGSR
083500971120     C                   MOVEL     '*LIBL'       LIBR             10
083600930510     C                   MOVEL     *BLANK        FILE             10
083700930510     C                   MOVEL     *BLANK        MBR              10
083800930510     C                   Z-ADD     0             NRR              10 0
083900930510     C                   ENDSR
084000930415     C**----------------------------------
084100930415     C** LANCIO AZIONI DI TRASMISSIONE IN SEDE
084200930415     C**----------------------------------
084300930415     C     INVSED        BEGSR
084400930510     C                   EXSR      TRUL51
084500010103     C                   MOVEL     'FIARBC46'    FILE
084600020920     C                   MOVEL     §membro       MBR
084700930510     C                   CALL      'TRUL51C'
084800930510     C                   PARM                    LIBR
084900930510     C                   PARM                    FILE
085000930510     C                   PARM                    MBR
085100930510     C                   PARM                    NRR
085200930415     C     NRR           IFGT      0
085300971120     C     NRR           ANDNE     9999999999
085400971120     C* Richiamo TIBS50R per conoscere librerie/S.Informativi
085500971120     C* da/a cui spedire
085600971120     C                   CLEAR                   TIBS50
085700971120     C                   MOVEL     *BLANKS       I50TLA
085800971120     C                   MOVEL     KNSIF         I50PSI
085900971120     C                   MOVEL     SIMFEL        I50PPO
086000971120     C                   MOVEL     '046'         I50APO
086100020920     C                   CALL      'TIBS55R'
086200971120     C                   PARM                    TIBS50
086300971120     C* INVIO
086400930415     C                   EXSR      X25SED
086500930415     C                   END
086600930415     C                   ENDSR
086700911112     C**----------------------------------
086800911112     C** SOTTOMETTE TRASMISSIONE X LA SEDE
086900911112     C**----------------------------------
087000911112     C     X25SED        BEGSR
087100971120     C*
087200971120     C                   CLEAR                   W236
087300911112     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
087400010103     C                   MOVEL     'FIARBC46'    CMZFLD                          DA: >>>
087500160617     C                   MOVEL(p)  §membro       CMZMBD
087600971120     C                   MOVEL     O50PLA        CMZLBD
087700010103     C                   MOVEL     'FIARB46R'    CMZFLA                          A: >>>
087800020920     C                   MOVEL(p)  §membro       CMZMBA
087900020927     C                   MOVEL     O50ALA        CMZLBA
088000020927     C                   MOVEL     'GAITRA'      CMZLBA
088100020927     C                   MOVE      O50ALA        WQUATR            4
088200020927     C     WQUATR        IFEQ      '101 '
088300020927     C                   MOVE      '201 '        CMZLBA
088400020927     C                   ENDIF
088500010305     C                   MOVE      'R'           CMZURG
088600911112     C                   MOVE      'A'           CMZADD                          IN ADD
088700911112     C                   MOVE      'S'           CMZCLR                          CLEAR
088800911112     C                   MOVE      'S'           CMZFTR                          DA TRANS.
088900911112     C                   MOVE      *BLANK        CMZERR                          1= ERR.
089000010103     C                   MOVE      'TR92'        CMZAZN                          AZIONE
089100020927     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
089200020927     C                   MOVEL     'GAITRA'      CMZSYF
089300020927     C                   MOVE      O50ASI        WQUATR
089400020927     C     WQUATR        IFEQ      '101 '
089500020927     C                   MOVE      '201 '        CMZSYF
089600020927     C                   ENDIF
089700911112     C                   MOVE      NRR           CMZNRR                          N.REC.
089800911112     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
089900021007     C                   CALL      'TIBS53C'
090000971120     C                   PARM                    O50LCL
090100911112     C                   PARM                    CMZDST
090200911112     C                   PARM                    CMZFLD
090300911112     C                   PARM                    CMZMBD
090400911112     C                   PARM                    CMZLBD
090500911112     C                   PARM                    CMZFLA
090600911112     C                   PARM                    CMZMBA
090700911112     C                   PARM                    CMZLBA
090800911112     C                   PARM                    CMZORI
090900911112     C                   PARM                    CMZDTI
091000911112     C                   PARM                    CMZURG
091100911112     C                   PARM                    CMZADD
091200911112     C                   PARM                    CMZCLR
091300911112     C                   PARM                    CMZFTR
091400911112     C                   PARM                    CMZERR
091500911112     C                   PARM                    CMZAZN
091600911112     C                   PARM                    CMZSYF
091700911112     C                   PARM                    CMZNRR
091800911112     C                   PARM                    CMZPRR
091900160527     C                   PARM                    W236            236
092000971120     C*
092100911112     C                   ENDSR
092200160520     C**---------------------------------------------------------
092300160520     C** CONTROLLO SE DEVO TRASMETTERE I DATI AL CLIENTE da BLP
092400160520     C**---------------------------------------------------------
092500160520     C     DATCLI        BEGSR
092600160520      * SE IL CLIENTE VUOLE RICEVERE DATI CONSEGNA SFLEGGO CAMPI TRASMISS.
092700160520     C                   SETOFF                                       32
092800160520     C     BLPCCM        IFNE      0
092900160520     C                   Z-ADD     1             W
093000160520     C     BLPCCM        LOOKUP    CLI(W)                                 32
093100160520     C                   END
093200160520     C  N32BLPKSC        IFNE      0
093300160520     C                   Z-ADD     1             W
093400160520     C     BLPKSC        LOOKUP    CLI(W)                                 32
093500160520     C                   END
093600160520     C* SE ANCORA NON TROVATO CHAIN SU ar4 PER VERIFICARE SE ESISTE IL
093700160520      *CODICE DEL CLIENTE NEL CAMPO NOTE
093800160520     C  N32              DO
093900160520     C     Kar4          CHAIN     Fiar401L                           66
094000160520     C                   IF        not *in66
094100160520     C                   MOVEL     ar4NOT        KSCJ              7 0
094200160520     C                   Z-ADD     1             W
094300160520     C     KSCJ          LoOKUP    CLi(W)                                 32
094400160520     C                   END
094500160520     C                   END
094600160520      *
094700160520     C   32              MOVEL     *BLANK        BLPFT3
094800160520     C   32              MOVE      *ZEROS        BLPDT3
094900160520     C*
095000160520     C                   ENDSR
095100941025     C*****************************************************************
095200941025     C*     I N Z S R
095300941025     C*****************************************************************
095400941025     C     *INZSR        BEGSR
095500971120     C*
095600941025     C     *ENTRY        PLIST
095700941025     C                   PARM                    KPJBA
095800160616     C                   PARM                    param7b
095900160616     c
096000020920     C                   MOVEL     KPJBU         param7a
096100160520
096200950102     C     NMI7A         IFGT      '0000000'
096300950102     C                   MOVE      NMI7A         KNMI
096400020920     C                   clear                   Kipp
096500020920     C                   move      fgs7a         Kipp
096600950102     C                   ENDIF
096700160616     c
096800160616     c                   if        %parms=2 and pa7nsped>*zeros
096900160616     c                   monitor
097000160616     c                   eval      kpaas=%int(%subst(pa7nsped:1:4))
097100160616     c                   eval      kplnp=%int(%subst(pa7nsped:5:3))
097200160616     c                   eval      kpnrs=%int(%subst(pa7nsped:8:2))
097300160616     c                   eval      kpnsp=%int(%subst(pa7nsped:10:7))
097400160616     c                   on-error
097500160616     c                   clear                   kpaas
097600160616     c                   clear                   kplnp
097700160616     c                   clear                   kpnrs
097800160616     c                   clear                   kpnsp
097900160616     c                   endmon
098000160616     c                   endif
098100160616     c
098200160520
098300941025     C                   Z-ADD     1             CODUT
098400941025     C                   CALL      'X§PARUT'
098500941025     C                   PARM                    UTEDSE
098600941025     C                   MOVEL     REC80         CNCR80
098700941025     C*
098800020924     C     SIMTPP        IFEQ      '2'
098900020924     C     SIMTPP        OREQ      *BLANKS
099000020924     C                   MOVE      SIMPOU        §FLE              3 0
099100020924     C                   ELSE
099200020924     C     SIMFEL        COMP      0                                      02
099300020924     C  N02              MOVE      simfel        §fle
099400020924     C                   END
099500020925     C                   z-add     SIMFEL        xFLE
099600941025     C*§§
099700941025     C                   TIME                    WHHUDT           14 0
099800941025     C                   MOVE      WHHUDT        G08DAT
099900941025     C                   MOVE      *ZEROS        G08INV
100000941025     C                   MOVEL     *BLANK        G08ERR
100100941025     C                   CALL      'XSRDA8'
100200941025     C                   PARM                    WLBDA8
100300941025     C                   Z-ADD     G08INV        AMGUDT            8 0
100400941025     C*
100500950126     C*CARICO I CODICI BOLLE DA NON INVIARE IN SEDE
100600010305     C                   OPEN      TABEL00F
100700950126     C                   MOVEL     '3A'          COD
100800950126     C                   MOVE      *ZEROS        §F                2 0
100900950126     C     KTAB1         SETLL     TABEL
101000160608     C     KTAB1         READE     TABEL
101100160608     c                   dow       not %eof(tabel00f)
101200160608
101300160608     C     TBLFLG        IFEQ      *BLANK                                        ANNUL.
101400950126     C                   MOVEL     TBLUNI        DS3A
101500950126     C     §3ABSD        IFEQ      'N'                                           NO TRASM.IN SEDE
101600950126     C                   ADD       1             §F
101700950126     C                   MOVEL     TBLKEY        CDB(§F)
101800950126     C                   ENDIF
101900950126     C                   ENDIF
102000160608
102100160608     C     KTAB1         READE     TABEL
102200950126     C                   ENDDO
102300160520
102400160520     C*CARICO gli eventi IDD
102500970805     C                   MOVEL     '2A'          COD
102600970805     C                   MOVE      *ZEROS        §F                2 0
102700970805     C     KTAB1         SETLL     TABEL
102800160608     C     KTAB1         READE     TABEL
102900160608     c                   dow       not %eof(tabel00f)
103000160608
103100160608     C     TBLFLG        IFEQ      *BLANK                                        ANNUL.
103200970805     C                   MOVEL     TBLUNI        DS2A
103300970805     C     §2AIDD        IFNE      *BLANKS                                       NO TRASM.IN SEDE
103400970805     C                   ADD       1             §F
103500970805     C                   MOVEL     TBLKEY        IDD(§F)
103600970805     C                   ENDIF
103700970805     C                   ENDIF
103800160608     C     KTAB1         READE     TABEL
103900970805     C                   ENDDO
104000160520     C*                                                    *
104100160520     C* CARICO I CODICI CLIENTI A CUI MANDARE LE DATE DI CONSEGNA
104200160608     C                   MOVEL     '3K'          COD
104300160520     C                   MOVE      *ZEROS        W                 4 0
104400160520     C     Ktab1         SETLL     TABEL00F
104500160520     C     Ktab1         READE     TABEL00F                               31
104600160608     c                   dow       not %eof(tabel00f)
104700160608
104800160520     C                   MOVEL     TBLUNI        DS3K
104900160520     C                   ADD       1             W
105000160520     C                   MOVEL     TBLKEY        CLI(W)
105100160520      *
105200160608     C     Ktab1         READE     TABEL00F                               31
105300160608     C                   ENDdo
105400160608     c
105500941026     C                   CLOSE     TABEL00F
105600941025     C*
105700941025     C     KART          KLIST
105800941216     C                   KFLD                    ARBAAS
105900160527     C                   KFLD                    ARBLNP
106000941025     C                   KFLD                    ARBNRS
106100941025     C                   KFLD                    ARBNSP
106200160616     C     Kpbol         KLIST
106300160616     C                   KFLD                    kpAAS
106400160616     C                   KFLD                    kpLNP
106500160616     C                   KFLD                    kpNRS
106600160616     C                   KFLD                    kpNSP
106700160519     C     Kblt          KLIST
106800160519     C                   KFLD                    artAAS
106900160527     C                   KFLD                    artLNP
107000160519     C                   KFLD                    artNRS
107100160519     C                   KFLD                    artNSP
107200160519     C                   KFLD                    artNSC
107300161114     C     KbltO         KLIST
107400161114     C                   KFLD                    bltAAS
107500161114     C                   KFLD                    bltLNP
107600161114     C                   KFLD                    bltNRS
107700161114     C                   KFLD                    bltNSP
107800161114     C                   KFLD                    bltNSC
107900970805     C     KEVB          KLIST
108000990803     C                   KFLD                    KAAS
108100970805     C                   KFLD                    KLNP
108200970805     C                   KFLD                    KNRS
108300970805     C                   KFLD                    KNSP
108400941025     C*
108500970805     C     KARBF         KLIST
108600970805     C                   KFLD                    LBLAAN
108700970805     C                   KFLD                    LBLLPN
108800970805     C                   KFLD                    LBLNRN
108900970805     C                   KFLD                    LBLNSN
109000970805     C     KARBM         KLIST
109100970805     C                   KFLD                    LBLAAP
109200970805     C                   KFLD                    LBLLPP
109300970805     C                   KFLD                    LBLNRP
109400970805     C                   KFLD                    LBLNSP
109500160526     C     Kblpo         KLIST
109600160526     C                   KFLD                    LBLAAo
109700160526     C                   KFLD                    LBLLPo
109800160526     C                   KFLD                    LBLNRo
109900160526     C                   KFLD                    LBLNSo
110000941025     C     KARB          KLIST
110100941025     C                   KFLD                    ARBAAS
110200941025     C                   KFLD                    ARBLNP
110300941025     C                   KFLD                    ARBNRS
110400941025     C                   KFLD                    ARBNSP
110500941025     C*
110600950126     C     KTAB1         KLIST
110700950126     C                   KFLD                    CODUT
110800950126     C                   KFLD                    COD
110900020920     C*
111000020920     C     Karb72        KLIST
111100020920     C                   KFLD                    kipp
111200020920     C                   KFLD                    knmi
111300160520     C     Kar4          KLIST
111400160520     C                   KFLD                    blpAAS
111500160520     C                   KFLD                    blpLNP
111600160520     C                   KFLD                    blpNRS
111700160520     C                   KFLD                    blpNSP
111800160520     C                   KFLD                    TRECz             1
111900160520     c                   move      'Z'           trecz
112000971120     C*
112100941025     C* DEFINIZIONE CHIAVI
112200941025     C*
112300941025     C     *LIKE         DEFINE    TBLCOD        COD
112400941216     C     *LIKE         DEFINE    ARBNMI        KNMI
112500020920     C     *LIKE         DEFINE    ARBipp        Kipp
112600990803     C     *LIKE         DEFINE    EVBAAS        KAAS
112700970805     C     *LIKE         DEFINE    EVBLNP        KLNP
112800970805     C     *LIKE         DEFINE    EVBNRS        KNRS
112900970805     C     *LIKE         DEFINE    EVBNSP        KNSP
113000020920     C                   Z-ADD     43            LUNG43           15 5
113100020920     C                   Z-ADD     48            LUNG48           15 5
113200970314     C*
113300941025     C                   ENDSR
113400941021     OFNARB000  E            AGGB0
113500941025     O                       ARBNCI
113600941025     O                       ARBNPI
113700930521     O                       ARBDCE
113800941025     O                       ARBNRC
113900930414     O                       ARBFT1
114000941025     O                       ARBDT1
114100941025     OFNARB000  E            AGGOR
114200941025     O                       ARBFT1
114300941025     O                       ARBDT1
114400160520     o
114500941021     OFNART000  E            AGGT0
114600961113     O                       ARTFTR
114700160616     OFNARB001  E            AGGB1
114800160616     O                       ARBNCI
114900160616     O                       ARBNPI
115000160616     O                       ARBDCE
115100160616     O                       ARBNRC
115200160616     O                       ARBFT1
115300160616     O                       ARBDT1
115400160616     OFNARB001  E            AGGOR1
115500160616     O                       ARBFT1
115600160616     O                       ARBDT1
115700010103**         CMDC
115800020920CHKOBJ OBJ(FIARBC46) OBJTYPE(*FILE) MBR(M000000)
115900911111**         CMDA
116000020920ADDPFM FILE(FIARBC46)               MBR(M000000)
116100911111**         CMDO
116200020920OVRDBF FILE(FIARBC46)               MBR(M000000)
116300950411**         CMDL
116400020920ALCOBJ OBJ((FIARBC46 *FILE *EXCL  M000000))
116500950411**         CMDD
116600020920DLCOBJ OBJ((FIARBC46 *FILE *EXCL  M000000))
