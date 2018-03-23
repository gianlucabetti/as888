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
041700161115     c                   exsr      Agg_ARTuni
041800160527     c                   endif
041900160527     c
042000160527     c                   if        arbfag<>'O' and %found(fnblt01l)
042100160519     c                   update    fnblt000
042200161114     c
042300161114     c                   exsr      Agg_ARTuni
042400161114     c*
042500160519     c                   endif
042600160527
042700160519     c                   ENDSR
042800161114     c
042900161114     C**-------------------------------------------------------------**
043000161114     c**  Aggiorno FIART00f UNICO
043100161114     C**-------------------------------------------------------------**
043200161114     c     Agg_artuni    BEGSR
043300161114     C* SE STO AGGIORNANDO I COLLI della bolla figlia, devo
043400161114     c* leggere il FIART unico della figlia e memorizzarmi i campi che andranno
043500161114     c*  riaggiornati sulla bolla originale
043600161114     c                   if        arbfag='O'
043700161115     c     kblt          chain     fiart01l
043800161114     c                   if        %found(fiart01l)
043900161115     c* memorizzo i campi che mi servono
044000161115     c                   eval      figl_flp=newflp
044100161115     c                   eval      figl_det=newdet
044200161115     c                   eval      figl_pet=newpet
044300161115     c                   eval      figl_dut=newdut
044400161115     c                   eval      figl_put=newput
044500161115     c                   eval      figl_flnap=newFLNAP
044600161115     c                   eval      figl_dam=newdam
044700161115     c                   eval      figl_dia=newdia
044800161115     c                   eval      figl_nap=newnap
044900161115     c                   eval      figl_ham=newham
045000161115     c                   eval      figl_damf=newdamf
045100161115     c                   eval      figl_diaf=newdiaf
045200161115     c                   eval      figl_hamf=newhamf
045300161115     c                   eval      figl_napf=newnapf
045400161115     c                   eval      figl_cca =newcca
045500161115     c                   eval      figl_dcm =newdcm
045600161115     c                   eval      figl_hcm =newhcm
045700161114     c                   endif
045800161114     c                   endif
045900161114
046000161114     c* Aggiorno anche fiart unico
046100161115     c     kbltO         chain     fiart01l
046200161114     c                   if        %found(fiart01l)
046300161114     c                   clear                   fnlru39ds
046400161114     c                   clear                   n_fiartds
046500161114     c                   eval      i38aas=bltaas
046600161114     c                   eval      i38lnp=bltlnp
046700161114     c                   eval      i38nrs=bltnrs
046800161114     c                   eval      i38nsp=bltnsp
046900161114     c                   eval      i38nsc=bltnsc
047000161114     c                   call      'FNLRU39R'
047100161114     c                   parm                    fnlru39ds
047200161114     c                   parm                    n_fiartds
047300161115     c
047400161114     c                   if        arbfag='O'
047500161115     c                   eval      newflp=figl_flp
047600161115     c                   eval      newdet=figl_det
047700161115     c                   eval      newpet=figl_pet
047800161115     c                   eval      newdut=figl_dut
047900161115     c                   eval      newput=figl_put
048000161115     c                   eval      newflnap=figl_FLNAP
048100161115     c                   eval      newdam=figl_dam
048200161115     c                   eval      newdia=figl_dia
048300161115     c                   eval      newnap=figl_nap
048400161115     c                   eval      newham=figl_ham
048500161115     c                   eval      newdamf=figl_damf
048600161115     c                   eval      newdiaf=figl_diaf
048700161115     c                   eval      newhamf=figl_hamf
048800161115     c                   eval      newnapf=figl_napf
048900161115     c                   eval      newcca =figl_cca
049000161115     c                   eval      newdcm =figl_dcm
049100161115     c                   eval      newhcm =figl_hcm
049200161115     c                   endif
049300161115     c
049400161114     c                   update    fiart000
049500161114     c                   endif
049600161114     c                   ENDSR
049700160519     C**-------------------------------------------------------------**
049800160519     C** Aggiorna bolla partenza
049900160519     C**-------------------------------------------------------------**
050000160519     c     Agg_blp       BEGSR
050100160526     c     karb          chain     fnblp01l
050200160519     c                   if        %found(fnblp01l)
050300160520     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
050400160520     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
050500160520     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
050600160520     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
050700160520     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
050800160520     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
050900160520     C     ARBFLP        IFGT      0
051000160520     C                   MOVE      ARBFLP        BLPFLP                         FILIALE TRANSITO
051100160520     C                   ENDIF
051200160520     C     BLPDET        IFEQ      0
051300160520     C                   MOVE      ARBDET        BLPDET
051400160520     C                   ENDIF
051500160520     C     BLPDAM        IFEQ      0
051600160520     C                   MOVE      ARBDAM        BLPDAM
051700160520     C                   ENDIF
051800160520     C     BLPDUT        IFEQ      0
051900160520     C                   MOVE      ARBDUT        BLPDUT
052000160520     C                   ENDIF
052100160520     C                   MOVE      ARBNDC        BLPNDC                         NR.DIST.CONS
052200160520     C                   MOVE      ARBDDC        BLPDDC                         DATA "  CONS
052300160520     C     BLPCCA        IFNE      '5'
052400160520     C                   MOVE      ARBCCA        BLPCCA                         T.MANC.CONS.
052500160520     C                   END
052600160520     C     BLPCCA        IFNE      '5'
052700160520     C     BLPDCP        OREQ      0
052800160520     C                   MOVE      ARBDCP        BLPDCP                         DATA 1° CONS
052900160520     C                   END
053000160520     C                   MOVE      ARBTMC        BLPTMC                         T.MANC.CONS.
053100160520     c
053200160520     c                   exsr      DATCLI
053300160520     C*
053400160520     c* se bolla dirottata non aggiorno la data e ora consegna merce
053500160520     c*  dovrò aggiornare con la bolla figlia
053600160520     c                   if        arbfag<>'D'
053700160519     c                   eval      blpdcm=arbdcm
053800160519     c                   eval      blphmc=arbhmc
053900160520     c                   endif
054000160520     c
054100160519     c                   update    fnblp000
054200160519     c                   endif
054300160519     c                   ENDSR
054400930421     C**-------------------------------------------------------------**
054500010103     C** QCAEXEC SUL MBR DI FIARBC46 PER LA SEDE
054600930421     C**-------------------------------------------------------------**
054700930421     C     CAEX46        BEGSR
054800930421     C** FILIALE PER APERTURA MEMBRO
054900930421     C** CONTROLLO SE ESISTE IL MEMBRO
055000930421     C                   SETOFF                                       60
055100160520     C                   MOVEL(P)  CMDC(1)       COMMAN           80
055200020920     c                   eval      %subst(comman:42:6) = §mbr
055300930421     C                   CALL      'QCMDEXC'                            60
055400930421     C                   PARM                    COMMAN
055500020920     C                   PARM                    LUNG48
055600930421     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
055700930421     C   60              DO
055800160520     C                   MOVEL(p)  CMDA(1)       COMMAN
055900020920     c                   eval      %subst(comman:42:6) = §mbr
056000930421     C                   CALL      'QCMDEXC'
056100930421     C                   PARM                    COMMAN
056200020920     C                   PARM                    LUNG48
056300930421     C                   END
056400930421     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
056500160520     C                   MOVEL(P)  CMDO(1)       COMMAN
056600020920     c                   eval      %subst(comman:42:6) = §mbr
056700930421     C                   CALL      'QCMDEXC'
056800930421     C                   PARM                    COMMAN
056900020920     C                   PARM                    LUNG48
057000010103     C* Provo ad allocare FIARBC46 X 3 volte
057100950411     C                   MOVEL     'N'           WALLOC            1
057200950411     C                   Z-ADD     1             XX                2 0
057300020920     C                   MOVEL(p)  CMDL(1)       COMMAN
057400020920     c                   eval      %subst(comman:36:6) = §mbr
057500950412     C     WALLOC        DOWEQ     'N'
057600950411     C     XX            ANDLE     3
057700950411     C                   CALL      'QCMDEXC'                            60
057800950411     C                   PARM                    COMMAN
057900020920     C                   PARM                    LUNG43
058000950411     C     *IN60         IFEQ      '0'
058100950411     C                   MOVEL     'S'           WALLOC                         Allocazione O.K.
058200950411     C                   END
058300950411     C                   ADD       1             XX
058400950411     C                   END
058500950411     C*  Se non sono riuscita ad allocarlo vado a fine pgm. e
058600950411     C*  risottometto in coda la trasmissione date di consegna
058700950411     C     WALLOC        IFEQ      'N'
058800160616     c                   if        singola=' '
058900020920     C                   MOVEL(p)  param7a       KPJBU
059000950411     C                   MOVEL     'LR90'        KCOAZ
059100950411     C                   CALL      'BCH10'
059200950411     C                   PARM                    KPJBA
059300950411     C                   GOTO      FINE
059400160616     c                   else
059500160616     c                   eval      pa7err='E'
059600160616     c                   endif
059700160616     c
059800950411     C                   END
059900950411     C*
060000010103     C                   OPEN      FIARBC46
060100160616     c
060200930421     C                   ENDSR
060300941025     C***--------------------------------------------***
060400941025     C* TRASMISS. SOLO DATA CONSEGNA ALLA BOLLA ORIGINALE
060500941025     C***--------------------------------------------***
060600941025     C     ARBORI        BEGSR
060700160527     C                   SETOFF                                       10
060800160527     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
060900160527     c*
061000020920     C     karb72        SETLL     FNARB72L
061100941025     C*
061200160520    1C     *IN10         DOUEQ     '1'
061300020920     C     Karb72        READE     FNARB72L                             1510
061400941025     C  N15
061500160520    2CANN10ARBFT1        IFEQ      'O'
061600941025     C     KARB          CHAIN     FNLBL01L                           22
061700160526     c                   clear                   primalet          1
061800160520      *
061900941025      * TRASMETTO SE C'E' DATA CONSEGNA
062000160520    3C     ARBDCM        IFGT      *ZEROS
062100970805     C     ARBCCA        OREQ      '5'
062200160527     C
062300160527     c* Leggo il dettaglio colli della bolla figlia
062400160527     c*  e aggiorno il detto collo della bolla mamma
062500160527     c                   setoff                                         11
062600160527     C     KART          SETLL     FNART01L
062700160527     C     KART          READE     FNART01L                             1511
062800160527    4C                   DOW       NOT *IN15 AND NOT *IN11
062900160527     c                   exsr      Agg_blt
063000160527
063100160527     c                   if        artftr=' '
063200160527     C                   MOVE      'T'           ARTFTR
063300160527     C                   EXCEPT    AGGT0
063400160527     C                   endif
063500160527     C     KART          READE     FNART01L                             1511
063600160527    4c                   enddo
063700160527     c
063800970306     C*
063900970313      * TRASMETTO TESTATA SE C'E' DATA CONSEGNA
064000160527      * se ART allocato non aggiorno la testata e aspetto il prossimo giro
064100160520    4C     ARBDCM        IFGT      *ZEROS
064200160527     c     *in15         andeq     *off
064300160527     c
064400160526     c     kblpo         chain     fnblp01l
064500160527    5c                   if        %found(fnblp01l)
064600160526     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
064700160526     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
064800160526     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
064900160526     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
065000160526     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
065100160526     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
065200160527    6C     ARBFLP        IFGT      0
065300160526     C                   MOVE      ARBFLP        BLPFLP                         FILIALE TRANSITO
065400160527    6C                   ENDIF
065500160526     C                   MOVE      ARBDET        BLPDET
065600160526     C                   MOVE      ARBDUT        BLPDUT
065700160526     C                   MOVE      ARBdam        BLPdam
065800160526     C* X BOLLA ORIGINALE AGGIORNO TMC CON FLAG CONSEGNA ANOMALA 5
065900160526     C                   MOVE      ARBNDC        BLPNDC                         NR.DIST.CONS
066000160526     C                   MOVE      ARBDDC        BLPDDC                         DATA "  CONS
066100160527    6C     BLPCCA        IFNE      '5'
066200160526     C     BLPDCP        OREQ      0
066300160526     C                   MOVE      ARBDCP        BLPDCP                         DATA 1° CONS
066400160527    6C                   END
066500160526     C                   MOVE      ARBTMC        BLPTMC                         T.MANC.CONS.
066600160526     C                   MOVE      ARBDCM        BLPDCM
066700160526     C                   MOVE      ARBHMC        BLPHMC
066800160526      * Controllo se trasmettere dati cliente
066900160526     C                   EXSR      DATCLI
067000160526     c
067100160526     c                   update    fnblp000
067200160527    5c                   endif
067300160526     c
067400160526     C                   MOVE      LBLLPO        ARBLNP
067500970313     C                   MOVE      LBLAAO        ARBAAS
067600970313     C                   MOVE      LBLNRO        ARBNRS
067700970313     C                   MOVE      LBLNSO        ARBNSP
067800160527     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
067900970313     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
068000950330      * SE NON TRASMETTO A SEDE LA BOLLA FIGLIA NON TRASMETTO
068100950330      * NEANCHE LA BOLLA ORIGINALE
068200950330     C                   Z-ADD     1             §F
068300950330     C                   SETOFF                                       03
068400950330      * BOLLE DA NON INVIARE
068500950330     C     ARBCBO        LOOKUP    CDB(§F)                                03
068600160617     C**N03              ADD       1             NRSED             7 0
068700010103     C  N03              WRITE     FIARBC00                                     SEDE
068800160520    4C                   END
068900941025     C*
069000941025     C                   MOVEL     'T'           ARBFT1
069100941025     C                   MOVE      AMGUDT        ARBDT1
069200160520    3C                   ENDif
069300160520     c
069400160616     c                   if        singola=' '
069500941025     C                   EXCEPT    AGGOR
069600160616     c                   else
069700160616     C                   EXCEPT    AGGOR1
069800160616     c                   endif
069900160616     c
070000160520    2C                   ENDif
070100160520    1C                   ENDdo
070200941025      *
070300941025     C                   ENDSR
070400160616     C***--------------------------------------------***
070500160616     C* Elaboro soltanto la bolla passata
070600160616     C***--------------------------------------------***
070700160616     c     Ela_bolla     BEGSR
070800160616
070900160616     c                   eval      singola='S'
071000160616     c
071100160616    1c                   if        not %open(fnarb01l)
071200160616     c                   open      fnarb01l
071300160616    1c                   endif
071400160616     c
071500160616     c     kpbol         chain(e)  fnarb01l
071600160616    1c                   if        %error or not %found(fnarb01l)
071700160616     c                   eval      pa7err='E'
071800160616     c                   leavesr
071900160616    1c                   endif
072000160616     c
072100160616     C* APRO FILE X LA SEDE
072200160616     C                   EXSR      CAEX46
072300160616     c
072400160616    1c                   if        pa7rich<>'C'
072500160616     c                   exsr      EventiB
072600160616    1c                   endif
072700160616     c
072800160616     c* se tipo richiamo = 'C' --> solo ricalcolo affidabilità solo se consegnata
072900160616     c*                            altrimenti in tutti i casi previsti
073000160616
073100160616    0c                   if        (pa7rich='C' and arbdcm>0) or
073200160616     c                             (pa7rich<>'C' and (arbdcm>0 or arbdcp>0
073300160616     c                             or wtreve='S'))
073400160616     c
073500160616     c                   exsr      SD99_LBL
073600160616     c
073700160616    1c                   if        pa7rich<>'C'
073800160616     c                   exsr      Agg_ART
073900160616
074000160616     c                   exsr      Agg_ARBBLP
074100160616     c
074200160616   x1c                   else
074300160616     c
074400160616    2C     D98DCE        IFNE      ARBDCE
074500160616     C     D98NPI        ORNE      ARBNPI
074600160616     C     D98NCI        ORNE      ARBNCI
074700160616     C     D98NRC        ORNE      ARBNRC
074800160616     C                   Z-ADD     D98DCE        ARBDCE
074900160616     C                   Z-ADD     D98NPI        ARBNPI
075000160616     C                   Z-ADD     D98NCI        ARBNCI
075100160616     C                   Z-ADD     D98NRC        ARBNRC
075200160616     C                   MOVEL     'C'           ARBFAG
075300160616     C                   Z-ADD     1             §F
075400160616     C                   SETOFF                                       03
075500160616      * BOLLE DA NON INVIARE
075600160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
075700160616    3c                   if        not *in03
075800160617     C***                ADD       1             NRSED             7 0
075900160616     C                   WRITE     FIARBC00                                     SEDE
076000160616    3C                   ENDIF
076100160616     c
076200160616     c     karb          chain     fnblp01l
076300160616    3c                   if        %found(fnblp01l)
076400160616     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
076500160616     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
076600160616     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
076700160616     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
076800160616     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
076900160616     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
077000160616     c                   update    fnblp000
077100160616    3c                   endif
077200160616
077300160616     c                   update    fnarb001
077400160616
077500160616     c* Se la bolla ha la mamma --> aggiorno anche la bolla originale
077600160616    3c                   if        haorig='S' and hafigl=' '
077700160616     C     KARB          CHAIN     FNLBL01L
077800160616    4c                   if        %found(fnlbl01l)
077900160616     c     kblpo         chain     fnblp01l
078000160616    5c                   if        %found(fnblp01l)
078100160616     C                   MOVE      ARBDTI        BLPDTI                         DATA ARR.TRAZ.
078200160616     C                   MOVE      ARBHTI        BLPHTI                         ORA ARR. TRAZ.
078300160616     C                   MOVE      ARBDCE        BLPDCE                         DATA CONS. PREV.CL
078400160616     C                   MOVE      ARBNPI        BLPNPI                         NON CONF.PART.INT.
078500160616     C                   MOVE      ARBNCI        BLPNCI                         NON CONF.CONS.INT.
078600160616     C                   MOVE      ARBNRC        BLPNRC                         NON CONF.RESA CLI.
078700160616     c                   update    fnblp000
078800160616    5c                   endif
078900160616     c
079000160616     C                   MOVE      LBLLPO        ARBLNP
079100160616     C                   MOVE      LBLAAO        ARBAAS
079200160616     C                   MOVE      LBLNRO        ARBNRS
079300160616     C                   MOVE      LBLNSO        ARBNSP
079400160616     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
079500160616     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
079600160616      * SE NON TRASMETTO A SEDE LA BOLLA FIGLIA NON TRASMETTO
079700160616      * NEANCHE LA BOLLA ORIGINALE
079800160616     C                   Z-ADD     1             §F
079900160616     C                   SETOFF                                       03
080000160616      * BOLLE DA NON INVIARE
080100160616     C     ARBCBO        LOOKUP    CDB(§F)                                03
080200160617     C**N03              ADD       1             NRSED             7 0
080300160616     C  N03              WRITE     FIARBC00                                     SEDE
080400160616    4c                   endif
080500160616    3c                   endif
080600160616    2c                   endif
080700160616      *
080800160616    1C                   endif
080900160616    0C                   endif
081000160616
081100160616     c                   ENDSR
081200930510      *----------------------------------------------------------------
081300930510      * PULIZIA PARAMETRI X TRUL51C
081400930510      *----------------------------------------------------------------
081500930510     C     TRUL51        BEGSR
081600971120     C                   MOVEL     '*LIBL'       LIBR             10
081700930510     C                   MOVEL     *BLANK        FILE             10
081800930510     C                   MOVEL     *BLANK        MBR              10
081900930510     C                   Z-ADD     0             NRR              10 0
082000930510     C                   ENDSR
082100930415     C**----------------------------------
082200930415     C** LANCIO AZIONI DI TRASMISSIONE IN SEDE
082300930415     C**----------------------------------
082400930415     C     INVSED        BEGSR
082500930510     C                   EXSR      TRUL51
082600010103     C                   MOVEL     'FIARBC46'    FILE
082700020920     C                   MOVEL     §membro       MBR
082800930510     C                   CALL      'TRUL51C'
082900930510     C                   PARM                    LIBR
083000930510     C                   PARM                    FILE
083100930510     C                   PARM                    MBR
083200930510     C                   PARM                    NRR
083300930415     C     NRR           IFGT      0
083400971120     C     NRR           ANDNE     9999999999
083500971120     C* Richiamo TIBS50R per conoscere librerie/S.Informativi
083600971120     C* da/a cui spedire
083700971120     C                   CLEAR                   TIBS50
083800971120     C                   MOVEL     *BLANKS       I50TLA
083900971120     C                   MOVEL     KNSIF         I50PSI
084000971120     C                   MOVEL     SIMFEL        I50PPO
084100971120     C                   MOVEL     '046'         I50APO
084200020920     C                   CALL      'TIBS55R'
084300971120     C                   PARM                    TIBS50
084400971120     C* INVIO
084500930415     C                   EXSR      X25SED
084600930415     C                   END
084700930415     C                   ENDSR
084800911112     C**----------------------------------
084900911112     C** SOTTOMETTE TRASMISSIONE X LA SEDE
085000911112     C**----------------------------------
085100911112     C     X25SED        BEGSR
085200971120     C*
085300971120     C                   CLEAR                   W236
085400911112     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
085500010103     C                   MOVEL     'FIARBC46'    CMZFLD                          DA: >>>
085600160617     C                   MOVEL(p)  §membro       CMZMBD
085700971120     C                   MOVEL     O50PLA        CMZLBD
085800010103     C                   MOVEL     'FIARB46R'    CMZFLA                          A: >>>
085900020920     C                   MOVEL(p)  §membro       CMZMBA
086000020927     C                   MOVEL     O50ALA        CMZLBA
086100020927     C                   MOVEL     'GAITRA'      CMZLBA
086200020927     C                   MOVE      O50ALA        WQUATR            4
086300020927     C     WQUATR        IFEQ      '101 '
086400020927     C                   MOVE      '201 '        CMZLBA
086500020927     C                   ENDIF
086600010305     C                   MOVE      'R'           CMZURG
086700911112     C                   MOVE      'A'           CMZADD                          IN ADD
086800911112     C                   MOVE      'S'           CMZCLR                          CLEAR
086900911112     C                   MOVE      'S'           CMZFTR                          DA TRANS.
087000911112     C                   MOVE      *BLANK        CMZERR                          1= ERR.
087100010103     C                   MOVE      'TR92'        CMZAZN                          AZIONE
087200020927     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
087300020927     C                   MOVEL     'GAITRA'      CMZSYF
087400020927     C                   MOVE      O50ASI        WQUATR
087500020927     C     WQUATR        IFEQ      '101 '
087600020927     C                   MOVE      '201 '        CMZSYF
087700020927     C                   ENDIF
087800911112     C                   MOVE      NRR           CMZNRR                          N.REC.
087900911112     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
088000021007     C                   CALL      'TIBS53C'
088100971120     C                   PARM                    O50LCL
088200911112     C                   PARM                    CMZDST
088300911112     C                   PARM                    CMZFLD
088400911112     C                   PARM                    CMZMBD
088500911112     C                   PARM                    CMZLBD
088600911112     C                   PARM                    CMZFLA
088700911112     C                   PARM                    CMZMBA
088800911112     C                   PARM                    CMZLBA
088900911112     C                   PARM                    CMZORI
089000911112     C                   PARM                    CMZDTI
089100911112     C                   PARM                    CMZURG
089200911112     C                   PARM                    CMZADD
089300911112     C                   PARM                    CMZCLR
089400911112     C                   PARM                    CMZFTR
089500911112     C                   PARM                    CMZERR
089600911112     C                   PARM                    CMZAZN
089700911112     C                   PARM                    CMZSYF
089800911112     C                   PARM                    CMZNRR
089900911112     C                   PARM                    CMZPRR
090000160527     C                   PARM                    W236            236
090100971120     C*
090200911112     C                   ENDSR
090300160520     C**---------------------------------------------------------
090400160520     C** CONTROLLO SE DEVO TRASMETTERE I DATI AL CLIENTE da BLP
090500160520     C**---------------------------------------------------------
090600160520     C     DATCLI        BEGSR
090700160520      * SE IL CLIENTE VUOLE RICEVERE DATI CONSEGNA SFLEGGO CAMPI TRASMISS.
090800160520     C                   SETOFF                                       32
090900160520     C     BLPCCM        IFNE      0
091000160520     C                   Z-ADD     1             W
091100160520     C     BLPCCM        LOOKUP    CLI(W)                                 32
091200160520     C                   END
091300160520     C  N32BLPKSC        IFNE      0
091400160520     C                   Z-ADD     1             W
091500160520     C     BLPKSC        LOOKUP    CLI(W)                                 32
091600160520     C                   END
091700160520     C* SE ANCORA NON TROVATO CHAIN SU ar4 PER VERIFICARE SE ESISTE IL
091800160520      *CODICE DEL CLIENTE NEL CAMPO NOTE
091900160520     C  N32              DO
092000160520     C     Kar4          CHAIN     Fiar401L                           66
092100160520     C                   IF        not *in66
092200160520     C                   MOVEL     ar4NOT        KSCJ              7 0
092300160520     C                   Z-ADD     1             W
092400160520     C     KSCJ          LoOKUP    CLi(W)                                 32
092500160520     C                   END
092600160520     C                   END
092700160520      *
092800160520     C   32              MOVEL     *BLANK        BLPFT3
092900160520     C   32              MOVE      *ZEROS        BLPDT3
093000160520     C*
093100160520     C                   ENDSR
093200941025     C*****************************************************************
093300941025     C*     I N Z S R
093400941025     C*****************************************************************
093500941025     C     *INZSR        BEGSR
093600971120     C*
093700941025     C     *ENTRY        PLIST
093800941025     C                   PARM                    KPJBA
093900160616     C                   PARM                    param7b
094000160616     c
094100020920     C                   MOVEL     KPJBU         param7a
094200160520
094300950102     C     NMI7A         IFGT      '0000000'
094400950102     C                   MOVE      NMI7A         KNMI
094500020920     C                   clear                   Kipp
094600020920     C                   move      fgs7a         Kipp
094700950102     C                   ENDIF
094800160616     c
094900160616     c                   if        %parms=2 and pa7nsped>*zeros
095000160616     c                   monitor
095100160616     c                   eval      kpaas=%int(%subst(pa7nsped:1:4))
095200160616     c                   eval      kplnp=%int(%subst(pa7nsped:5:3))
095300160616     c                   eval      kpnrs=%int(%subst(pa7nsped:8:2))
095400160616     c                   eval      kpnsp=%int(%subst(pa7nsped:10:7))
095500160616     c                   on-error
095600160616     c                   clear                   kpaas
095700160616     c                   clear                   kplnp
095800160616     c                   clear                   kpnrs
095900160616     c                   clear                   kpnsp
096000160616     c                   endmon
096100160616     c                   endif
096200160616     c
096300160520
096400941025     C                   Z-ADD     1             CODUT
096500941025     C                   CALL      'X§PARUT'
096600941025     C                   PARM                    UTEDSE
096700941025     C                   MOVEL     REC80         CNCR80
096800941025     C*
096900020924     C     SIMTPP        IFEQ      '2'
097000020924     C     SIMTPP        OREQ      *BLANKS
097100020924     C                   MOVE      SIMPOU        §FLE              3 0
097200020924     C                   ELSE
097300020924     C     SIMFEL        COMP      0                                      02
097400020924     C  N02              MOVE      simfel        §fle
097500020924     C                   END
097600020925     C                   z-add     SIMFEL        xFLE
097700941025     C*§§
097800941025     C                   TIME                    WHHUDT           14 0
097900941025     C                   MOVE      WHHUDT        G08DAT
098000941025     C                   MOVE      *ZEROS        G08INV
098100941025     C                   MOVEL     *BLANK        G08ERR
098200941025     C                   CALL      'XSRDA8'
098300941025     C                   PARM                    WLBDA8
098400941025     C                   Z-ADD     G08INV        AMGUDT            8 0
098500941025     C*
098600950126     C*CARICO I CODICI BOLLE DA NON INVIARE IN SEDE
098700010305     C                   OPEN      TABEL00F
098800950126     C                   MOVEL     '3A'          COD
098900950126     C                   MOVE      *ZEROS        §F                2 0
099000950126     C     KTAB1         SETLL     TABEL
099100160608     C     KTAB1         READE     TABEL
099200160608     c                   dow       not %eof(tabel00f)
099300160608
099400160608     C     TBLFLG        IFEQ      *BLANK                                        ANNUL.
099500950126     C                   MOVEL     TBLUNI        DS3A
099600950126     C     §3ABSD        IFEQ      'N'                                           NO TRASM.IN SEDE
099700950126     C                   ADD       1             §F
099800950126     C                   MOVEL     TBLKEY        CDB(§F)
099900950126     C                   ENDIF
100000950126     C                   ENDIF
100100160608
100200160608     C     KTAB1         READE     TABEL
100300950126     C                   ENDDO
100400160520
100500160520     C*CARICO gli eventi IDD
100600970805     C                   MOVEL     '2A'          COD
100700970805     C                   MOVE      *ZEROS        §F                2 0
100800970805     C     KTAB1         SETLL     TABEL
100900160608     C     KTAB1         READE     TABEL
101000160608     c                   dow       not %eof(tabel00f)
101100160608
101200160608     C     TBLFLG        IFEQ      *BLANK                                        ANNUL.
101300970805     C                   MOVEL     TBLUNI        DS2A
101400970805     C     §2AIDD        IFNE      *BLANKS                                       NO TRASM.IN SEDE
101500970805     C                   ADD       1             §F
101600970805     C                   MOVEL     TBLKEY        IDD(§F)
101700970805     C                   ENDIF
101800970805     C                   ENDIF
101900160608     C     KTAB1         READE     TABEL
102000970805     C                   ENDDO
102100160520     C*                                                    *
102200160520     C* CARICO I CODICI CLIENTI A CUI MANDARE LE DATE DI CONSEGNA
102300160608     C                   MOVEL     '3K'          COD
102400160520     C                   MOVE      *ZEROS        W                 4 0
102500160520     C     Ktab1         SETLL     TABEL00F
102600160520     C     Ktab1         READE     TABEL00F                               31
102700160608     c                   dow       not %eof(tabel00f)
102800160608
102900160520     C                   MOVEL     TBLUNI        DS3K
103000160520     C                   ADD       1             W
103100160520     C                   MOVEL     TBLKEY        CLI(W)
103200160520      *
103300160608     C     Ktab1         READE     TABEL00F                               31
103400160608     C                   ENDdo
103500160608     c
103600941026     C                   CLOSE     TABEL00F
103700941025     C*
103800941025     C     KART          KLIST
103900941216     C                   KFLD                    ARBAAS
104000160527     C                   KFLD                    ARBLNP
104100941025     C                   KFLD                    ARBNRS
104200941025     C                   KFLD                    ARBNSP
104300160616     C     Kpbol         KLIST
104400160616     C                   KFLD                    kpAAS
104500160616     C                   KFLD                    kpLNP
104600160616     C                   KFLD                    kpNRS
104700160616     C                   KFLD                    kpNSP
104800160519     C     Kblt          KLIST
104900160519     C                   KFLD                    artAAS
105000160527     C                   KFLD                    artLNP
105100160519     C                   KFLD                    artNRS
105200160519     C                   KFLD                    artNSP
105300160519     C                   KFLD                    artNSC
105400161114     C     KbltO         KLIST
105500161114     C                   KFLD                    bltAAS
105600161114     C                   KFLD                    bltLNP
105700161114     C                   KFLD                    bltNRS
105800161114     C                   KFLD                    bltNSP
105900161114     C                   KFLD                    bltNSC
106000970805     C     KEVB          KLIST
106100990803     C                   KFLD                    KAAS
106200970805     C                   KFLD                    KLNP
106300970805     C                   KFLD                    KNRS
106400970805     C                   KFLD                    KNSP
106500941025     C*
106600970805     C     KARBF         KLIST
106700970805     C                   KFLD                    LBLAAN
106800970805     C                   KFLD                    LBLLPN
106900970805     C                   KFLD                    LBLNRN
107000970805     C                   KFLD                    LBLNSN
107100970805     C     KARBM         KLIST
107200970805     C                   KFLD                    LBLAAP
107300970805     C                   KFLD                    LBLLPP
107400970805     C                   KFLD                    LBLNRP
107500970805     C                   KFLD                    LBLNSP
107600160526     C     Kblpo         KLIST
107700160526     C                   KFLD                    LBLAAo
107800160526     C                   KFLD                    LBLLPo
107900160526     C                   KFLD                    LBLNRo
108000160526     C                   KFLD                    LBLNSo
108100941025     C     KARB          KLIST
108200941025     C                   KFLD                    ARBAAS
108300941025     C                   KFLD                    ARBLNP
108400941025     C                   KFLD                    ARBNRS
108500941025     C                   KFLD                    ARBNSP
108600941025     C*
108700950126     C     KTAB1         KLIST
108800950126     C                   KFLD                    CODUT
108900950126     C                   KFLD                    COD
109000020920     C*
109100020920     C     Karb72        KLIST
109200020920     C                   KFLD                    kipp
109300020920     C                   KFLD                    knmi
109400160520     C     Kar4          KLIST
109500160520     C                   KFLD                    blpAAS
109600160520     C                   KFLD                    blpLNP
109700160520     C                   KFLD                    blpNRS
109800160520     C                   KFLD                    blpNSP
109900160520     C                   KFLD                    TRECz             1
110000160520     c                   move      'Z'           trecz
110100971120     C*
110200941025     C* DEFINIZIONE CHIAVI
110300941025     C*
110400941025     C     *LIKE         DEFINE    TBLCOD        COD
110500941216     C     *LIKE         DEFINE    ARBNMI        KNMI
110600020920     C     *LIKE         DEFINE    ARBipp        Kipp
110700990803     C     *LIKE         DEFINE    EVBAAS        KAAS
110800970805     C     *LIKE         DEFINE    EVBLNP        KLNP
110900970805     C     *LIKE         DEFINE    EVBNRS        KNRS
111000970805     C     *LIKE         DEFINE    EVBNSP        KNSP
111100020920     C                   Z-ADD     43            LUNG43           15 5
111200020920     C                   Z-ADD     48            LUNG48           15 5
111300970314     C*
111400941025     C                   ENDSR
111500941021     OFNARB000  E            AGGB0
111600941025     O                       ARBNCI
111700941025     O                       ARBNPI
111800930521     O                       ARBDCE
111900941025     O                       ARBNRC
112000930414     O                       ARBFT1
112100941025     O                       ARBDT1
112200941025     OFNARB000  E            AGGOR
112300941025     O                       ARBFT1
112400941025     O                       ARBDT1
112500160520     o
112600941021     OFNART000  E            AGGT0
112700961113     O                       ARTFTR
112800160616     OFNARB001  E            AGGB1
112900160616     O                       ARBNCI
113000160616     O                       ARBNPI
113100160616     O                       ARBDCE
113200160616     O                       ARBNRC
113300160616     O                       ARBFT1
113400160616     O                       ARBDT1
113500160616     OFNARB001  E            AGGOR1
113600160616     O                       ARBFT1
113700160616     O                       ARBDT1
113800010103**         CMDC
113900020920CHKOBJ OBJ(FIARBC46) OBJTYPE(*FILE) MBR(M000000)
114000911111**         CMDA
114100020920ADDPFM FILE(FIARBC46)               MBR(M000000)
114200911111**         CMDO
114300020920OVRDBF FILE(FIARBC46)               MBR(M000000)
114400950411**         CMDL
114500020920ALCOBJ OBJ((FIARBC46 *FILE *EXCL  M000000))
114600950411**         CMDD
114700020920DLCOBJ OBJ((FIARBC46 *FILE *EXCL  M000000))
