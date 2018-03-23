000100891106     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891106     F*--------------------------------------------------------------*
000300971124     F* GESTIONE TABELLE CODICI AUTISTA                              *
000400891106     F*----------------------* USO DEGLI INDICATORI *----------------*
000500891106     F*  01        INSERIMENTO                                       *
000600891106     F*  02        VARIAZIONE                                        *
000700900629     F*  03        ANNULLATO
000800891129     F*  30        CODICE ERRATO                                     *
000900891129     F*  31        DESCRIZIONE OBBLIGATORIA                          *
001000050216     F*  34        campo OBBLIGATORIo                                *
001100891106     F*--------------------------------------------------------------*
001200011211     FTRmz40D   CF   E             WORKSTN
001300891106     FTABEL00F  UF A E           K DISK
001400070726     Fansif01l  iF   E           K DISK
001500070727     Fmzxnh01l  if a E           K DISK
001600891106     D*--------------------------------------------------------------*
001700891106     D*                        I N P U T                             *
001800891106     D*--------------------------------------------------------------*
001900971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
002000891106     D KPJBA         E DS
002100971124     D DSXN          E DS
002200120221     D trmz47ds      E DS                  inz
002300971124     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002400070919     d dtaeur          s               d   datfmt(*Eur)
002500070919     d dtaiso          s               d
002600070727     d comdta          s              8  0
002700070727     d savmat          s                   like(vmat)
002800070727     d savsoc          s                   like(vsoc)
002900891106     C******************************************************
003000891106     C     *ENTRY        PLIST
003100891106     C                   PARM                    KPJBA
003200891129     C                   MOVEL     KPJBU         VKEY
003300891106     C                   Z-ADD     1             CODUT
003400891106     C                   CALL      'XPARUT'
003500891106     C                   PARM                    UTEDSE
003600891106     C                   MOVEL     RAGUT         RSUT
003700891106     C******************************************************
003800891106     C* POSIZIONAMENTO TABELLE                             *
003900891106     C     KTAB          KLIST                                                  *
004000891106     C                   KFLD                    §KUT                           *
004100891106     C                   KFLD                    §COD                           *
004200891106     C                   KFLD                    §KEY                           *
004300070727     C     Kxnh          KLIST                                                  *
004400070727     C                   KFLD                    vcod                           *
004500070727     C                   KFLD                    comdta                         *
004600891106     C******************************************************
004700891106     C     *LIKE         DEFINE    TBLKUT        §KUT
004800891106     C     *LIKE         DEFINE    TBLCOD        §COD
004900891106     C     *LIKE         DEFINE    TBLKEY        §KEY
005000971210     C******************************************************
005100971210     C* Richiamo pgm per controllo modificabilità tabella
005200971210     C                   CLEAR                   TRUL12
005300971210     C                   MOVEL     'L'           I12TLA
005400971210     C                   MOVEL     KNSIF         I12NSI
005500971210     C                   MOVEL     'XN'          I12COD
005600971210     C                   CALL      'TRUL12R'
005700971210     C                   PARM                    TRUL12
005800971210     C     O12ERR        IFEQ      '1'
005900971210     C                   GOTO      FINE
006000971210     C                   END
006100971210     C******************************************************
006200891106     C     STAR          TAG
006300891106     C                   SETOFF                                       79
006400891106     C                   MOVEL     KNMUS         PROF              3
006500891106     C     PROF          IFNE      'EDP'
006600891106     C                   SETON                                        79
006700891106     C                   END
006800891106     C                   MOVEL     *BLANKS       §DES             25
006900070925     c                   move      kpjbu         vcod
007000070925     c                   move      kpjbu         PRMCOD            2
007100070925     c                   if        PRMCOD = ' '
007200891129     C                   MOVEL     *BLANKS       VCOD
007300891129     C                   MOVEL     '?'           VCOD
007400070925     c                   end
007500891106     C     INIZIO        TAG
007600070925     c                   if        PRMCOD= ' '
007700971124     C                   EXFMT     TB01D1
007800070919     C   Kc              GOTO      FINE
007900891129     C     VCOD          IFEQ      *BLANKS
008000891106     C                   SETON                                        30
008100891106     C                   GOTO      INIZIO
008200891106     C                   END
008300070925     C                   END
008400891106     C*§  RICERCA IN TABEL00F
008500891107     C     NOVID         TAG
008600891129     C     VCOD          IFEQ      '?'
008700891106     C                   MOVEL     CODUT         §KUT
008800891129     C                   MOVEL     VKEY          §COD
008900891129     C                   MOVE      *ZEROS        VCOD
009000071017     c                   clear                   §key
009100071017     c                   clear                   §des
009200891106     C                   CALL      'X§TABER'
009300891106     C                   PARM                    §KUT
009400891106     C                   PARM                    §COD
009500891106     C                   PARM                    §KEY
009600891106     C                   PARM                    §DES
009700891129     C                   MOVEL     §KEY          VCOD
009800891106     C                   GOTO      INIZIO
009900891106     C                   ELSE
010000891106     C                   MOVEL     CODUT         §KUT
010100891129     C                   MOVEL     VKEY          §COD
010200891121     C                   MOVEL     *BLANKS       §KEY
010300891129     C                   MOVEL     VCOD          §KEY
010400891106     C                   END
010500891106     C*
010600891106     C                   SETOFF                                       0102
010700900629     C                   EXSR      PULVID
010800891106     C     KTAB          CHAIN     TABEL                              80
010900891106     C     *IN80         IFEQ      '1'
011000891106     C                   SETON                                        01        *IMMISSIONE
011100891106     C                   ELSE
011200971124     C                   MOVEL     TBLUNI        DSXN
011300971124     C                   MOVEL     §XNDES        VDES
011400011211     C                   movel     §xnper        Vper
011500050216     C                   movel     §xnfun        Vfun
011600070727     C                   movel     §xnsoc        Vsoc
011700070727     C                   movel     §xnsoc        hsoc
011800070926     c                   if        %subst(tbluni: 39: 5) <> ' ' and
011900070926     c                             %subst(tbluni: 39: 5) <> '00000'
012000130131     C                   move      §xnmat        vmat
012100130131     C                   move      §xnmat        hmat
012200130131     c                   exsr      srmz47
012300070919     c                   eval      hdli = D47DLIO
012400070919     c                   eval      hcog = d47cogo
012500070802     c                   eval      hnom = d47nomo
012600070727     c                   else
012700070727     c                   clear                   vmat
012800070727     c                   clear                   hmat
012900070919     c                   clear                   hdli
013000070925     c                   seton                                        38
013100070727     c                   end
013200971124     C**
013300900629     C     TBLFLG        IFEQ      '*'
013400900629     C                   SETON                                        03        *ANNULLATO
013500900629     C                   ELSE
013600891106     C                   SETON                                        02        *VARIAZIONE
013700891106     C                   END
013800900629     C                   END
013900891106     C     SU            TAG
014000891106     C                   SETOFF                                       99
014100971124     C                   EXFMT     TB01D2
014200070919     C   Kc              GOTO      FINE
014300070925     c                   if        *inkl
014400070925     c                   if        PRMCOD = ' '
014500070925     C                   GOTO      INIZIO
014600070925     c                   ELSE
014700070925     C                   GOTO      FINE
014800070925     C                   END
014900070925     C                   END
015000070802     C**--- storico
015100070802     C   Kw              DO
015200070907     c                   move      vcod          kpjbu
015300070802     c                   call      'TRMZ48R'
015400070802     c                   parm                    kpjba
015500070919     c                   goto      su
015600070802     c                   enddo
015700070802     C**--- ANNULLAMENTO
015800070802     C   KQ              DO
015900900629     C                   MOVEL     '*'           TBLFLG
016000900629     C                   UPDATE    TABEL
016100900629     C                   GOTO      INIZIO
016200900629     C                   END
016300900629     C** RIPRISTINO
016400900629     C   KE              DO
016500900629     C                   MOVEL     *BLANKS       TBLFLG
016600900629     C                   UPDATE    TABEL
016700900629     C                   GOTO      INIZIO
016800900629     C                   END
016900900629     C**
017000900629     C                   EXSR      CTRFMT
017100971124     C  NKF
017200971124     COR 99              GOTO      SU
017300900629     C**
017400971124     C                   EXSR      SCRIVI
017500891129     C                   GOTO      STAR
017600891106     C     FINE          TAG
017700891106     C                   SETON                                        LR
017800900629     C**------------------------
017900070727     C** decodifica il dipendente
018000900629     C**------------------------
018100070727     C     srmz47        BEGSR
018200070727     C                   clear                   trmz47ds
018300070727     c                   eval      d47soc = vsoc
018400070727     c                   eval      d47mat = vmat
018500070727     c                   eval      kpjbu = trmz47ds
018600130131     c                   call      'TRMZ47R'
018700070727     c                   parm                    kpjba
018800070727     c                   eval      trmz47ds = kpjbu
018900070802     c                   if        d47erro <>' '
019000070727     c                   seton                                        3699
019100070727     c                   else
019200070919     c                   if        D47DLIO <>0
019300070919     c                   move      d47dlio       dtaiso
019400070919     c                   move      dtaiso        dtaeur
019500070919     c                   move      dtaeur        vdli
019600070919     c                   end
019700070919     c                   eval      dmatc = d47cogo
019800070727     c                   eval      dmatn = d47nomo
019900070919     c     D47DLIO       comp      0                                      38
020000070727     c                   end
020100900629     C                   ENDSR
020200070727     C**------------------------
020300070727     C** PULISCI CAMPI VIDEO
020400070727     C**------------------------
020500070727     C     PULVID        BEGSR
020600070926     c                   seton                                        38
020700070926     c                   clear                   hsoc
020800070926     c                   clear                   hmat
020900070926     c                   clear                   vsoc
021000070926     c                   clear                   vmat
021100070926     c                   movel     *hival        savsoc
021200070926     c                   movel     *hival        savmat
021300070727     C                   MOVEL     *BLANKS       VDES
021400070727     C                   MOVEL     *BLANKS       dmatc
021500070727     C                   MOVEL     *BLANKS       dmatn
021600070727     C                   ENDSR
021700900629     C**------------------------
021800900629     C** CONTROLLI 2 VIDEATA
021900900629     C**------------------------
022000900629     C     CTRFMT        BEGSR
022100070727     c                   setoff                                       313435
022200070727     c                   setoff                                       363233
022300900629     C** CONTROLLO DESCRIZIONE
022400900629     C     VDES          IFEQ      *BLANKS
022500900629     C                   SETON                                        3199
022600900629     C                   GOTO      ENDFMT
022700900629     C                   END
022800050216     C     Vfun          IFEQ      *BLANKS
022900050216     C                   SETON                                        3499
023000050216     C                   GOTO      ENDFMT
023100050216     C                   END
023200070926     c                   clear                   vdli
023300070926     c                   clear                   dmatc
023400070926     c                   clear                   dmatn
023500070727     C     Vsoc          IFEQ      ' '
023600070926     C*                  SETON                                        3599
023700070926     C*                  GOTO      ENDFMT
023800070726     c                   else
023900070726     C     vsoc          CHAIN     ansif01L
024000070726     C                   if        not %found(ansif01l)
024100070726     C                   SETON                                        3599
024200070726     C                   GOTO      ENDFMT
024300070726     C                   END
024400070726     C                   END
024500070727     C     Vmat          IFEQ      0
024600070926     C*                  SETON                                        3699
024700070926     C*                  GOTO      ENDFMT
024800070726     c                   else
024900130201     c                   exsr      srmz47
025000070919     c     D47DLIO       comp      0                                      38
025100070727     c   36              goto      endfmt
025200070726     c                   end
025300070726     c* controllo se variata società o matricola x scrivere storico
025400070727     c                   if        (vsoc <> hsoc and vsoc <> savsoc and
025500070727     c                             hsoc <> ' ' ) or
025600070727     c                             (vmat <> hmat and vmat <> savmat and
025700070727     c                             hmat <> 0 ) or
025800070727     c                             *inki
025900070726     c                   exsr      srwin
026000070727     c                   if        vdat <> *loval
026100070726     c                   eval      savsoc = vsoc
026200070726     c                   eval      savmat = vmat
026300070727     c                   else
026400070727     C                   SETON                                        99
026500070726     c                   end
026600070727     c                   end
026700900629     C**
026800900629     C     ENDFMT        ENDSR
026900891106     C**----------------------------
027000070726     C** storico
027100891106     C**----------------------------
027200070726     C     srwin         BEGSR
027300070919     C     hsoc          CHAIN     ansif01L
027400070919     C                   if        %found(ansif01l)
027500070919     c                   eval      dsoc= SIFDESBREV
027600070919     c                   else
027700070919     c                   clear                   dsoc
027800070919     c                   end
027900070919     c                   if        hdli <> 0
028000070919     c                   move      hdli          dtaiso
028100070919     c                   move      dtaiso        dtaeur
028200070919     c                   move      dtaeur        vdat
028300070919     c                   else
028400070919     c                   clear                   vdat
028500070919     c                   end
028600070726     C*
028700070726     c                   do        *hival
028800070726     c                   exfmt     TB01W1
028900070919     c                   if        *inkl
029000070726     c                   clear                   vdat
029100070726     c                   leave
029200070726     c                   end
029300070727     c                   move      vdat          dtaiso
029400070727     c                   move      dtaiso        comdta
029500070727     c     kxnh          chain     mzxnh01l
029600070727     c                   if        %found(mzxnh01l)
029700070727     c                   seton                                        37
029800070727     c                   iter
029900070727     c                   else
030000070727     c                   setoff                                       37
030100070727     c                   end
030200070727     c*
030300070727     c                   if        vdat <> *loval
030400070727     c                   leave
030500070727     c                   end
030600070726     c                   enddo
030700070726     C*
030800070726     C                   ENDSR
030900070726     C**----------------------------
031000070726     C** scrive storico
031100070726     C**----------------------------
031200070726     C     SCRIVIsto     BEGSR
031300070727     c                   move      vdat          dtaiso
031400070727     c                   move      dtaiso        comdta
031500070727     c     kxnh          chain     mzxnh01l
031600070727     c                   if        not %found(mzxnh01l)
031700070727     c                   move      dtaiso        xnhdta
031800070727     c                   eval      xnhcau = vcod
031900070727     c                   eval      xnhsoc = hsoc
032000070727     c                   eval      xnhmat = hmat
032100070727     c                   write     mzxnh000
032200070727     c                   end
032300070726     C                   ENDSR
032400070726     C**----------------------------
032500070726     C** AGGIORNAMENTO / INSERIMENTO
032600070726     C**----------------------------
032700070726     C     SCRIVI        BEGSR
032800891106     C     KTAB          CHAIN     TABEL                              80
032900891106     C                   MOVEL     §KUT          TBLKUT
033000891106     C                   MOVEL     §COD          TBLCOD
033100891121     C                   MOVEL     *BLANKS       TBLKEY
033200891106     C                   MOVEL     §KEY          TBLKEY
033300971124     C                   MOVEL     VDES          §XNDES
033400011211     C                   MOVEL     Vper          §XNper
033500050216     C                   MOVEL     Vfun          §XNfun
033600070727     C                   MOVEl     Vsoc          §XNsoc
033700070727     C                   MOVE      Vmat          §XNmat
033800971124     C                   MOVEL     DSXN          TBLUNI
033900070726     c* scrivo storico se variata società o matricola
034000070727     c                   if        (vsoc <> hsoc and hsoc <> ' ') or
034100070727     c                             (vmat <> hmat and hmat <> 0)
034200070726     c                   exsr      scrivisto
034300070726     c                   end
034400900420     C*§
034500070919     C                   clear                   TBLFTT
034600070919     C                   clear                   TBLFLT
034700900629     C                   MOVE      *BLANK        TBLFTR
034800900629     C                   MOVE      *ZERO         TBLDTR
034900900420     C*§
035000891106     C   80              WRITE     TABEL                                98
035100891106     C  N80              UPDATE    TABEL                                98
035200891106     C                   ENDSR
