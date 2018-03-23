000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200941107      * FNLR50R *----------------------------------------------------*
000300930428      *---------*                                                    *
000400911016      *     - GESTIONE PREPARAZIONE DISTINTA USCITA CONSEGNA -       *
000500960702     F*--------------------------------------------------------------*
000600160510     Ffiprm01l  IF   E           K DISK
000700941107     FFNLR50D   CF   E             WORKSTN
000800941107     F                                     SFILE(LR50S03:NRR)
000900911016     FTABEL00F  IF   E           K DISK
001000990920     FTNTBE01L  IF   E           K DISK
001100021204     Ffiapd01L  IF   E           K DISK
001200080326     Ffidsf02L  IF   E           K DISK
001300110224     FFipct02L  IF   E           K DISK
001400020905     FFNARB70L  IF   E           K DISK
001500020905     F                                     RENAME(FNARB000:FNARB70)
001600941107     FFNARB01L  UF   E           K DISK
001700070410     FFiarg01L  UF a E           K DISK
001800990920     FFIAR601L  IF   E           K DISK
001900051115     FFIAR901L  IF   E           K DISK
002000990920     FFIARI01L  IF   E           K DISK
002100941107     FFNARBW1L  UF A E           K DISK
002200941107     FFNARBW2L  UF   E           K DISK
002300941107     F                                     RENAME(FNARBW00:FNARBW2)
002400020905     FFNFVV01L  UF A E           K DISK
002500941107     FFNLBL01L  IF   E           K DISK
002600941107     FFNLBL02L  IF   E           K DISK
002700941107     F                                     RENAME(FNLBL000:FNLBL02)
002800990519     FFNANM01L  IF   E           K DISK
002900990519     F                                     INFDS(FNANM1)
003000941107     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003100011121     Ffiftt02L  IF   E           K DISK
003200011121     Ffifst02L  IF   E           K DISK
003300011121     F                                     RENAME(fiftt000:fifst000)
003400950607     FAZCLN01L  IF   E           K DISK
003500990803     FFNEVB04L  UF A E           K DISK
003600060214     FFiAR401L  IF   E           K DISK
003700070118     FFNBRV01L  IF   E           K DISK
003800010403     FFNORM06L  IF   E           K DISK
003900031020     Ffnart01l  IF   E           K DISK
004000040217     FFiar501L  IF   E           K DISK
004100041007     FFiqdt01L  uf a E           K DISK
004200070410     FFidst09L  uf a E           K DISK    prefix(£)
004300070312     FFidst31L  uf a E           K DISK
004400080121     FFidst51L  uf a E           K DISK
004500070312     Ffiapd22L  IF   E           K DISK
004600070312     Ffiapd31L  IF   E           K DISK
004700080121     Ffiapd51L  IF   E           K DISK
004800940321      *---------------------------------------------------------------
004900950613     D L£6             S              3  0 DIM(50)                              LINEE £6
005000950613     D LIN             S              3  0 DIM(29)                              LINEE ARRIVI
005100911022     D UNI             S              3    DIM(29)                              LINEE DA TABEL
005200911021     D LMN             S              3  0 DIM(29)                              FILIALI
005300170203     D ERR             S             70    DIM(29) CTDATA PERRCD(1)             ERRORI A VIDEO
005400100203     D s7r             S              2    DIM(100)                             CODICE TABELLA 3A
005500100514     d s7rc            S              2    DIM(100)                             CODICE TABELLA 3A
005600100203     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
005700941109     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
005800941108     D C1P             S              1    DIM(50)                              CODICI TABELLA 1P
005900941108     D D1P             S             25    DIM(50)                              DESCR. TABELLA 1P
006000950606     D MES             S              1    DIM(31)
006100160719     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
006200940321      *---------------------------------------------------------------
006300940321      *  Schiere per gestione caricamento Stop
006400940321      *---------------------------------------------------------------
006500070312     d* schiere file fiapd20f
006600070312     D Ptc1            S               *
006700070312     D                                     INZ(%ADDR(apd2tc1))
006800070312     D tc1             S                   LIKE(apd2tc1)
006900070312     D                                     DIM(5)
007000070312     D                                     BASED(Ptc1)
007100070312     D Pts1            S               *
007200070312     D                                     INZ(%ADDR(apd2ts1))
007300070312     D ts1             S                   LIKE(apd2ts1)
007400070312     D                                     DIM(2)
007500070312     D                                     BASED(Pts1)
007600070312     d* schiere file fidst00f
007700070312     D Ptc1d           S               *
007800070312     D                                     INZ(%ADDR(£dsttc1))
007900070312     D tc1d            S                   LIKE(£dsttc1)
008000070312     D                                     DIM(5)
008100070312     D                                     BASED(Ptc1d)
008200070312     D Pts1d           S               *
008300070312     D                                     INZ(%ADDR(£dstts1))
008400070312     D ts1d            S                   LIKE(£dstts1)
008500070312     D                                     DIM(2)
008600070312     D                                     BASED(Pts1d)
008700070312     d*
008800961217     D STP             S              3  0 DIM(999)                             1° schiera Stop
008900961217     D STP2            S              3  0 DIM(999)                             2° schiera Stop
009000961217     D RSD2            S             70    DIM(999)                             2° schiera Rag.soc.
009100080410     c* ds file
009200160316     D dnsd          E DS
009300160316     D FVVDS         E DS                  EXTNAME(FNFVV00F)
009400070312     D fidstds       E DS                  INZ extname(fidst00f) prefix(£)
009500070312     D fidstds2      E DS                  INZ extname(fidst00f) prefix($)
009600070312     D fiapd2ds      E DS                  extname(fiapd20f)
009700070410     D fiargds       E DS                  extname(fiarg00f)
009800080410     D fnarbds       E DS                  extname(fnarb00f)
009900100203     D dARGFLO       E DS
010000160510     D fidg17ds      E DS
010100080410      * DS per parcel/messaggerie su FNFVV
010200050523     D dfvvspn       E DS
010300050523      * DS per parcel/messaggerie su FIAPD
010400050523     D dapdflr       E DS
010500080208      * DS filler  FIdst
010600080208     D ddstflr       E DS
010700080226      * DS per richiamo pgm fnlrd1r x cambio aut. se pda orm partito
010800080226     D fnlrd1ds      E DS
010900080226      * DS per richiamo pgm fnlr99r x gestione ORM in particolarità RC
011000080226     D fnlr99ds      E DS
011100031125      * DS per richiamo pgm fnlr98r x richiamo pgm per togliere bolla da distinta
011200031125     D fnlr98ds      E DS
011300141112     D fnlr98ds2     E DS
011400030630      * DS per richiamo pgm FNLR50R1
011500030630     D FNLR50        E DS                  EXTNAME(FNLR50DS)
011600080410      * DS per richiamo pgm FNLR6R controllo numeratore distinta
011700020906     D FNLR6ds       E DS
011800130424      * DS per richiamo pgm TNSD99R calcolo affidabilità
011900130424     D tnsd99ds      E DS
012000080410      * DS per richiamo pgm FNLR36R interrogazione bolle
012100941108     D DSLR36          DS
012200941108     D  P36AAS                14     17  0
012300941108     D  P36LNP                18     20  0
012400941108     D  P36NRS                21     22  0
012500941108     D  P36NSP                23     29  0
012600941108     D  P36F03                30     30
012700941108     D  P36FLG                31     31
012800941109      * DS per richiamo pgm FNLR55R
012900941109     D FNLR55        E DS                  EXTNAME(FNLR55DS)
013000941110      * DS per richiamo pgm FNLR02R
013100160719     D*DSLR01        E DS
013200160719      * DS per richiamo pgm Fidg31R
013300160719     D fidg31ds      E DS
013400021204      * DS per richiamo pgm FNLV24R
013500021204     D fnlv24ds      E DS
013600941110      * DS per richiamo pgm FNLR39R
013700941110     D DSLR02        E DS
013800041008      * DS  pgm fnlrf8r x gestione bancali affidati rientrati e colli ritirati
013900041008     D fnlrf8ds      E DS
014000080410      * DS  pgm fidg37r x aggiornare i valori della distinta                ti
014100070517     D fidg37ds      E DS
014200080410      * DS  pgm fidg09r x controllare giro tellus                           ti
014300070725     D fidg09ds      E DS
014400080410      * DS  pgm fidg44r x forzatura distinta con fase CUS                   ti
014500080410     D fidg44ds      E DS
014600080410      * DS  pgm fidg42r x aggiornare i dati a PDA x spedizione              ti
014700080410     D fidg42ds      E DS
014800141029     D fnlry09ds2    E DS
014900080410     D* ds tabelle
015000990921     D YEURDS        E DS                  EXTNAME(YEURCODS)
015100990920     D DGED          E DS
015200000929     D DDGD          E DS
015300160502     D DS03          E DS
015400990920     D DSCV          E DS
015500911106     D DS3A          E DS
015600941108     D DS1P          E DS
015700100203     D DS7r          E DS
015800050520     D og150         E DS
015900070309     D OG146         E DS                  INZ
016000080208     D OG148         E DS                  INZ
016100080121     D OG147         E DS                  INZ
016200911016     D KPJBA         E DS
016300080410      * ds interne pgm
016400000620     D OG143         E DS
016500020905     c* numeratori
016600020905     d trul33ds      e ds
016700020905     c*
016800131031     d dEVB01        e ds
016900131031     c*
017000020509     d trul06ds      e ds
017100020509     d  lint                   1     90  0 dim(30)
017200911016      *
017300941107     D WLBDA8          DS
017400941107     D  G02DAT                 1      8  0
017500941107     D  G02INV                 9     16  0
017600941107     D  G02ERR                17     17
017700941107     D  G02TGI                18     22  0
017800911017      *
017900020419     d Wdata8          DS
018000020419     d  dadata                 1      8  0
018100020419     d  adata                  9     16  0
018200020419     d  GioLav                17     21  0
018300020419      *
018400920306     D DATPAR          DS
018500941107     D  GIODAT                 1      8  0
018600941107     D  GIOINV                 9     16  0
018700941107     D  GIOTGI                17     21  0
018800950123     D                 DS
018900950123     D  V1CFPD                 1      3  0
019000950123     D  V1CCPC                 4      7
019100950123     D  WCDCPC                 1      7
019200960415     D DSLR33        E DS                  EXTNAME(FNLR33DS)
019300990519     D FNANM1          DS
019400960415     D  ANMNR1               397    400B 0
019500020905     D ddatiute      e ds
019600020905     D azuteds       e ds                  extname(AZUTE00F)
019700020905     D tibs34ds      E DS                  inz
019800000515     D DSBL4A        E DS
019900170203     D fnlr50wDS     E DS
020000170203     d   savpjbu       s                   like(kpjbu)
020100090211     D*DFAR          E DS
020200090211     D* §FARDS       E                     EXTFLD(D§FARDES)
020300090211     D* §FARFB       E                     EXTFLD(D§FARFFB)
020400090211     D* §FARNF       E                     EXTFLD(D§FARNONF)
020500090211     D* §FARMN       E                     EXTFLD(D§FARMAN)
020600090211     D* §FARST       E                     EXTFLD(D§FARSTP)
020700090211     D* §FARAF       E                     EXTFLD(D§FARAFVV)
020800090211     D* §FARCF       E                     EXTFLD(D§FARCFVV)
020900030618     c* ds per sottocampare il barcode della spedizione
021000030618     D ds_pis          DS
021100030618     D  ds_pisaas                     2  0
021200030618     D  ds_pislnp                     3  0
021300030618     D  ds_pisnrs                     2  0
021400030618     D  ds_pisnsp                     7  0
021500030623     D  ds_chkdgt                     1  0
021600030623     D trul28ds      E DS
021700040217     D Dar5gen       E DS
021800150929     D Dar5emd       E DS
021900030623     d savchk          s                   like(o28ckd)
022000030623     d barcode         s             14
022100070312     d wnfv            s                   like(£dstnfv)
022200070612     d wpes            s                   like(arbpkf)
022300070612     d wvol            s                   like(arbvlf)
022400160510     d oggi            s               D
022500940321      *---------------------------------------------------------------
022600941109     IAZORG01L  AA
022700941109     I                             P    4    5 0ORGFIL
022800971119     I                                 11   11  ORGFL1
022900000620     I                               3776 3800  ORGDE3
023000070309     I                               3851 3875  ORGDe6
023100080121     I                               3876 3900  ORGDe7
023200080208     I                               3901 3925  ORGDe8
023300070309     I                               3951 3975  ORGDF0
023400911016      *---------------------------------------------------------------*
023500911016      * FLUSSO PRINCIPALE                                             *
023600911016      *---------------------------------------------------------------*
023700941107      *  Loop Principale
023800941107     C     *INKC         DOWEQ     '0'
023900941107     C                   EXSR      GESD01
024000941107     C                   END                                                    *do hival
024100911016     C*
024200911016     C*                  FINE LAVORO
024300911018     C     FINE          TAG
024400960415     C* RICHIAMO PGM PER CHIUSURA FILES
024500960415     C     W33R          IFEQ      '1'
024600960415     C                   MOVEL     'C'           D33TLA
024700960415     C                   CALL      'FNLR33R'
024800960415     C                   PARM                    DSLR33
024900960415     C                   END
025000911128      *
025100911018     C                   SETON                                        LR
025200941107     C*----------------------------------------------------*
025300941107     C* Gestione Primo formato                             *
025400941107     C*----------------------------------------------------*
025500941107     C     GESD01        BEGSR
025600941107     C*
025700941107     C                   Z-ADD     0             NUM
025800040217     C                   SETOFF                                       464267
025900100203     C                   SETOFF                                       48
026000030701     c* v1cisp mi servono per sapere se voglio inserire  le
026100080605     c* spedizioni a mano o con la pistola. ora forzo P perchè oramai
026200080605     c* tutti i po stampano le bolle su laser.
026300030701     c                   movel     'P'           v1cisp
026400030618     C*
026500950215     C                   EXFMT     LR50D01
026600941107     C                   MOVEL     *BLANKS       $MSG
026700941107     C                   SETOFF                                       901113
026800950123     C                   SETOFF                                       171219
026900040217     C                   SETOFF                                       464767
027000080208     C                   SETOFF                                       751069
027100941107     C                   Z-ADD     0             ULTSTP
027200941107     C                   Z-ADD     0             W                 3 0
027300941107     C                   Z-ADD     0             STP2
027400951121     C                   MOVEA     *BLANKS       RSD2
027500941107?    C* F3 - Fine lavoro
027600941107     C     *INKC         IFEQ      '1'
027700941107     C                   GOTO      FINVD1
027800941107     C                   END
027900950907?    C* Controllo se pgm non e richiamato da gestione fogli consegna
028000950907     C     D50TPR        IFEQ      'C'
028100950907     C                   EXSR      PGMRIC
028200950907     C                   ELSE
028300950907     C                   EXSR      PGMMNU
028400950907     C                   END
028500941107     C*
028600941107     C     FINVD1        ENDSR
028700950907     C*----------------------------------------------------*
028800950907     C* Pgm. richiamato da menù                            *
028900950907     C*----------------------------------------------------*
029000950907     C     PGMMNU        BEGSR
029100950907     C*
029200030804?    C* F2 - toglie spedizioni da distinta
029300030804     C     *INKB         IFEQ      '1'
029400110125     c                   clear                   fnlr98ds
029500110125     c                   movel(p)  fnlr98ds      kpjbu
029600030804     c                   call      'FNLR98C'
029700030804     c                   parm                    kpjba
029800020905     C                   MOVE      *ZEROS        SAVNDC
029900950907     C                   GOTO      FINMNU
030000950907     C                   END
030100140924?    C* F19- Richiama pgm Disposizioni via WEB
030200140924     C     *inkt         IFEQ      '1'
030300141029     c                   clear                   fnlry09ds2
030400140924     c                   eval      ilry09fgs = v1clna
030500140924     c                   eval      ilry09tch = 'E'
030600141013     c                   exsr      verdispo
030700141029     c                   if        OLRY09ESAL = '1'
030800140926     c                   seton                                        97
030900141001     c                   else
031000141001     c                   setoff                                       97
031100140926     c                   endif
031200140930     C                   GOTO      FINMNU
031300140924     c                   endif
031400030804?    C* F18 - Cambio Filiale
031500030804     C     *INKS         IFEQ      '1'
031600030804     C*??
031700030804     C                   MOVE      *ZEROS        SAVNDC
031800030804     C                   SETOFF                                       20
031900030804     C                   GOTO      FINMNU
032000030804     C                   END
032100950907     C*
032200950907     C     *INKA         IFEQ      '1'
032300080411     C                   MOVEL     £dstpdr       SAVCPCorm         7
032400080226     C                   MOVEL     WCDCPC        SAVCPC            7
032500950907     C                   SETON                                        03
032600950907     C                   END
032700020906     C* controlli po in gestione
032800950907     C                   EXSR      CONTR1
032900020905     c   90              goto      finmnu
033000020906     C* Controllo il numeratore 12 che non sia bloccato
033100020906     C                   CLEAR                   TRUL33DS
033200020906     c                   eval      i33tla = 'L'
033300020906     C                   Z-ADD     0             I33AAA
033400020906     C                   Z-ADD     1             I33ope
033500020906     C                   Z-ADD     12            I33CNU
033600020906     C                   Z-ADD     v1clna        I33PO1
033700020906     C                   Z-ADD     1             I33NUM
033800020906     c                   movel(p)  trul33ds      kpjbu
033900020906     c                   call      'TRUL33R'
034000020906     c                   parm                    kpjba
034100020906     c                   movel     kpjbu         trul33ds
034200020906     c* record numeratore allocato
034300020906     C                   IF        O33ERR = 02
034400020906     C                   SETON                                        9013
034500020906     C                   MOVEL     ERR(12)       $MSG
034600020906     C                   GOTO      FINMNU
034700020906     C                   END
034800020905?    C* F11 - Sblocco Distinte in preparazione
034900020905     C     *INKK         IFEQ      '1'
035000020905     C                   CLEAR                   FNLR50
035100020905     C                   Z-ADD     V1CLNA        D50FIL
035200020906     C                   TESTN                   V1CNDC               3131
035300020905     C   31              MOVE      V1CNDC        D50NDT
035400020905     C                   MOVEL     FNLR50        KPJBU
035500020905     C                   CALL      'FNLR50R1'
035600020905     C                   PARM                    KPJBA
035700020905     C                   END
035800020906     C* controlli successivi
035900020906     C                   EXSR      CONTR2
036000020906     c   90              goto      finmnu
036100950907     C*
036200950907     C* Controllo se digitato il numero distinta vado in modifica
036300020905     C     V01NDC        IFNE      *ZEROS
036400070312     C* alloco il file delle distinte FIDST00F
036500070312     c                   exsr      srlckdst
036600070312     c   90              goto      finmnu
036700950907     C* carico il file di lavoro con le sped.riguardanti la distinta
036800950907     C     V01NDC        IFEQ      SAVNDC
036900950907     C*   Alloco la distinta
037000020905     C     KFVV          CHAIN     FNFVV01L                           12
037100030926     c                   if        not *in12 and fvvatb<>'*'
037200030926     c                   if        fvvfcf <> ' '
037300030926     C                   SETON                                        9012
037400030926     C                   MOVEL     ERR(14)       $MSG
037500030926     c                   unlock    fnfvv01l
037600030926     c   90              goto      finmnu
037700030926     c                   end
037800030926     C                   MOVEL     'A'           FVVFCF
037900030926     C                   UPDATE    FNFVV000
038000950907     C                   EXSR      CARDIS
038100950907     C                   EXSR      GESSBF
038200950907     C                   MOVE      *ZEROS        SAVNDC
038300950907     C                   Z-ADD     V01NDC        FVVNFV
038400030926     c                   end
038500950907     C                   ELSE
038600950907     C                   Z-ADD     V01NDC        SAVNDC            7 0
038700950907     C                   END
038800950907     C*
038900950907     C                   ELSE
039000950907     C     TAG01         TAG
039100950907     C                   SETOFF                                       93
039200950907     C*
039300950907     C* Compilazione distinta con inserimento singola spedizione
039400950907     C                   MOVE      *ZEROS        SAVAAS
039500950907     C                   MOVE      *ZEROS        SAVLNP
039600950907     C                   MOVE      *ZEROS        SAVNRS
039700950907     C                   MOVE      *ZEROS        SAVNSP
039800170307     C                   MOVE      'N'           WForza            1
039900950907     C  N90              EXSR      GESFM3
040000950907     C                   MOVE      *ZEROS        SAVNDC
040100950907     C                   END                                                    *if modifica
040200950907     C* Se nelle videate precedenti hanno richiesto una videata
040300950907     C* particolare ritorno al controllo del tipo compilazione
040400950907     C   93              GOTO      TAG01
040500950907     C*
040600950907     C     FINMNU        ENDSR
040700080410     C*----------------------------------------------------*
040800080410     C* se pda e cambio aut. richiamo FNLRD1               *
040900080410     C*----------------------------------------------------*
041000080410     C     srchgcpc      BEGSR
041100080410     C* richiamo pgm per cambiare aut sui record FIPDO00T
041200080801     c                   if        savcpcorm <> wcdcpc
041300140212     c                             and savcpcorm <> ' '
041400080226     c                   clear                   fnlrd1ds
041500080226     c                   eval      LRD1FGS = v1clna
041600080226     c                   eval      LRD1NDC = v01ndc
041700080226     c                   movel     savcpcorm     LRD1PDRO
041800080226     c                   movel     wcdcpc        LRD1PDRN
041900080226     c                   call      'FNLRD1R'
042000080226     c                   parm                    fnlrd1ds
042100140212     c                   clear                   savcpcorm
042200080226     c                   end
042300080410     C                   ENDSR
042400080226     C*----------------------------------------------------*
042500080226     C* Pgm. richiamato da gestione fogli consegna         *
042600080226     C*----------------------------------------------------*
042700080226     C     PGMRIC        BEGSR
042800070315     C                   Z-ADD     4             FVVNPG
042900070315     C                   Z-ADD     D50NDT        FVVNFV
043000070315     C                   Z-ADD     D50FIL        FVVFGS
043100070312     C* alloco il file delle distinte FIDST00F
043200070312     c                   exsr      srlckdst
043300070312     c   90              goto      finric
043400950907     C*   Alloco la distinta
043500020905     C     KFVV          CHAIN     FNFVV01L                           12
043600030926     c                   if        not *in12 and fvvatb<>'*'
043700030926     c                   if        fvvfcf <> ' '
043800030926     C                   SETON                                        9012
043900030926     C                   MOVEL     ERR(14)       $MSG
044000030926     c                   unlock    fnfvv01l
044100030926     c   90              goto      finric
044200030926     c                   end
044300950907     C                   MOVE      FVVDFV        ROVDDC
044400950907     C                   MOVEL     'A'           FVVFCF
044500950907     C                   UPDATE    FNFVV000
044600950907     C*   Reperisco decodifica padroncino
044700950907     C                   MOVE      WCDCPC        APDPDR
044800021204     C     kapd          CHAIN     fiapd01L                           13
044900950907     C                   MOVE      APDRSC        V05PAD
045000950907     C                   MOVE      APDRSC        V06PAD
045100950908     C                   MOVE      WCDCPC        CODPAD            7 0
045200950907     C*------------------------------------------------------*
045300950907     C*  Controllo sul calendario il tipo di giorno che è la
045400950907     C*  data di consegna
045500950907     C     ROVDDC        IFNE      0
045600950907     C                   Z-ADD     ROVDDC        WMMGG             4 0
045700950907     C                   Z-ADD     ROVDDC        WG                2 0
045800950907     C                   MOVEL     ROVDDC        KANN
045900950907     C                   Z-ADD     0             KTFP
046000950907     C                   Z-ADD     0             KTFA
046100950907     C                   MOVEL     WMMGG         KMES
046200950907     C     KCLN          CHAIN     AZCLN01L                           31
046300950907     C                   MOVEA     CLNMAT        MES
046400950907     C                   MOVE      MES(WG)       WGG               1
046500950907     C                   END
046600950907     C*------------------------------------------------------*
046700950907     C                   EXSR      CARDIS
046800950907     C* Compilazione distinta con inserimento singola spedizione
046900950907     C     TAG02         TAG
047000950907     C                   SETOFF                                       93
047100950907     C                   MOVE      *ZEROS        SAVAAS
047200950907     C                   MOVE      *ZEROS        SAVLNP
047300950907     C                   MOVE      *ZEROS        SAVNRS
047400950907     C                   MOVE      *ZEROS        SAVNSP
047500170307     C                   MOVE      'N'           WForza            1
047600950907     C  N90              EXSR      GESFM3
047700950907     C                   MOVE      *ZEROS        SAVNDC
047800950907     C* Se nelle videate precedenti hanno richiesto una videata
047900950907     C* particolare ritorno al controllo del tipo compilazione
048000950907     C   93              GOTO      TAG02
048100950907     C                   END
048200950907     C*
048300030926     C     finric        ENDSR
048400070312     C*----------------------------------------------------*
048500070312     C* alloco il record della distinta sul file FIDST     *
048600070312     C*----------------------------------------------------*
048700070312     C     srlckdst      BEGSR
048800070312     c                   eval      wnfv = fvvnfv
048900070410     C     KDST2         CHAIN(e)  fidst09l
049000070312     c                   if        %error
049100070312     C                   SETON                                        9012
049200070312     C                   MOVEL     ERR(14)       $MSG
049300070312     c                   end
049400070612     c                   if        %found(fidst09l)
049500070830     c                   z-add     £DSTPKL       V02PTO
049600070830     c                   z-add     £DSTVLU       V02CTO
049700070612     c                   else
049800070612     c                   clear                   v02pto
049900070612     c                   clear                   v02cto
050000070612     c                   end
050100070312     C                   ENDSR
050200020906     C*----------------------------------------------------*
050300020906     C* CONTROLLO FORMATO 1: po di gestione                *
050400020906     C*----------------------------------------------------*
050500020906     C     CONTR1        BEGSR
050600040901     C* controllo e inverto la data distinta (G02INV)
050700040901     c                   exsr      srddc
050800040901     c* no errore data proseguo
050900040901     c                   if        not *in90
051000040901     C* Controllo la filiale che cambia tutta la mia linea di arrivo
051100941108     C     *IN21         IFEQ      '1'                                          *
051200020509     c                   clear                   trul06ds
051300020509     c                   eval      d06cod = '£1'
051400020916     c                   movel     simfel        d06key
051500040901     c                   move      'S'           d06key
051600040901     C                   Z-ADD     G02INV        d06drf
051700020509     c                   movel     trul06ds      kpjbu
051800020509     c                   call      'TRUL06R'
051900020509     c                   parm                    kpjba
052000020509     c                   movel     kpjbu         trul06ds
052100020701     c                   movea     lint          Lmn
052200941107     C* Controllo se esiste la filiale da me digitata
052300941108     C     V1CLNA        LOOKUP    LMN                                    70
052400940503     C     *IN70         IFEQ      '0'
052500940503     C                   SETON                                        9017
052600940503     C                   MOVEL     ERR(6)        $MSG
052700941107     C                   ELSE
052800950613     C* Controllo se filiale gestita da altra
052900950613     C     V1CLNA        LOOKUP    L£6                                    70
053000950613     C     *IN70         IFEQ      '1'
053100950613     C                   SETON                                        9017
053200950613     C                   MOVEL     ERR(18)       $MSG
053300950613     C                   ELSE
053400941107     C* Recupero le linee di arrivo dalla tabella £6
053500941108     C                   MOVE      V1CLNA        FILIA
053600941107     C* Aggancio la £6 e recupero le linee a loro associate
053700020509     c                   clear                   trul06ds
053800020509     c                   eval      d06cod = '£6'
053900020509     c                   movel     filia         d06key
054000040831     c                   move      'S'           d06key
054100040901     C                   Z-ADD     G02INV        d06drf
054200020509     c                   movel     trul06ds      kpjbu
054300020509     c                   call      'TRUL06R'
054400020509     c                   parm                    kpjba
054500020509     c                   movel     kpjbu         trul06ds
054600020509     c                   movea     lint          Lin
054700941108     C                   END
054800911021     C                   END
054900950613     C                   END
055000040901     C                   END
055100140926     c                   setoff                                       97
055200141029     c                   clear                   fnlry09ds2
055300140926     c                   eval      ilry09fgs = v1clna
055400140926     c                   eval      ilry09tch = 'C'
055500141013     c                   exsr      verdispo
055600141029     c                   if        OLRY09ESAL = '1' and
055700140926     c                             OLRY09ERR  = *blank
055800140926     c                   seton                                        97
055900140926     c                   endif
056000050520     c* parcel/messaggerie
056100050520     c                   if        not *in17
056200050520     c                   exsr      srtpm
056300050520     c                   end
056400080208     C                   ENDSR
056500020906     C*----------------------------------------------------*
056600050520     C* CONTROLLO parcel / messaggerie                     *
056700020906     C*----------------------------------------------------*
056800050520     C     srtpm         BEGSR
056900070412     c                   setoff                                       98
057000080208     c* leggo organigramma x gestire ind 04 parcel+messaggerie
057100050520     c     v1clna        chain     azorg01l
057200050520     c                   if        %found(azorg01l)
057300070309     c                   movel     orgde6        og146
057400080208     c                   movel     orgde8        og148
057500080121     C                   MOVEL     ORGde7        og147
057600080212     c* se procedura PDA non partita proteggo i flag
057700080212     c                   if        §OGPDAorm = ' ' and §OGPDACON = ' '
057800080212     c                   setoff                                       71
057900080212     c                   else
058000080212     c                   seton                                        71
058100080212     c                   end
058200070313     c                   if        §ogdda <> ' '
058300070313     c                   movel     §ogdda        comdda            8 0
058400070412     c                   if        comdda <= dateu
058500070419     c*                  seton                                        98
058600070412     c                   end
058700070313     c                   else
058800070313     c                   clear                   comdda
058900070313     c                   end
059000070309     c                   movel     orgdf0        og150
059100050520     c     §OGPKM        comp      'S'                                    04
059200160429     c     §OGturno      comp      'S'                                    01
059300050520     c                   end
059400160510     c*
059500160510     c                   exsr      srprm
059600050520     C*
059700050520     C                   ENDSR
059800050520     C*----------------------------------------------------*
059900050520     C* CONTROLLO FORMATO 1                                *
060000050520     C*----------------------------------------------------*
060100050520     C     CONTR2        BEGSR
060200941109     C*
060300941109     C* NUMERO FOGLIO VIAGGIO
060400941109     C     V1CNDC        IFNE      *BLANKS
060500941109     C     V1CNDC        ANDNE     *ZEROS
060600941109     C*
060700020906     C* ricerca
060800941109     C     '?'           SCAN      V1CNDC                                 31
060900941109     C     *IN31         IFEQ      '1'
061000160719     C                   CLEAR                   fidg31ds
061100160802     C                   Z-ADD     0             FDGGDA
061200160727     C                   Z-ADD     0             FDGGAL
061300160719     C                   Z-ADD     V1CLNA        fdgFGS
061400160719     C                   MOVEL     'FNLR50R'     fdgPGM
061500160802     C                   MOVEL     'I'           fdgRIC
061600160719     C                   MOVEL     '2'           fdgTFV
061700160719     C                   Z-ADD     4             fdgNPG
061800160802     C                   move      'T'           fdgtpv
061900160719     C                   MOVEL     fidg31ds      KPJBU
062000160719     c                   clear                   $FV6
062100160719      * impostato a tutti 99999
062200160719     c                   do        600           yy                3 0
062300160719     c                   z-add     999999        $FV6(yy)
062400160719     c                   enddo
062500160719     C                   CALL      'FIDG31R'
062600941109     C                   PARM                    KPJBA
062700160719     c                   parm                    $FV6
062800160719     C                   MOVEL     KPJBU         fidg31ds
062900950215     C                   MOVE      *ZEROS        V1CNDC
063000160719     C                   MOVE      fdgNFV        V1CNDC
063100941109     C                   END
063200020906     C* controlli
063300941109     C                   TESTN                   V1CNDC               31
063400020918     c                   if        not *in31
063500020918     C                   SETON                                        9012
063600020918     C                   MOVEL     ERR(13)       $MSG
063700020918     C                   Z-ADD     *ZEROS        V01NDC
063800020918     C                   ELSE
063900020918     C                   MOVE      V1CNDC        V01NDC
064000020918     C                   END
064100020918     C                   ELSE
064200941109     C                   Z-ADD     *ZEROS        V01NDC
064300950601     C                   MOVEL     *BLANKS       V1CPEP
064400941109     C                   END
064500941109     C*  Controllo il numero di distinta se diverso da zero
064600941109     C*  deve esistere in archivio flarb00 o flarb50
064700941109     C*
064800960604     C  N90V01NDC        IFNE      *ZEROS
064900941109     C* Controllo se esiste in archivio FLFVV
065000941109     C                   Z-ADD     V01NDC        FVVNFV
065100020905     C                   Z-ADD     V1cLNA        FVVFGS
065200941109     C                   SETOFF                                       49
065300020905     C     KFVV          CHAIN     FNFVV01L                           1249
065400020906     C* rilascio
065500950207     C  N12
065600950207     CANN49              UPDATE    FNFVV000
065700950207     C  N12
065800950207     CANN49V01NDC        IFNE      SAVNDC
065900941228     C                   MOVEL     FVVFPP        V1CFPP
066000950601     C                   MOVEL     FVVPEP        V1CPEP
066100050523     C                   MOVEL     FVVspn        dfvvspn
066200150804     c                   if        *in04
066300160429     C                   MOVEL     §fvvtpm       v1ctpm
066400150804     c                   else
066500150804     c                   clear                   v1ctpm
066600150804     c                   end
066700150804     c                   if        v1ctpm = ' '
066800050523     C                   MOVEL     'P'           v1ctpm
066900050523     c                   end
067000160429     c* turno
067100160429     c                   if        *in01
067200160429     C                   MOVEL     §fvvturno     v1ctur
067300160429     c                   else
067400160429     c                   clear                   v1ctur
067500160429     c                   end
067600160429     c                   if        v1ctur = ' ' and §fvvtpm = ' '
067700160429     C                   MOVEL     '1'           v1ctur
067800160429     c                   end
067900941228     C                   END
068000941109     C     *IN49         IFEQ      '1'
068100941109     C                   SETON                                        9013
068200941109     C                   MOVEL     ERR(12)       $MSG
068300941109     C                   END
068400960604     C* Se numero distinta > 5 chr do errore
068500960604     C                   MOVEL     V01NDC        WCOMO2            2 0
068600960604     C  N12WCOMO2        IFGT      0
068700960604     C                   SETON                                        9012
068800960604     C                   MOVEL     ERR(13)       $MSG
068900960604     C                   END
069000960604     C*
069100941109     C     *IN12         IFEQ      '1'
069200941109     C     FVVATB        OREQ      '*'
069300941109     C     FVVFCF        OREQ      'S'
069400941109     C                   SETON                                        9012
069500941109     C                   MOVEL     ERR(13)       $MSG
069600941109     C                   END
069700941109     C  N90              DO
069800950613     C     FVVFCF        IFNE      ' '
069900941109     C                   SETON                                        9012
070000941109     C                   MOVEL     ERR(14)       $MSG
070100941109     C                   END
070200020906     C* in gestione ad altra filiale
070300020906     C*    FVVFGS        LOOKUP    LIN                                    31
070400020906     C*    *IN31         IFEQ      '0'
070500020906     C*                  SETON                                        9012
070600020906     C*                  MOVEL     ERR(15)       $MSG
070700020906     C*                  END
070800941109     C*
070900951010     C     V01NDC        IFNE      SAVNDC
071000941109     C                   MOVE      FVVDFV        ROVDDC
071100941109     C                   MOVE      FVVDFV        G02INV
071200941109     C                   MOVEL     '3'           G02ERR
071300941109     C                   CALL      'XSRDA8'
071400941109     C                   PARM                    WLBDA8
071500941109     C                   Z-ADD     G02INV        ROVDDC
071600941109     C                   Z-ADD     G02DAT        V1CDDC
071700941109     C                   MOVEL     ROVDDC        WAA4              4 0
071800941109     C                   MOVE      WAA4          AAA
071900941109     C                   END
072000951010     C                   END
072100941109     C*
072200941109     C  N90              DO
072300020905     C                   Z-ADD     V1CLNA        KFGS
072400020905     C                   Z-ADD     V01NDC        KNDC
072500960702     C                   Z-ADD     ROVDDC        KDDC
072600080121     c                   eval      wnfv = kndc
072700020905     C  N12KARB          CHAIN     FNARB70L                           92
072800080121     c* imposto flag pda
072900080208     C     V01NDC        IFNE      SAVNDC
073000080121     C     KDST3         CHAIN(N)  FiDST09L
073100080121     c                   if        %found(fidst09l)
073200080208     c                   eval      ddstflr = £dstflr
073300080208     c                   eval      v1ctstpda = §dsttstpda
073400160429     c                   eval      v1cpda = £dstpda
073500080121     c                   else
073600080121     c                   clear                   v1cpda
073700080208     c                   clear                   v1ctstpda
073800080121     c                   end
073900080319     c* verifico fasi distinta e se presenti ORM
074000080320     c                   exsr      srfasi
074100080410     c                   end
074200120718     C  N12              MOVEL     £DSTPDR       ARBPDC
074300941109     C*
074400950601     C     *IN92         IFEQ      '0'
074500950601     C     V1CCPC        IFEQ      *ZEROS
074600941109     C     V1CCPC        OREQ      *BLANKS
074700950123     C                   MOVE      ARBPDC        WCDCPC
074800941109     C                   END
074900120806     c                   else
075000120806     C     V1CCPC        IFEQ      *ZEROS
075100120806     C     V1CCPC        OREQ      *BLANKS
075200120806     C                   MOVE      £dstpdr       WCDCPC
075300120806     C                   END
075400950601     C                   END
075500950601     C*
075600941109     C                   SETOFF                                       70
075700941109     C  N92ARBLNA        LOOKUP    LIN                                    70
075800941109     C  N92
075900020905     CANN70dutlpo        IFEQ      '2'
076000941109     C                   SETON                                        9012
076100941109     C                   MOVEL     ERR(13)       $MSG
076200941109     C                   END
076300941109     C                   END
076400941109     C*
076500941109     C                   END
076600020906     C* DATA DISTINTA
076700941107     C  N90V1CDDC        IFEQ      *ZEROS
076800940503     C                   SETON                                        9011
076900940503     C                   MOVEL     ERR(7)        $MSG
077000911016     C                   ELSE
077100911017     C* routine di controllo data distinta
077200040901     c                   exsr      srddc
077300040901     C     G02ERR        IFne      '1'
077400951010     C* IN MODIFICA NON POSSO VARIARE LA DATE DISTINTA
077500951010     C  N90V01NDC        IFNE      *ZEROS
077600951010     C     G02INV        ANDNE     FVVDFV
077700951010     C                   SETON                                        9011
077800951010     C                   MOVEL     ERR(19)       $MSG
077900951010     C                   END
078000941107     C                   Z-ADD     G02DAT        V1CDDC
078100940503     C                   END
078200950418     C*
078300971119     C* CONTROLLO SE LNA ITALIA O  ESTERO
078400971119     C                   MOVEL     *BLANKS       ORGFL1
078500971119     C                   MOVE      V1CLNA        ORGFIL
078600160429     C     ORGFIL        CHAIN     AZORG01L
078700050711     c                   movel     orgde3        og143
078800020729     C                   MOVEL     *blank        flgest            1
078900020729      * se estero imposto il flag
079000160429     c                   if        %found(azorg01l)
079100160429     C     §OgNTW        ifeq      'FED'
079200020729     C     §OgNTW        oreq      'EEX'
079300020729     C     §OgNTW        oreq      'EUP'
079400020729     C                   MOVEL     'E'           flgest
079500020729     c                   end
079600160429     c                   end
079700050711      *?  inserimento  DISTINTA ESTERO TESTO LA DATA
079800050711     C     V1CNDC        IFeq      *BLANKS
079900050711     C     flgest        ANDeq     'E'
080000050711     C     V1CNDC        OReq      *ZEROS
080100050711     C     flgest        andeq     'E'
080200050711      *controllo range fra le date di oggi e quella immessa
080300050711     c                   eval      dadata = g02inv
080400050711     c                   eval      adata  = dateu
080500050711     c                   CALL      'XSRLAV8'
080600050711     c                   PARM                    Wdata8
080700050711     c                   if        giolav > §dgdgg
080800050711     c                   SETON                                        9011
080900050711     c                   MOVEL     ERR(23)       $MSG
081000050711     c                   end
081100050711     c                   end
081200020419     C* inserimento  PER NO DISTINTA ESTERO TESTO LA DATA
081300020419     C     V1CNDC        IFeq      *BLANKS
081400020729     C     flgest        ANDNE     'E'
081500020419     C     V1CNDC        OReq      *ZEROS
081600020729     C     flgest        andne     'E'
081700020419      *non ammessa data minore di oggi
081800020419     c                   if        g02inv < dateu
081900020419     c                   SETON                                        9011
082000020419     c                   MOVEL     ERR(24)       $MSG
082100020419     c                   endif
082200020419      *controllo range fra le date di oggi e quella immessa
082300020419     c                   eval      dadata = dateu
082400020419     c                   eval      adata  = g02inv
082500020419     c                   CALL      'XSRLAV8'
082600020419     c                   PARM                    Wdata8
082700020419     c                   if        giolav > §dgddc
082800020419     c                   SETON                                        9011
082900020419     c                   MOVEL     ERR(23)       $MSG
083000020419     c                   end
083100020419     c                   end
083200020419     C* manutenzione  PER NO DISTINTA ESTERO TESTO LA DATA
083300020419     C     V1CNDC        IFne      *BLANKS
083400020729     C     flgest        andne     'E'
083500020419     C     V1CNDC        ORne      *ZEROS
083600020729     C     flgest        andne     'E'
083700920306     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
083800950418     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
083900990823     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO 1 15 GG
084000990823     C                   MOVE      DATEU         WMGUDT            4 0
084100990823     C                   MOVEL     WMGUDT        WMUDT             2 0
084200000929     C                   Z-ADD     §DGDGG        NRGIO             3 0
084300000929     C  N90WMGUDT        IFGE      §DGDEI
084400000929     C     WMGUDT        ANDLE     §DGDEF
084500000929     C                   Z-ADD     §DGDGE        NRGIO
084600000929     C                   END
084700000929     C  N90DATEU         IFGE      §DGDII
084800000929     C     DATEU         ANDLE     §DGDIF
084900000929     C                   Z-ADD     §DGDGI        NRGIO
085000000929     C                   END
085100000929     C     G02TGI        SUB       NRGIO         GIOTGI
085200941107     C                   CALL      'XSRGI8'
085300920306     C                   PARM                    DATPAR
085400950418     C                   Z-ADD     GIOINV        DATM15            8 0
085500950418     C     DATM15        IFGT      DATEU
085600950418     C                   SETON                                        9011
085700950418     C                   MOVEL     ERR(16)       $MSG
085800950418     C                   END
085900000929     C     G02TGI        ADD       NRGIO         GIOTGI
086000941107     C                   CALL      'XSRGI8'
086100920306     C                   PARM                    DATPAR
086200950418     C                   Z-ADD     GIOINV        DATP15            8 0
086300950418     C     DATP15        IFLT      DATEU
086400950418     C                   SETON                                        9011
086500950418     C                   MOVEL     ERR(16)       $MSG
086600950418     C                   END
086700950418     C*
086800920306     C                   END
086900920306     C*
087000941107     C  N90              Z-ADD     G02INV        ROVDDC            8 0
087100950418     C  N90              MOVEL     ROVDDC        WAA4              4 0
087200950418     C  N90              MOVE      WAA4          AAA
087300911016     C                   END
087400950607     C*------------------------------------------------------*
087500950607     C*  Controllo sul calendario il tipo di giorno che è la
087600950607     C*  data di consegna
087700950609     C     ROVDDC        IFNE      0
087800950607     C                   Z-ADD     ROVDDC        WMMGG             4 0
087900950607     C                   Z-ADD     ROVDDC        WG                2 0
088000950607     C                   MOVEL     ROVDDC        KANN
088100950607     C                   Z-ADD     0             KTFP
088200950607     C                   Z-ADD     0             KTFA
088300950607     C                   MOVEL     WMMGG         KMES
088400950607     C     KCLN          CHAIN     AZCLN01L                           31
088500950607     C                   MOVEA     CLNMAT        MES
088600950607     C                   MOVE      MES(WG)       WGG               1
088700950609     C                   END
088800020906     C*PADRONCINO:
088900020906     c* il po di gestione e il po padroncino devono essere uguali
089000020906     C     V1CFPD        IFNE      V1CLNA
089100020906     C                   SETON                                        9019
089200020906     C                   MOVEL     ERR(17)       $MSG
089300020906     C                   END
089400020906     C* Ricerca padroncino
089500950123     C  N90V1CCPC        IFNE      *BLANKS
089600950123     C     V1CCPC        ANDNE     *ZEROS
089700911113     C                   SETOFF                                           31
089800941107     C     '?'           SCAN      V1CCPC                                 31
089900911104     C   31              DO
090000021204     c                   clear                   fnlv24ds
090100021204     C                   MOVEL     'A'           d24tip
090200021204     C                   MOVEL     'R'           d24FLG
090300021204     C                   MOVEL     V1CFPD        d24FIL
090400941107     C                   MOVE      *BLANKS       V1CCPC
090500021204     C                   MOVEL(p)  fnlv24ds      KPJBU
090600941108     C                   CALL      'FNLV24R'
090700921028     C                   PARM                    KPJBA
090800021204     C                   MOVEL     KPJBU         fnlv24ds
090900021204     C     d24PDR        IFNE      *ZEROS
091000021204     C                   MOVEL     d24PDR        WCDCPC
091100911104     C                   END
091200950123     C*
091300950123     C                   END
091400911104     C                   END
091500911017     C*
091600911017     C* Controllo il codice padroncino se corretto
091700911017     C*
091800941107     C  N90V1CCPC        IFNE      *ZEROS
091900941107     C     V1CCPC        ANDNE     *BLANKS
092000950217     C* Controllo se esiste nell'anagrafica padroncini
092100950217     C                   SETOFF                                       3132
092200950217     C                   TESTN                   WCDCPC               31
092300950217     C  N31              SETON                                        32
092400950217     C  N32              MOVE      WCDCPC        APDPDR
092500021204     C  N32kapd          CHAIN     fiapd01L                           13
092600000728     C  N32
092700000728     CANN13APDATB        IFNE      ' '
092800000728     C                   SETON                                        13
092900000728     C                   ENDIF
093000170203      ***
093100170203      *   Controlla se Disaccreditato
093200170203      ***
093300170203     c                   if        not *in13
093400170208     c                             and  hPdrAutista <> apdPdr
093500170203     c                   clear                   fnlr50wds
093600170208     c                   clear                   hPdrAutista       7 0
093700170203     C                   eval      savpjbu = kpjbu
093800170203     c                   z-add     apdPDR        §50autista
093900170203     c                   z-add     rovDDC        §50datadis
094000170203     c                   eval      §50WINDOW  = 'F'
094100170203     C                   eval         kpjbu = fnlr50wds
094200170203      ***
094300170206     C                   write     LR50D01
094400170203     C                   call      'FNLR50WR'
094500170203     C                   parm                    kpjba
094600170203     C                   eval      fnlr50wds = kpjbu
094700170203     C                   eval      kpjbu = savpjbu
094800170203      ***
094900170208     c                   IF        §50esito ='1'
095000170208     c                   IF        §50funz  ='F12'
095100170203     C                   SETON                                        9013
095200170203     C                   MOVEL     ERR(29)       $MSG
095300170203     c                   leavesr
095400170203     c                   endif
095500170208     c                   IF        §50funz  ='F07'
095600170208     c                   eval        hPdrAutista = §50autista
095700170208     c                   endif
095800170208     c                   endif
095900170203      ***
096000170203     c                   end
096100170203      ***
096200940503     C     *IN13         IFEQ      '1'
096300950217     C     *IN32         OREQ      '1'
096400940503     C                   SETON                                        9013
096500940503     C                   MOVEL     ERR(9)        $MSG
096600940503     C                   ELSE
096700941109     C                   MOVE      APDRSC        V05PAD
096800941109     C                   MOVE      APDRSC        V06PAD
096900950123     C                   MOVE      WCDCPC        CODPAD            7 0
097000950123     C                   MOVEL     V1CFPD        WFIL              3 0
097100950613     C     WFIL          LOOKUP    LIN                                    37
097200950613     C     *IN37         IFEQ      '0'
097300940503     C                   SETON                                        9013
097400940503     C                   MOVEL     ERR(10)       $MSG
097500940503     C                   END
097600950601     C*  Se picking non presente su foglio viaggio lo imposto da qui
097700950601     C     V1CPEP        IFEQ      *BLANKS
097800950601     C     APDPKM        IFEQ      'S'
097900950601     C     V1CFPP        ANDEQ     'M'
098000950601     C     APDPKP        OREQ      'S'
098100950601     C     V1CFPP        ANDEQ     'P'
098200950601     C                   MOVEL     'S'           V1CPEP
098300950601     C                   ELSE
098400950601     C                   MOVEL     'N'           V1CPEP
098500950601     C                   END
098600950601     C                   END
098700080121     c                   movel     apdflr        dapdflr
098800080410     c******************** Dati PDA  ****************************
098900080121     c                   if        v1cpda = *blanks
099000080121     c                   movel     §apdpda       v1cpda
099100101021     c                   movel     §apdtstpda    v1ctstpda
099200080709     c                   end
099300080121     c                   if        v1cpda = *blanks
099400080121     c                   movel     'N'           v1cpda
099500101021     c                   clear                   v1ctstpda
099600080121     c                   end
099700080212     c* controllo flag dati per PDA se attivo
099800080212     c                   if        *in71
099900080208     C                   if        (v1cpda = 'O' and
100000080208     c                             §OGPDAORM = ' ') or
100100080208     C                             (v1cpda = 'C' and
100200080208     c                             §OGPDACON = ' ') or
100300080208     C                             (v1cpda = 'E' and
100400080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
100500080208     C                   MOVEL     err(25)       $msg
100600080208     c                   select
100700080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
100800080208     c                   eval      $msg = %trim($msg)+' CONSEGNE'
100900080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
101000080208     c                   eval      $msg = %trim($msg) + ' ORM'
101100080208     c                   endsl
101200080208     C                   SETON                                        1090
101300080208     C                   ENDIF
101400080318     c* se distinta già preparata per pda non posso cambiare il flag x con.
101500080318     c* se ho la fase PDC
101600081111     c                   if        (£dstpda = 'E' or £dstpda ='C') and
101700081111     c                             (v1cpda ='N' or v1cpda='O') and
101800080320     c                             fasecon ='PDC'
101900080318     c                   seton                                        1090
102000080318     C                   MOVEL     err(27)       $msg
102100080318     c                   end
102200080318     c* se distinta già preparata per pda non posso cambiare il flag x orm
102300080318     c* se ho il FIPCO00T
102400081111     c                   if        (£dstpda = 'E' or £dstpda ='O') and
102500081111     c                             (v1cpda = 'N' or v1cpda ='C') and
102600080320     c                              faseorm ='PDR'
102700080318     c                   seton                                        1090
102800080318     C                   MOVEL     err(27)       $msg
102900080318     c                   end
103000080208     c* controllo flag test per PDA
103100080208     C                   if        (v1ctstpda = 'O' and
103200080208     c                             §OGPDAORM = ' ') or
103300080208     C                             (v1ctstpda = 'C' and
103400080208     c                             §OGPDACON = ' ') or
103500080208     C                             (v1ctstpda = 'E' and
103600080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
103700080208     C                   MOVEL     err(25)       $msg
103800080208     c                   select
103900080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
104000080208     c                   eval      $msg = %trim($msg) + ' CONSEGNE'
104100080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
104200080208     c                   eval      $msg = %trim($msg) + ' ORM'
104300080208     c                   endsl
104400080208     C                   SETON                                        6990
104500080208     C                   ENDIF
104600080326     c* controllo la congruenza dei valori impostati nel test x pda e
104700080326     c* e nei dati x pda
104800080326     C                   if        v1ctstpda <>' '
104900080326     C                   if        (v1cpda = 'O' and
105000080326     C                             v1ctstpda <>'O') or
105100080326     C                             (v1cpda = 'C' and
105200080326     C                             v1ctstpda <>'C') or
105300080326     C                             v1cpda = 'N'
105400080326     C                   MOVEL     err(26)       $MSG
105500080326     C                   SETON                                        6990
105600080326     C                   ENDIF
105700110224     c* si può inserire il flag di test C/E solo se non si sono ricevuti
105800110224     c* esiti consegne (se già inseriti ma non ancora ricevuti, paziena !!!)
105900110224     c                   if        §dsttstpda = ' ' and
106000110224     c                             v1ctstpda <> 'O'
106100110225     c     kpct          setll     fipct02l                               69
106200110225     c                   if        *in69
106300110225     C                   MOVEL     err(27)       $MSG
106400110225     C                   SETON                                        90
106500110224     c                   end
106600110224     c                   end
106700110224     c* se si era in test con Entrambi e si vuole passare in buona con uno
106800110224     c* dei due, non si può fare
106900110530     c*                  if        §dsttstpda = 'E' and
107000110530     c*                            v1ctstpda <> 'E'
107100110530     C*                  MOVEL     err(26)       $MSG
107200110530     C*                  SETON                                        9069
107300110530     c*                  end
107400110224     c* non è possibile eliminare il flag di test
107500110224     c                   else
107600110224     c                   if        §dsttstpda <> ' ' and
107700110225     c                             (faseorm ='PDR' or fasecon = 'PDC')
107800110225     C                   MOVEL     err(27)       $msg
107900110225     C                   SETON                                        9069
108000110224     c                   end
108100110224     C                   ENDIF
108200080208     c* se procedura attiva e aut. in test avviso
108300080208     C                   if        (v1ctstpda ='E' and
108400080211     C                             (§OGPDAORM ='S' OR §OGPDACON='S'))
108500080208     C                             or (v1ctstpda ='C' and
108600080208     C                             §OGPDACON='S')
108700080208     C                             or (v1ctstpda ='O' and
108800080208     C                             §OGPDAORM='S')
108900080208     C                   eval      msgv= 'ATT. gestione PDA attiva'
109000080208     c                   else
109100080208     c                   clear                   msgv
109200080208     C                   ENDIF
109300080318     c* se aut. ha dati per pda differenti da quelli della distinta avviso
109400080319     c                   clear                   msgv1
109500080415     c                   setoff                                       45
109600101025     C                   if        (v1cpda = 'N' and
109700080319     C                             v1cpda <> §apdpda)
109800080415     C                   eval      msgv1= 'ATTENZIONE Aut. con PDA'
109900080415     c                   seton                                        45
110000080319     c                   end
110100101025     C                   if        (v1cpda <> 'N' and
110200080319     C                             §apdpda = 'N')
110300080415     C                   eval      msgv1= 'ATTENZIONE Aut.senza PDA'
110400080415     c                   seton                                        45
110500080319     c                   end
110600080212     C                   ENDIF
110700080121     c* parcel/messaggerie
110800050524     c                   if        v1ctpm = *blanks
110900150804     c                   if        *in04
111000150804     c                   movel     §apdtpm       v1ctpm
111100150804     c                   end
111200150804     c                   if        v1ctpm = ' '
111300050523     c                   movel     'P'           v1ctpm
111400050523     c                   end
111500050523     c                   end
111600160429     c* turno
111700160505     c                   if        v1ctur = *blanks and v1ctpm = 'P'
111800160502     c                   if        *in01
111900160429     c                   movel     §apdturno     v1ctur
112000160505     c                   end
112100160429     c                   if        v1ctur = ' '
112200160429     c                   movel     '1'           v1ctur
112300160429     c                   end
112400160429     c                   end
112500160505     c                   if        v1ctpm = 'M' and v1ctur <> ' '
112600160505     c                             and *in01
112700160505     C                   MOVEL     ERR(28)       $MSG
112800160505     c                   seton                                        9190
112900160505     c                   end
113000950601     C*
113100941109     C  N90APDPDD        IFNE      'S'
113200931109     C*  Se padroncino non è dipedente controllo se in filiale
113300931109     C*  di appartenza è attivata la gestione conteggi
113400941109     C                   MOVE      WFIL          ORGFIL
113500160429     C     ORGFIL        CHAIN     AZORG01L
113600160429     C                   IF        %found(azorg01l)
113700950123     C                   MOVE      WCDCPC        FTWPDR
113800931109     C                   MOVE      'C'           FTWFVL
113900931109     C                   MOVE      ' '           FTWTSR
114000931109     C                   MOVE      ROVDDC        FTWDDC
114100160429     C     KEYFT2        CHAIN     fiftt02L
114200160429     C     KEYFT2        CHAIN     fifst02L
114300160429     C                   if        %found(fiftt02l) or
114400160429     C                             %found(fifst02l)
114500940503     C                   SETON                                        9011
114600940503     C                   MOVEL     ERR(8)        $MSG
114700931109     C                   END
114800940503     C                   END
114900931109     C                   END
115000911024     C                   END
115100080318     c* se cambio l'aut ed ho spedizioni o orm sulla distinta chiedo di
115200080318     c* confermare la modifica
115300080319     C     V01NDC        IFNE      *ZEROS
115400941108     C     ARBPDC        ANDNE     CODPAD
115500950123     C     SAVCPC        ANDNE     WCDCPC
115600080319     C     *in92         ANDeq     '0'
115700080318     C     V01NDC        orNE      *ZEROS
115800080326     C     ARBPDC        ANDNE     CODPAD
115900080319     C     SAVCPC        ANDNE     WCDCPC
116000080326     C     worm          ANDeq     'S'
116100940503     C     *IN03         IFEQ      '0'
116200940503     C                   SETON                                        9013
116300940503     C                   MOVEL     ERR(11)       $MSG
116400940503     C                   ELSE
116500940503     C                   SETON                                        90
116600940503     C                   MOVEL     ERR(11)       $MSG
116700940503     C                   SETOFF                                       03
116800911104     C                   END
116900940503     C                   END
117000911017     C*
117100911105     C                   END
117200950123     C   92
117300950123     CANN90V1CCPC        IFEQ      *ZEROS
117400950123     C     V1CCPC        OREQ      *BLANKS
117500941107     C     V01NDC        IFNE      *ZEROS
117600940503     C                   SETON                                        9013
117700940503     C                   MOVEL     ERR(9)        $MSG
117800911108     C                   END
117900911108     C                   END
118000950123     C  N90V1CCPC        IFEQ      *ZEROS
118100950123     C     V1CCPC        OREQ      *BLANKS
118200941107     C     V01NDC        IFEQ      *ZEROS
118300940503     C                   SETON                                        9013
118400940503     C                   MOVEL     ERR(9)        $MSG
118500911104     C                   END
118600911105     C                   END
118700030618     c* accendo o spengo l'indicatore 36 per gestire in seguito il campo
118800030618     c* dove andrà il barcode della spedizione
118900030618     c     v1cisp        comp      'P'                                    36
119000030703     c                   if        v1cisp = 'P'
119100030703     c                   eval      v03f2 = 'F2=Manuale'
119200030703     c                   else
119300030703     c                   eval      v03f2 = 'F2=Pistola'
119400030703     c                   end
119500911017     C*
119600911017     C                   ENDSR
119700911017     C*----------------------------------------------------*
119800040901     C* Controllo data distinta                            *
119900911017     C*----------------------------------------------------*
120000040901     C     srddc         BEGSR
120100911017     C*
120200040901     C                   MOVE      V1CDDC        G02DAT
120300040901     C                   MOVEL     *BLANK        G02ERR
120400040901     C                   CALL      'XSRDA8'
120500040901     C                   PARM                    WLBDA8
120600040901     C     G02ERR        IFEQ      '1'
120700040901     C                   SETON                                        9011
120800040901     C                   MOVEL     ERR(7)        $MSG
120900040901     c                   end
121000040901     C*
121100040901     C                   ENDSR
121200040901     C*----------------------------------------------------*
121300040901     C* CARICO IL FILE DI LAVORO CON LE SPEDIZIONE DISTINTA*
121400040901     C*----------------------------------------------------*
121500040901     C     CARDIS        BEGSR
121600040901     C*
121700020905     C                   Z-ADD     V1cLNA        KFGS
121800960702     C                   Z-ADD     V01NDC        KNDC
121900960702     C                   Z-ADD     ROVDDC        KDDC
122000020905     C     KARB          SETLL     FNARB70L
122100911017     C*
122200941108     C* Azzero il contatore delle bolle in dist.
122300920220     C                   Z-ADD     0             NUM               3 0
122400911017     C                   DO        *HIVAL
122500911017     C*
122600020905     C     KARB          READE     FNARB70L                               50
122700941107     C     *IN50         IFEQ      '0'
122800160429     C     KARB1         CHAIN(N)  FNARBW2L
122900911017     C*
123000911112     C                   MOVE      '1'           ARBFLG
123100911017     C*
123200160429     c                   if        not %found(fnarbw2l) and
123300160429     C                             arbngd > NUM
123400920528     C                   Z-ADD     ARBNGD        NUM
123500920528     C                   END
123600990920     C*  Carico in schiera gli eventuali stop
123700940322     C     ARBSTP        IFNE      0
123800990920     C*  Azzero campo codice cliente
123900990920     C                   Z-ADD     0             CODCLI            7 0
124000990920     C* Verifico il tipo prima e seconda bolla
124100951207     C                   Z-ADD     1             X                 2 0
124200951207     C                   MOVEL     *BLANKS       WTBOL1
124300951207     C                   CLEAR                   DS3A
124400951207     C     ARBCBO        LOOKUP    C3A(X)                                 13
124500951207     C     *IN13         IFEQ      '1'
124600951207     C                   MOVEL     D3A(X)        DS3A
124700951207     C                   MOVEL     §3ATB1        WTBOL1            1
124800990920     C                   MOVEL     §3ATB2        WTBOL2            1
124900951207     C                   END
125000990920     C* Se la prima bolla è un assegnato eseguo posizionamento
125100990920     C* su FIAR6 con flag tipo bolla impostato a '1' .....
125200990920     C     WTBOL1        IFEQ      'A'
125300990920     C                   MOVEL     '1'           KTRC
125400990920     C                   Z-ADD     ARBAAS        KAAS
125500990920     C                   Z-ADD     ARBAAS        KLNP
125600990920     C                   Z-ADD     ARBAAS        KNRS
125700990920     C                   Z-ADD     ARBAAS        KNSP
125800990920     C     KAR6          CHAIN     FIAR601L                           74
125900990920     C* ... se non trovo bolla o il codice cliente non è
126000990920     C* impostato reperisco il codice da FNARB
126100990920     C     *IN74         IFEQ      '1'
126200990920     C     AR6KSC        OREQ      0
126300990920     C                   Z-ADD     ARBKSC        CODCLI
126400990920     C                   ELSE
126500990920     C* ... altrimenti recupero codice cliente da FIAR6
126600990920     C                   Z-ADD     AR6KSC        CODCLI
126700990920     C                   END
126800990920     C* .... se prima bolla è un porto franco ....
126900990920     C                   ELSE
127000990920     C* .... verifico se la seconda è un assegnato
127100990920     C     WTBOL2        IFEQ      'A'
127200990920     C                   MOVEL     '2'           KTRC
127300990920     C                   Z-ADD     ARBAAS        KAAS
127400990920     C                   Z-ADD     ARBAAS        KLNP
127500990920     C                   Z-ADD     ARBAAS        KNRS
127600990920     C                   Z-ADD     ARBAAS        KNSP
127700990920     C     KAR6          CHAIN     FIAR601L                           74
127800990920     C* ... e reperisco il codice bolla da lì
127900990920     C     *IN74         IFEQ      '0'
128000990920     C     AR6KSC        ANDNE     0
128100990920     C                   Z-ADD     AR6KSC        CODCLI
128200951207     C                   END
128300990920     C                   END
128400990920     C*
128500990920     C                   END
128600990920     C* Controllo se codice bolla è codificato
128700951207     C                   Z-ADD     CODCLI        APPOG             4 0
128800951207     C     APPOG         IFNE      8888
128900951207     C     APPOG         ANDNE     9999
129000990920     C* ed è un assegnato
129100990920     C     CODCLI        ANDNE     0
129200951211     C                   MOVEL     *BLANKS       RAGCLI
129300951211     C                   MOVEL     CODCLI        RAGCLI
129400951207     C                   ELSE
129500951121     C                   MOVEL     ARBRSD        RAGCLI           70
129600951121     C                   MOVE      ARBIND        RAGCLI           70
129700951207     C                   END
129800961128     C                   Z-ADD     1             X                 2 0
129900951123     C     ARBSTP        LOOKUP    STP2(X)                                13
130000961217     C     *IN13         IFEQ      '0'
130100961217     C     W             ANDGE     999
130200961217     C                   SETON                                        79
130300961217     C                   MOVEL     ERR(20)       $ERR
130400961217     C                   END
130500940331     C     *IN13         IFEQ      '0'
130600961217     C     W             ANDLT     999
130700940331     C                   ADD       1             W
130800940322     C                   Z-ADD     ARBSTP        STP2(W)
130900951121     C                   MOVEL     RAGCLI        RSD2(W)
131000940331     C                   END
131100940322     C     ARBSTP        IFGT      ULTSTP
131200940322     C                   Z-ADD     ARBSTP        ULTSTP
131300940322     C                   END
131400940322     C                   END
131500160429     c                   if        not %found(fnarbw2l) and *in50 = '0'
131600160429     C                   WRITE     FNARBW00
131700911108     C                   END
131800160429     C                   END
131900911017     C*
132000941107     C  N50              END
132100911017     C*
132200911104     C                   SETON                                        05
132300941107     C                   FEOD      FNARBW2L
132400911017     C                   ENDSR
132500911017     C*----------------------------------------------------*
132600911017     C* GESTIONE DEL FORMATO 3                             *
132700911017     C*----------------------------------------------------*
132800940202     C     GESFM3        BEGSR
132900911017     C*
133000911017     C* Pulisco i campi che devo vidualizzare nel fm2
133100030618     C                   clear                   V03pis
133200030618     C                   MOVEL     ROVDDC        VIDANS
133300911017     C                   Z-ADD     *ZEROS        V03LNP
133400911017     C                   Z-ADD     *ZEROS        V03NRS
133500911017     C                   Z-ADD     *ZEROS        V03NSP
133600911105     C                   SETOFF                                       23
133700911108     C                   Z-ADD     *ZEROS        CTR
133800911017     C*
133900911017     C                   DO        *HIVAL
134000030701     c* NO ERRORE
134100030620     C* se non c'è errore ed è stata trovata la bolla pulisco i riferimenti
134200030620     c* della spedizione
134300030620     c                   if        not *in90
134400030620     C                   clear                   V03pis
134500030620     C                   MOVEL     ROVDDC        VIDANS
134600030620     C                   Z-ADD     *ZEROS        V03LNP
134700030620     C                   Z-ADD     *ZEROS        V03NRS
134800030620     C                   Z-ADD     *ZEROS        V03NSP
134900080415     C                   SETOFF                                       4341
135000030620     c                   else
135100030701     c* SI ERRORE
135200030701     c* se c'è ind. 90 di errore generico acceso, abblenco i campi della
135300030701     c* spedizione se ho barcode impostato e se sono entrato in modalità
135400030701     c* pistola, questo per evitare di dover abblencare i campi in caso
135500030701     c* di errore
135600030701     c                   if        v1cisp = 'P' and v03nsp <> 0 and
135700030701     c                             v03pis <> 0
135800030701     C                   MOVEL     ROVDDC        VIDANS
135900030701     C                   Z-ADD     *ZEROS        V03LNP
136000030701     C                   Z-ADD     *ZEROS        V03NRS
136100030701     C                   Z-ADD     *ZEROS        V03NSP
136200030620     c                   end
136300030701     c* se c'è ind. 90 di errore generico acceso, abblenco il campo del
136400030701     c* barcode se ho spedizione impostata e se sono entrato in modalità
136500030701     c* manuale, questo per evitare di dover abblencare i campi in caso
136600030701     c* di errore
136700030701     c                   if        v1cisp = 'M' and v03nsp <> 0 and
136800030701     c                             v03pis <> 0
136900030701     C                   Z-ADD     *ZEROS        V03pis
137000030701     c                   end
137100030701     c                   end
137200941108     C                   EXFMT     LR50D02
137300030910     C                   SETOFF                                       90
137400030701     c* accendo o spengo l'indicatore 73 che mi serve per il posizionamento
137500030701     c* 73 in caso di errore si posiziona sul barcode altrimenti sul numero
137600030701     c* della spedizione
137700030701     c                   if        (v03pis <> 0 and v03nsp <> 0) or
137800030701     c                             (v03pis = 0 and v03nsp = 0)
137900030701     c     v1cisp        comp      'P'                                    73
138000030702     c                   end
138100030702     c                   if        (v03pis <> 0 and v03nsp = 0) or
138200030702     c                             (v03pis = 0 and v03nsp <> 0)
138300030701     c     v03pis        comp      0                                  7373
138400030701     c                   end
138500030618     c* se il campo v03pis è impostato valorizzo i campi della spedizione
138600030620     c* solo se non sono stati impostati a mano o al giro di barcode che poi
138700030620     c* ha dato l'errore
138800030618     c                   clear                   ds_pis
138900030623     c                   if        v03pis <> 0
139000030702     c                   move      v03pis        ds_pis
139100030623     c                   if        v03nsp = 0 and
139200030620     c                             v03lnp = 0 and v03nrs = 0
139300030701     C                   MOVEL     ROVDDC        VIDANS
139400030618     c                   move      ds_pisaas     vidans
139500030618     c                   move      ds_pislnp     v03lnp
139600030618     c                   move      ds_pisnrs     v03nrs
139700030618     c                   move      ds_pisnsp     v03nsp
139800030623     c* controllo il check digits
139900030623     c                   exsr      srchkdgt
140000030623     c                   if        *in40
140100030623     c                   iter
140200030618     c                   end
140300030623     c                   else
140400030623     c                   move      vidans        com02             2 0
140500030623     c                   if        com02<>ds_pisaas or v03lnp<>ds_pislnp or
140600030623     c                             v03nrs<>ds_pisnrs or v03nsp<>ds_pisnsp
140700030623     c                   seton                                        5190
140800030623     c                   iter
140900030623     c                   end
141000030623     c                   end
141100030623     c                   end
141200950117     C*  Imposto variabili x test forzature
141300950117     C     VIDANS        IFNE      SAVAAS
141400950117     C     V03LNP        ORNE      SAVLNP
141500950117     C     V03NSP        ORNE      SAVNSP
141600950117     C     V03NRS        ORNE      SAVNRS
141700170307     c                   if        WForza = 'N'
141800160502     C                   MOVE      'N'           WFZFFD            1
141900950117     C                   MOVE      'N'           WFZAPP            1
142000950607     C                   MOVE      'N'           WFZTUR            1
142100100203     C                   MOVE      'N'           WFZatt            1
142200150929     C                   MOVE      'N'           WFZCAlert         1
142300160502     C                   MOVE      'N'           WFZCtur           1
142400031119     c                   clear                   k17
142500110803     c                   clear                   k08
142600170307     c                   end
142700950117     C                   END
142800950117     C*
142900911108     C   KL              DO
143000911108     C* 1° VOLTA
143100911108     C     CTR           IFEQ      *ZEROS
143200911108     C                   ADD       1             CTR               1 0
143300911108     C                   SETON                                        9094
143400911108     C                   ELSE
143500070410     c                   unlock    fidst09l
143600911108     C* 2° VOLTA
143700940202     C                   Z-ADD     4             FVVNPG
143800941107     C                   MOVE      V01NDC        FVVNFV
143900020905     C                   MOVE      V1cLNA        FVVFGS
144000020905     C     KFVV          CHAIN     FNFVV01L                           50
144100940209     C  N50FVVFCF        IFEQ      'A'
144200940202     C                   MOVEL     *BLANKS       FVVFCF
144300940202     C                   END
144400941109     C  N50              UPDATE    FNFVV000
144500000515      *GESTIONE FOGLIO 8 (RIENTRI) PARALLELA A FOGLIO 4
144600000515     C                   MOVE      FVVDS         FVVDS2
144700000515     C                   Z-ADD     8             FVVNPG
144800000515     C     KFVV          CHAIN     FNFVV000                           50
144900000515     C     *IN50         IFEQ      *OFF
145000000515     C                   MOVE      FVVDS2        FVVDS
145100000515     C                   Z-ADD     8             FVVNPG
145200000515     C                   UPDATE    FNFVV000
145300000515     C                   END
145400000626     C                   Z-ADD     4             FVVNPG
145500940202     C                   Z-ADD     *ZEROS        CTR
145600940202     C                   EXSR      PULRIC
145700911108     C                   END
145800911108     C                   END
145900911128     C**
146000030703     C** F2  - INserimento spedizione da manuale a pistola o viceversa
146100030703     c**       serve per il posizionamento sulla spedizione o sul barcode
146200030703     C   KB              DO
146300030703     c                   if        v1cisp = 'P'
146400030703     c                   movel     'M'           v1cisp
146500030703     c                   eval      v03f2 = 'F2=Pistola'
146600030703     c                   seton                                        90
146700030703     c                   setoff                                       7336
146800030703     c                   else
146900030703     c                   movel     'P'           v1cisp
147000030703     c                   eval      v03f2 = 'F2=Manuale'
147100030703     c                   seton                                        907336
147200030703     c                   end
147300030703     C                   ENDdo
147400030703     C**
147500030703     C** F7  - INTERROGAZIONE BOLLE ARRIVI
147600911128     C   KG              DO
147700941109     C                   MOVEL     '2'           P36FLG
147800941108     C                   Z-ADD     VIDANS        P36AAS
147900941108     C                   Z-ADD     V03LNP        P36LNP
148000941108     C                   Z-ADD     V03NSP        P36NSP
148100941108     C                   Z-ADD     V03NRS        P36NRS
148200941109     C                   MOVEL     ' '           P36F03
148300941109     C     P36NSP        IFGT      0
148400941109     C                   MOVEL     '1'           P36FLG
148500941109     C                   END
148600941108     C                   MOVEL     DSLR36        KPJBU
148700941108     C                   CALL      'FNLR36R'
148800911128     C                   PARM                    KPJBA
148900941109     C                   MOVEL     KPJBU         DSLR36
149000941109     C     P36F03        IFNE      '1'
149100941215     C     V03NSP        ANDEQ     0
149200941109     C                   Z-ADD     P36AAS        VIDANS
149300941109     C                   Z-ADD     P36LNP        V03LNP
149400941109     C                   Z-ADD     P36NRS        V03NRS
149500941109     C                   Z-ADD     P36NSP        V03NSP
149600941109     C                   END
149700911128     C                   SETON                                        90
149800911128     C                   END
149900911017     C*
150000941109     C** F13 - BLOCCO SBLOCCO SPEDIZIONI
150100920831     C   KM              DO
150200031017     c                   clear                   dslr02
150300030630     c                   exsr      srfnlr39
150400031017     C                   SETON                                        90
150500920831     C                   END
150600920206     C*
150700031125     C** F17 - ABILITAZIONE MERCE (forzatura non abilita più)
150800031119     c   kr              z-add     1             k17               1 0
150900110803     C** F08 - forzatura immissione in distinta x sped. perfect order
151000110803     c   kh              z-add     1             k08               1 0
151100920206     C*
151200941108     C* F12 Videata precedente
151300911128     C  NKL
151400911128     CANN90              DO
151500940202     C                   Z-ADD     *ZEROS        CTR
151600911017     C*
151700911017     C* Controllo se la spedizione è esistente o valida su video
151800911017     C*
151900911017     C     VIDANS        IFEQ      *ZEROS
152000911017     C                   SETON                                        1790
152100911017     C                   END
152200911017     C     V03LNP        IFEQ      *ZEROS
152300911017     C                   SETON                                        1490
152400911017     C                   END
152500911017     C     V03NSP        IFEQ      *ZEROS
152600911127     C                   SETON                                        2990
152700911017     C                   END
152800911017     C* N90 CERCO LA SPEDIZIONE NELL'ARCHIVIO ARB00 E ARB50
152900911017     C*
153000911023     C  N90              DO
153100911017     C                   Z-ADD     VIDANS        ARBAAS
153200911017     C                   Z-ADD     V03LNP        ARBLNP
153300911017     C                   Z-ADD     V03NRS        ARBNRS
153400911017     C                   Z-ADD     V03NSP        ARBNSP
153500030909     C* AGGANCIO FNARB
153600940207     C                   SETOFF                                       86
153700941108     C     KARB1         CHAIN(N)  FNARB01L                           15
153800030623     C  N15              DO
153900030909     c* se non ci sono errori controllo che la spedizione non sia già
154000030909     c* stata inserita in QUESTA distinta
154100030909     C                   if        not *in90
154200030909     C     KARB1         CHAIN(N)  FNARBW2L
154300030909     c                   if        %found(fnarbw2l)
154400030909     c                   seton                                        5790
154500030909     c                   end
154600030909     c                   end
154700941108     C*  Reperisco Tipo Bolla
154800941108     C                   Z-ADD     1             X                 2 0
154900941108     C                   CLEAR                   DS3A
155000941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
155100941108     C     *IN13         IFEQ      '1'
155200941109     C                   MOVEL     D3A(X)        DS3A
155300941108     C                   END
155400940322     C     *IN90         IFEQ      '0'
155500940322     C     ARBNDC        ANDNE     *ZEROS
155600941107     C     ARBNDC        ANDNE     V01NDC
155700930621     C     ARBNMI        ANDEQ     0
155800141112     c*
155900031125     c                   clear                   fnlr98ds
156000031125     c                   z-add     arbaas        i98aas
156100031125     c                   z-add     arblnp        i98lnp
156200031125     c                   z-add     arbnsp        i98nsp
156300031125     c                   z-add     arbnrs        i98nrs
156400110125     c                   z-add     arbifp        i98fgs
156500110125     c                   z-add     4             i98npg
156600110125     c                   z-add     arbndc        i98ndc
156700031125     c                   movel     fnlr98ds      kpjbu
156800031125     c                   call      'FNLR98C'
156900031125     c                   parm                    kpjba
157000031125     c                   movel     kpjbu         fnlr98ds
157100031125     c                   if        o98esito <> '0'
157200930520     C                   SETON                                        2490
157300031125     c                   endif
157400930520     C                   END
157500940322     C*
157600941108     C* Controllo se linea di arrivo abilitante
157700911129     C  N90ARBLNA        LOOKUP    LIN                                    70
157800911127     C  N70              SETON                                        2590
157900941108     C* Controllo che la spedizione non sia una mamma
158000110804     C     KARB1         CHAIN     FNLBL02L                           74
158100110804     C  N74              SETON                                        9089
158200110804     C   74              SETOFF                                       89
158300941108     C*----------------------------------------------------*
158400050613     C*  errore per spedizioni V.PACCO
158500050613     c                   if        arbrsd = 'VEDI PACCO' and not *in90
158600050613     c                   seton                                        5890
158700050613     c                   end
158800941108     C* Controlli x bolle da consegnare
158900941108     C     ARBDCM        IFEQ      0
159000941108     C* Controllo il livello di abilitazione merce
159100941108     C  N90ARBAMA        IFEQ      *ZEROS
159200040312     c     arbdam        andeq     0
159300911127     C                   SETON                                        2690
159400911105     C                   END
159500040312     C  N90ARBAMA        IFEQ      *ZEROS
159600040312     c     arbdam        andne     0
159700040312     c     k17           andeq     0
159800040312     C                   SETON                                        6890
159900040312     C                   END
160000110803     c* perfect order: se si inserisce una spedizione avviso + f8 x forzare
160100130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
160200130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
160300130221     C**N90ARBxco*****   IFne      ' '
160400130221     C  N90ARBxco        IFgt      '0'
160500110803     c     arbntc        andeq     0
160600110803     c     k08           andeq     0
160700110803     C                   SETON                                        6590
160800110803     C                   END
160900941108     C* Controllo che la data di consegna prevista <=  alla data distinta
161000030701     C  n90ARBDCR        IFGT      ROVDDC
161100021015     C     ARBTCR        andne     'P'
161200030701     C                   SETON                                        4490
161300920218     C                   END
161400941108     C* Se data consegna richiesta ha come flag dopo controllo
161500030701     C  n90ARBTCR        IFEQ      'D'
161600920306     C     ARBDCR        ANDEQ     ROVDDC
161700030701     C                   SETON                                        4490
161800920218     C                   END
161900160316     c* se spedizione con giro "VP" vedi pacco DPD, deve esssere
162000160316     c* in una distinta di un autista fittizio
162100160316     c     karb1         chain(n)  fiarg01l
162200160316     c                   if        %found(fiarg01l) and argcgi = §nsdcgi
162300160316     c                             and apdpdd <> 'S'
162400160316     C                   SETON                                        9990
162500160316     c                   end
162600150929      ****
162700030909     C* controllo gli errori che richiedono una forzatura
162800030909     c  n90              do
162900950117     C*----------------------------------------------------*
163000040217     C                   SETOFF                                       4667
163100030909     C     *INKK         IFEQ      '1'
163200950607     C                   MOVEL     'S'           WFZAPP
163300950117     C                   ELSE
163400950607     C     WFZAPP        IFEQ      'N'
163500950607     C     ARBTC1        IFEQ      'A'
163600950607     C     ARBTC2        OREQ      'A'
163700950607     C* Controllo che ci sia la consegna richiesta x consegna
163800950607     C* con appuntamento
163900030909     C     ARBDCR        IFEQ      0
164000950607     C     ARBTCR        ANDEQ     ' '
164100950607     C     ARBHCR        ANDEQ     0
164200040217     c                   exsr      numtel
164300040217     c                   if        §ar5teld = *blank
164400030910     C                   SETON                                        4690
164500030909     c                   leave
164600040217     c                   else
164700040217     c                   seton                                        6790
164800040217     c                   endif
164900950607     C                   END
165000950607     C                   END
165100950607     C*
165200950607     C                   END
165300950607     C                   END
165400030909     C*----------------------------------------------------*
165500030910     C                   SETOFF                                       47
165600030909     C     *INKF         IFEQ      '1'
165700030909     C                   MOVEL     'S'           WFZFFD
165800170307     C                   MOVE      'S'           WForza            1
165900030909     C                   ELSE
166000030909     C     WFZFFD        IFEQ      'N'
166100030909     C* Controllo se la spedizione è in fermo deposito e lo segnalo
166200030909     C     ARBFFD        IFEQ      'S'
166300051107     C     ARBFFD        oreq      'M'
166400030910     C                   SETON                                        4790
166500030909     c                   leave
166600030909     C                   END
166700030909     C                   END
166800030909     C                   END
166900950607     C*----------------------------------------------------*
167000030910     C                   SETOFF                                       42
167100030909     c     *INKT         IFEQ      '1'
167200950607     C                   MOVEL     'S'           WFZTUR
167300170307     C                   MOVE      'S'           WForza            1
167400950607     C                   ELSE
167500950607     C     WFZTUR        IFEQ      'N'
167600030909     C     ARBGC1        IFNE      *BLANKS
167700950607     C                   MOVEL     ARBGC1        WGIOR             1
167800950607     C     WGIOR         IFEQ      WGG
167900030910     C                   SETON                                        4290
168000030909     c                   leave
168100950607     C                   END
168200950607     C                   END
168300030909     C     ARBGC2        IFNE      *BLANKS
168400950607     C                   MOVEL     ARBGC2        WGIOR             1
168500950607     C     WGIOR         IFEQ      WGG
168600030910     C                   SETON                                        4290
168700030909     c                   leave
168800950607     C                   END
168900950607     C                   END
169000950607     C                   END
169100950607     C                   END
169200150929     C*----------------------------------------------------*
169300100203     C* controllo se sepdizione ha particolarità cons. di attesa
169400100203     C                   SETOFF                                       48
169500100203     c     *INKn         IFEQ      '1'
169600100203     C                   MOVEL     'S'           WFZatt
169700170307     C                   MOVE      'S'           WForza            1
169800100203     C                   ELSE
169900100203     C     WFZatt        IFEQ      'N'
170000100203     C     ARBGma        andne     *BLANKS
170100100203     C     ARBTCR        andeq     'P'
170200100203     C     ARBdcr        IFGT      rovddc
170300130429     c                   clear                   tnsd99ds
170400130424     C                   Z-ADD     ROVDDC        D98DFC
170500130424     C                   Z-ADD     2359          D98OFC
170600130424     C                   MOVEL     ' '           D98TLA
170700130424     C                   MOVEL     'A'           D98TBO
170800130424     C                   Z-ADD     ARBAAS        D98AAS
170900130424     C                   Z-ADD     ARBLNP        D98LNP
171000130424     C                   Z-ADD     ARBNRS        D98NRS
171100130424     C                   Z-ADD     ARBNSP        D98NSP
171200130424     C                   CALL      'TNSD99R'
171300130424     C                   PARM                    TNSD99DS
171400130424     c                   if        d98nci <=-24
171500100203     C     arbgma        lookup    s7r                                    48
171600100203     C                   IF        *in48
171700100203     C                   SETON                                        90
171800100203     c                   leave
171900100203     C                   END
172000100203     C                   END
172100100203     C                   END
172200100203     C                   END
172300100203     C                   END
172400150929     C*----------------------------------------------------*
172500150929     C* controllo se mandato un contro ALERT nello stesso giorno
172600150929     C                   SETOFF                                       53
172700150929     c     *INKP         IFEQ      '1'
172800150929     C                   MOVEL     'S'           WFZCAlert
172900170307     C                   MOVE      'S'           WForza            1
173000150929     C                   ELSE
173100150929     C     WFZCAlert     IFEQ      'N'
173200150929     c                   exsr      contro_alert
173300150929     c   53              leave
173400150929     C                   END
173500150929     C                   END
173600160502     C* controllo se la spedizione è in una zona del secondo turno
173700160502     C                   SETOFF                                       59
173800160502     c     *INKi         IFEQ      '1'
173900160502     C                   MOVEL     'S'           WFZCtur
174000170307     C                   MOVE      'S'           WForza            1
174100160502     C                   ELSE
174200170517      **** solo se NON è un FITTIZIO
174300170517      *
174400170517     c                   if        APDpdd <> 'S'
174500160502     C     WFZCtur       IFEQ      'N'
174600160502     c                   exsr      contro_turno
174700160502     c   59              leave
174800160502     C                   END
174900170517     c                   end
175000170517      *
175100160502     C                   END
175200150929     C*----------------------------------------------------*
175300150929      ***
175400030909     c                   enddo
175500150929      **
175600030909     c* imposto i campi salvati per forzatura
175700030909     c                   if         *in42 or *in47 or *in46
175800100203     c                              or *in48
175900170307     c                              or *in53 or *in59
176000030909     C                   z-add     VIDANS        SAVAAS
176100030909     C                   z-add     V03LNP        SAVLNP
176200030909     C                   z-add     V03NSP        SAVNSP
176300030909     C                   z-add     V03NRS        SAVNRS
176400170307     C                   MOVE      'N'           WForza            1
176500030909     c                   end
176600950117     C*----------------------------------------------------*
176700941108     C* Controllo abilitazione incasso contrassegno e assegnato
176800941108     C  N90ARBACC        IFNE      'S'
176900911127     C                   SETON                                        2790
177000911105     C                   END
177100911105     C*
177200941108     C  N90ARBACA        IFNE      'S'
177300911127     C                   SETON                                        2890
177400911105     C                   END
177500911114     C*
177600941108     C  N90ARBFDC        IFNE      ' '
177700930513     C     ARBDCP        ANDEQ     0
177800911127     C                   SETON                                        2490
177900911114     C                   END
178000030630     c* se c'è un lasciato avviso sblocco in automatico la spedizione
178100030630     c                   IF        arbfbc = 'A'
178200031017     c                   clear                   dslr02
178300031017     c                   eval      dr2vid = 'N'
178400030630     c                   exsr      srfnlr39
178500031017     c                   if        dr2err = *blanks
178600030630     c                   clear                   arbcmc
178700030630     c                   clear                   arbfbc
178800030630     c                   end
178900030630     c                   end
179000941108     C* Controllo se bolla abilitata oppure no alla consegna
179100911127     C  N90ARBFBC        IFNE      *BLANK
179200911127     C                   SETON                                        2290
179300911105     C                   END
179400920124     C*
179500150929     C* Controlla se presente un contro Alert per il giorno stesso FORZABILE
179600150929     C*
179700150929     C*
179800941108     C                   ELSE
179900941108     C*----------------------------------------------------*
180000941108     C* Bolla già consegnata
180100941108     C*----------------------------------------------------*
180200990913     C* Controllo abilitazione incasso contrassegno
180300990913     C  N90ARBACC        IFNE      'S'
180400990913     C                   SETON                                        2790
180500990913     C                   END
180600990913     C* CONTROLLO SE C'E' ASSEGNATO DA INCASSARE
180700990913     C  N90ARBACA        IFNE      'S'
180800990913     C                   SETON                                        2890
180900990913     C                   END
181000920220     C* CONTROLLO SE I FLAG DI INCASSO ICA / ICC SONO DA PRENDERE IN
181100930427     C* CONSIDERAZIONE (NEL CASO DI CONSEGNA PARZIALE O CONSEGNA CON
181200930427     C* RITORNO ALL'INCASSO)
181300940207     C  N24ARBFIP        IFNE      'I'
181400911129     C     ARBICC        IFEQ      ' '
181500941108     C     ARBICA        ANDNE     'R'
181600941108     C*  Controllo se c'è contrassegno
181700941108     C                   Z-ADD     0             AR9CAS
181800941108     C     §3AFCA        IFNE      0
181900051115     C     KARB1         CHAIN     FIAR901L                           08
182000941108     C                   END
182100941108     C     AR9CAS        IFEQ      *ZEROS
182200940207     C                   SETON                                        249086
182300911129     C                   END
182400941108     C                   END
182500911129     C*
182600941108     C  N24ARBICC        IFNE      'R'
182700941108     C     ARBICA        ANDNE     'R'
182800920120     C                   SETON                                        2490
182900920120     C                   END
183000920124     C                   END
183100941108     C*----------------------------------------------------*
183200930428     C                   END
183300941108     C*----------------------------------------------------*
183400941109     C     KARB1         CHAIN     FNLBL01L                           87
183500940207     C     *IN87         IFEQ      '0'
183600950126     C     ARBLNA        ANDEQ     LBLLAP
183700941108     C     KLBL          CHAIN(N)  FNARB01L                           87
183800980429     C*
183900980429     C     *IN87         IFEQ      '0'
184000980429     C     ARBCCA        ANDEQ     '2'
184100980429     C     *IN87         OREQ      '0'
184200980429     C     ARBCCA        ANDEQ     '6'
184300980429     C                   SETON                                        87
184400980430     C     KARB2         CHAIN     FNARB01L                           30
184500980429     C                   END
184600940208     C*
184700941108     C     *IN87         IFEQ      '0'
184800941108     C*  Reperisco Tipo Bolla
184900941108     C                   Z-ADD     1             X                 2 0
185000941108     C                   CLEAR                   DS3A
185100941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
185200941108     C     *IN13         IFEQ      '1'
185300941109     C                   MOVEL     D3A(X)        DS3A
185400941108     C                   END
185500941108     C*  Controllo se c'è contrassegno
185600941108     C                   Z-ADD     0             AR9CAS
185700941108     C     §3AFCA        IFNE      0
185800051115     C     KARB1         CHAIN     FIAR901L                           08
185900941108     C                   END
186000940208     C*  Controllo se la mamma è già stata consegnata ma non ancora
186100940208     C*  contabilizzata
186200941108     C     ARBICC        IFEQ      'S'
186300941108     C     ARBNMI        ANDEQ     0
186400941108     C     AR9CAS        ANDNE     *ZEROS
186500941108     C     ARBICC        OREQ      'I'
186600940208     C     ARBNMI        ANDEQ     0
186700941108     C     AR9CAS        ANDNE     *ZEROS
186800940208     C                   SETON                                        9038
186900940208     C                   END
187000941108     C                   END
187100940208     C*
187200950223     C  N87
187300950223     CANN38ARBICC        IFEQ      *BLANKS
187400941108     C     AR9CAS        ANDNE     0
187500941108     C     ARBICC        OREQ      'R'
187600941108     C     AR9CAS        ANDNE     0
187700940208     C   86              SETOFF                                       9024
187800940208     C*  Controllo se la mamma della spedizione inserita ha un'altra
187900940208     C*  figlia inserita in un'altra distinta
188000941108     C     KARB1         CHAIN     FNLBL02L                           85
188100940208     C     *IN85         DOWEQ     '0'
188200940208     C     *IN39         ANDEQ     '0'
188300940208     C*   Se ho trovato una figlia diversa dall' originale mi posiziono
188400940208     C*   su di essa e controllo se ha impostato il flag di ditinta chiusa
188500940208     C     VIDANS        IFNE      LBLAAN
188600940208     C     V03LNP        ORNE      LBLLPN
188700940208     C     V03NRS        ORNE      LBLNRN
188800940208     C     V03NSP        ORNE      LBLNSN
188900100514     C     KARB2         CHAIN(N)  FNARB01L                           84
189000940208     C     *IN84         IFEQ      '0'
189100940208     C     ARBNDC        ANDNE     0
189200940208     C     ARBFDC        ANDEQ     ' '
189300940208     C                   SETON                                        3990
189400940208     C                   END
189500940208     C                   END
189600940208     C*   Leggo la figlia successiva
189700940208     C     *IN39         IFEQ      '0'
189800940208     C                   Z-ADD     LBLAAP        ARBAAS
189900940208     C                   Z-ADD     LBLLPP        ARBLNP
190000940208     C                   Z-ADD     LBLNRP        ARBNRS
190100940208     C                   Z-ADD     LBLNSP        ARBNSP
190200941108     C     KARB1         READE     FNLBL02L                               85
190300940208     C                   END
190400940208     C                   END
190500940208     C*   reimposto la chiave per il posizionamento sulla figlia originale
190600940208     C                   Z-ADD     VIDANS        LBLAAN
190700940208     C                   Z-ADD     V03LNP        LBLLPN
190800940208     C                   Z-ADD     V03NRS        LBLNRN
190900940208     C                   Z-ADD     V03NSP        LBLNSN
191000940208     C                   END
191100940208     C*
191200941108     C* Riaggancio la figlia
191300941108     C     KLBL1         CHAIN(N)  FNARB01L                           87
191400920324     C                   END
191500941108     C                   END
191600930429     C*
191700941108     C* Per record esistente controllo se è valido alla consegna
191800941108     C* controllo se ho già caricato al file di lavoro
191900911127     C  N90              DO
192000940322     C*
192100940322     C*  Richiamo routine per gestione attribuzione Stop
192200940322     C                   EXSR      GESSTP
192300941108     C     KARB1         CHAIN(N)  FNARBW2L                           52
192400911113     C                   MOVE      ' '           ARBFLG
192500930615     C   52
192600930615     CANN15              ADD       1             NUM
192700930615     C   52
192800930615     CANN15              Z-ADD     NUM           ARBNGD
192900941108     C   52
193000941108     CANN15              WRITE     FNARBW00
193100911105     C                   SETOFF                                       23
193200911105     C                   END
193300911017     C*
193400911023     C                   END
193500911022     C*
193600941108     C* Per F10 vado in visualizzazione subfile
193700941108     C   KJ              DO
193800911106     C                   SETOFF                                       90
193900941108     C                   FEOD      FNARBW2L
194000911024     C                   EXSR      GESSBF
194100911106     C                   SETOFF                                       93
194200950208     C   81              GOTO      EFM3
194300911024     C                   END
194400911017     C                   END
194500911108     C   94
194600911108     CAN KL
194700911108     CORNKL              END
194800911017     C*
194900911024     C     EFM3          TAG
195000911017     C                   ENDSR
195100040217     C*-----------------------------------------------------*
195200040217     C* controllo numero di telefono se appuntamento
195300040217     C*-----------------------------------------------------*
195400040217     C     numtel        BEGSR
195500040217      * rekord GEN riferimenti destinatario
195600040217     C                   clear                   dar5gen
195700040217     C                   move      'GEN'         trcar5
195800040217     C     Kfiar5        CHAIN     FiAR501L                           43
195900040217     C     *IN43         IFEQ      '0'
196000040217     C                   MOVEL     ar5uni        dar5gen
196100040217     C                   endif
196200040217     C                   ENDSR
196300150929     C*-----------------------------------------------------*
196400150929     C* controllo se mandato un CONTRO Alert al CLIENTE
196500150929     C*-----------------------------------------------------*
196600150929     C     contro_alert  BEGSR
196700150929      * rec.EMD   invio Alert e contro Alert
196800150929     C                   setoff                                       53
196900150929     C                   clear                   dar5emd
197000150929     C                   move      'EMD'         trcar5
197100150929     C     Kfiar5        CHAIN     FiAR501L
197200150929     C                   IF        %Found(FiAR501L)
197300150929     C                   MOVEL     ar5uni        dar5emd
197400150929     c                   if        §AR5ICA ='S'
197500150929     c                   if        §AR5DPC <>*blank and §AR5DPC <>*zeros
197600150929     c                   move      §AR5DPC       data8DPC          8 0
197700150929     c                   if        data8DPC = Dateu
197800150929     C                   seton                                        5390
197900150929     C                   end
198000150929     C                   end
198100150929     C                   end
198200150929     C                   endif
198300150929     C                   ENDSR
198400160502     C*-----------------------------------------------------*
198500160502     C* controllo se spedizone è del secondo turno
198600160502     C*-----------------------------------------------------*
198700160502     C     contro_turno  BEGSR
198800160502     c*
198900160502     C                   setoff                                       59
199000160502     C                   if        v1ctpm = 'P' and
199100160502     c                             v1ctur = '1' and *in01
199200160510     c                             and d17tur1 <> 'U' and d17tur2 <> 'U'
199300160502     c                   clear                   ds03
199400160502     C                   eval      tblcod = '03'
199500160502     C                   movel     v1cfpd        com05             5
199600160502     C                   move      arbznc        com05             5
199700160502     C                   movel(p)  com05         tblkey
199800160502     C     ktab          chain     Tabel00f
199900160502     c                   if        %found(tabel00f)
200000160502     C                   MOVEL     TBLUNI        DS03
200100160502     c                   if        §03TPM = ' ' and
200200160502     c                             §03Turno = '2'
200300160502     C                   seton                                        5990
200400160502     c                   end
200500160502     c                   end
200600160502     c                   end
200700160502     C                   ENDSR
200800911018     C*-----------------------------------------------------*
200900030623     C* controllo check digits
201000911018     C*-----------------------------------------------------*
201100030623     C     srchkdgt      BEGSR
201200030623     c                   movel     ds_pis        barcode
201300030623     C                   clear                   trul28ds
201400030623     c                   eval      i28mod = 'BAR'
201500030623     c                   eval      i28cod = barcode
201600030623     c                   call      'TRUL28R1'
201700030623     c                   parm                    trul28ds
201800030623     c* se reperito barcode imposto i campi in stampa
201900030623     c                   if        o28err <> *blanks or  o28cod <> ds_pis
202000030623     c                   seton                                        4090
202100030623     c                   end
202200030623     C                   ENDSR
202300030630     C*-----------------------------------------------------*
202400030630     C* chiamata a fnlr39r
202500030630     C*-----------------------------------------------------*
202600030630     C     srfnlr39      BEGSR
202700030630     C                   Z-ADD     VIDANS        DR2AAS
202800030630     C                   Z-ADD     V03LNP        DR2LNP
202900030630     C                   Z-ADD     V03NRS        DR2NRS
203000030630     C                   Z-ADD     V03NSP        DR2NSP
203100030630     C                   Z-ADD     V1CLNA        DR2FGS
203200030630     C                   MOVEL     '1'           DR2RIC
203300030630     C                   MOVEL     DSLR02        KPJBU
203400030630     C                   CALL      'FNLR39R'
203500030630     C                   PARM                    KPJBA
203600031017     c                   movel     kpjbu         dslr02
203700030630     C                   ENDSR
203800030623     C*-----------------------------------------------------*
203900030623     C* GESTIONE DEL SUBFILE E DELLE VIDEATE A LUI COLLEGATE*
204000030623     C*-----------------------------------------------------*
204100030623     C     GESSBF        BEGSR
204200911018     C*
204300911104     C                   SETON                                        64
204400911106     C                   Z-ADD     *ZEROS        CTR
204500930608     C                   DO        *HIVAL
204600911018     C*
204700941108     C* Pulizia del subfile per il caricamento
204800930608     C  N75
204900930608     CANN18              DO
205000940323     C                   Z-ADD     1             NRRKEY
205100911018     C                   SETON                                        62
205200941108     C                   WRITE     LR50C03
205300911018     C                   SETOFF                                       62
205400911018     C                   Z-ADD     *ZEROS        NRR               4 0
205500911018     C*
205600911018     C                   EXSR      CARSFL
205700911018     C*
205800911104     C     NRR           COMP      *ZEROS                             60
205900911104     C                   SETON                                        61
206000930608     C                   END
206100911023     C*
206200911104     C     PRIMA         TAG
206300930608     C                   DO
206400941108     C                   WRITE     LR50Z03
206500941108     C                   EXFMT     LR50C03
206600911023     C*
206700940322     C                   MOVEL     *BLANKS       $MSG
206800911023     C                   SETOFF                                       767775
206900940322     C                   SETOFF                                       189016
207000911023     C*
207100911023     C*
207200941108     C* F12 Controllo se digitati la prima volta mando un messaggio di
207300941108     C* avvertimento
207400911106     C  N81              Z-ADD     *ZEROS        CTR
207500911106     C   81              DO
207600911106     C* 1° VOLTA
207700911106     C     CTR           IFEQ      *ZEROS
207800911106     C                   ADD       1             CTR               1 0
207900940322     C                   SETON                                        909475
208000940322     C                   MOVEL     ERR(1)        $MSG
208100911106     C                   SETOFF                                       81
208200911106     C                   ELSE
208300070410     c                   unlock    fidst09l
208400911106     C* 2° VOLTA
208500950224     C                   Z-ADD     4             FVVNPG
208600950224     C                   MOVE      V01NDC        FVVNFV
208700020905     C                   Z-ADD     V1cLNA        FVVFGS
208800020905     C     KFVV          CHAIN     FNFVV01L                           50
208900950224     C  N50FVVFCF        IFEQ      'A'
209000950224     C                   MOVEL     *BLANKS       FVVFCF
209100950224     C                   UPDATE    FNFVV000
209200950224     C                   END
209300000515      *GESTIONE FOGLIO 8 (RIENTRI) PARALLELA A FOGLIO 4
209400000515     C                   MOVE      FVVDS         FVVDS2
209500000515     C                   Z-ADD     8             FVVNPG
209600000515     C     KFVV          CHAIN     FNFVV000                           50
209700000515     C     *IN50         IFEQ      *OFF
209800000515     C                   MOVE      FVVDS2        FVVDS
209900000515     C                   Z-ADD     8             FVVNPG
210000000515     C                   UPDATE    FNFVV000
210100000515     C                   END
210200000626     C                   Z-ADD     4             FVVNPG
210300000515      *
210400911106     C                   Z-ADD     *ZEROS        CTR
210500911106     C                   EXSR      PULRIC
210600911106     C                   END
210700911106     C                   END
210800911023     C*
210900941108     C* F12 Torno prima videata richieata bolla singola
211000941108     C* F10 Vado videata selezione soedizione
211100941108     C   KJ              SETON                                        93
211200911106     C   81
211300941108     COR KJ              GOTO      ENDGES
211400911104     C* PER COMANDO 22 SPROTEGGO I CAMPI DEL GIRO E DELLO STOP
211500911104     C   KW              SETOFF                                       64
211600911104     C* PER COMANDO 16 ANNULLAMENTO DISTINTA DEVE ESSERE ABILITATO
211700911104     C* SOLO IN CASO DI VARIAZIONE
211800911106     C* PER ANNULLAMNETO CHIEDO CONFERMA
211900911106     C   KQ              DO
212000941108     C                   EXFMT     LR50D03
212100911106     C     V08SNO        IFEQ      'SI'
212200911106     C                   EXSR      ANNUL
212300920423     C  N90              MOVEL     *BLANK        SAVCPC
212400941107     C  N90              MOVEL     *BLANK        V1CCPC
212500941107     C  N90              MOVE      V01NDC        V1CNDP
212600941107     C  N90              MOVEL     *BLANK        V1CNDC
212700940322     C  N90              SETON                                        8175
212800000717     C   90
212900000717     CANN78              MOVEL     ERR(2)        $MSG
213000911106     C                   ELSE
213100911106     C                   SETON                                        90
213200911106     C                   END
213300911106     C                   END
213400911023     C* SE DATO ENTER LEGGO I CAMPI DEL SUBFILE
213500911023     C*
213600930608     C  N81
213700930608     CAN 60
213800930608     CANN90              DO
213900911023     C*
214000911023     C                   SETOFF                                       54
214100911023     C*
214200930608     C                   DO        *HIVAL
214300911023     C*
214400941108     C                   READC     LR50S03                                54
214500930608     C  N54              DO
214600911022     C* CONTROLLO LE OPZIONI INSERITE
214700911018     C*
214800941108     C     V3CSEL        IFEQ      'E'
214900911023     C                   SETON                                        77
215000930608     C                   END
215100911022     C*
215200941108     C     V3CSEL        IFEQ      'I'
215300911023     C                   SETON                                        76
215400930608     C                   END
215500911022     C*
215600941108     C     V3CSEL        IFEQ      'V'
215700941108     C                   MOVEL     '1'           P36FLG
215800941108     C                   Z-ADD     V3CAAS        P36AAS
215900941108     C                   Z-ADD     V3CLNP        P36LNP
216000941108     C                   Z-ADD     V3CNSP        P36NSP
216100941108     C                   Z-ADD     V3CNRS        P36NRS
216200941108     C                   MOVEL     DSLR36        KPJBU
216300941108     C                   CALL      'FNLR36R'
216400911105     C                   PARM                    KPJBA
216500911022     C*
216600911022     C* MUOVO I PARAMETRI CHE SERVONO
216700911022     C* FACCIO LA CALL AL PROGRAMMA DI VISUALIZZAZIONE
216800930608     C                   END
216900911022     C*
217000940322     C     *IN76         IFEQ      '1'
217100940322     C     *IN77         ANDEQ     '1'
217200940323     C                   SETON                                        7518
217300940322     C                   MOVEL     ERR(3)        $MSG
217400940322     C                   END
217500940322     C  N76
217600940322     CANN77              SETOFF                                       18
217700940322     C*
217800940322     C*  Controllo gli Stop !!!
217900940322     C                   EXSR      CTRSTP
218000110804     c* perfect order
218100130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
218200130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
218300130221     c*****v3cxco        comp      ' '                                8888
218400130221     c     v3cxco        comp      '0'                                88
218500911023     C*
218600941108     C                   UPDATE    LR50S03
218700911022     C*
218800930608     C                   END
218900930608     C  N54              END
219000911023     C*
219100911023     C* SE NON CI SONO ERRORI
219200911021     C*
219300940322     C  N75
219400940322     CANN18              Z-ADD     0             NRR
219500940322     C  N75
219600940322     CANN18              DO        *HIVAL
219700911023     C                   ADD       1             NRR
219800911018     C*
219900941108     C     NRR           CHAIN     LR50S03                            54
220000911018     C*
220100930608     C  N54              DO
220200911018     C* CONTROLLO LE OPZIONI INSERITE
220300941109     C                   MOVE      V3CAAS        ARBAAS
220400941109     C                   MOVE      V3CLNP        ARBLNP
220500941109     C                   MOVE      V3CNRS        ARBNRS
220600941109     C                   MOVE      V3CNSP        ARBNSP
220700941109     C*
220800941108     C     V3CSEL        IFEQ      ' '
220900930608     C     *IN76         IFEQ      '1'
221000911023     C* CANCELLO IL RECORD DI LAVORO
221100941108     C     KARB1         DELETE    FNARBW2L                           55
221200911023     C                   END
221300941109     C*
221400930608     C     *IN77         IFEQ      '1'
221500940322     C     *IN77         OREQ      '0'
221600940322     C     *IN76         ANDEQ     '0'
221700911023     C* AGGIORNO IL FILE DI LAVORO CON GIRO E STOP
221800941108     C     KARB1         CHAIN     FNARBW2L                           55
221900941109     C     *IN55         IFEQ      '0'
222000941109     C                   Z-ADD     V3CNGD        ARBNGD
222100941109     C                   Z-ADD     V3CSTP        ARBSTP
222200941109     C                   MOVE      '1'           ARBFLG
222300941109     C                   UPDATE    FNARBW2
222400930608     C                   END
222500941109     C                   END
222600940322     C                   END
222700911023     C*
222800941108     C     V3CSEL        IFEQ      'E'
222900141112     c*check spedizione con disposizioni di dirottamento richiede forzatura se presenti
223000141112     c                   clear                   fnlr98ds2
223100141112     c                   z-add     arbaas        i982aas
223200141112     c                   z-add     arblnp        i982lnp
223300141112     c                   z-add     arbnsp        i982nsp
223400141112     c                   z-add     arbnrs        i982nrs
223500141112     c                   call      'FNLR98R2'
223600141112     c                   parm                    fnlr98ds2
223700141112     c                   if        o982esito = '0'
223800911023     C* CANCELLO IL RECORD DI LAVORO
223900941108     C     KARB1         DELETE    FNARBW2L                           55
224000930623     C* AGGIORNO LE EVENTUALI ANOMALIE SULLA BOLLA
224100931227     C  N55              EXSR      ANOM
224200141112     c                   endif
224300911023     C*
224400930608     C                   END
224500911023     C*
224600941108     C     V3CSEL        IFEQ      'I'
224700911023     C* AGGIORNO IL FILE DI LAVORO CON GIRO E STOP
224800941108     C     KARB1         CHAIN     FNARBW2L                           55
224900941109     C  N55              Z-ADD     V3CNGD        ARBNGD
225000941109     C  N55              Z-ADD     V3CSTP        ARBSTP
225100941109     C  N55              MOVE      '1'           ARBFLG
225200941108     C  N55              UPDATE    FNARBW2
225300930608     C                   END
225400911018     C*
225500930608     C                   END
225600930608     C  N54              END
225700930608     C                   END
225800911108     C*
225900911108     C* PULIZIA DEL SUBFILE PER IL CARICAMENTO
226000911108     C*
226100941110     C   KF
226200941110     CANN75              DO
226300911108     C                   SETON                                        62
226400941108     C                   WRITE     LR50C03
226500911108     C                   SETOFF                                       62
226600911108     C                   Z-ADD     *ZEROS        NRR               4 0
226700911108     C*
226800940323     C                   Z-ADD     1             NRRKEY
226900911108     C                   EXSR      CARSFL
227000911108     C* PER COMANDO 1 RICARICO IL SUBFILE ED EMETTO FORMATO DI TOTALE
227100911108     C                   SETOFF                                       06
227200911108     C                   EXSR      GESFM6
227300941108     C                   WRITE     LR50Z03
227400911108     C                   END
227500930608     C                   END
227600930608     C  N81              END
227700911018     C*
227800911104     C     ENDGES        ENDSR
227900911018     C*-----------------------------------------------------*
228000070309     C* aggiorno file distinte FIDST00F                     *
228100911018     C*-----------------------------------------------------*
228200070309     C     srfidst       BEGSR
228300070309     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
228400070309     c* se data distinte automatiche < oggi p.o. partito
228500090310     c*                  if        comdda <> 0
228600070309     C* F I D S T 0 0 F
228700070309     c                   clear                   fidstds
228800070312     c                   eval      wnfv = fvvnfv
228900070410     C     KDST2         CHAIN     fidst09l
229000120718     C                   eval      £dstpdr = apdpdr
229100070309     C                   MOVEL     'A'           £DSTtipa
229200120718     c                   eval      £dstnot = d55not
229300070309     C                   eval      £dstnpg = fvvnpg
229400070309     C                   eval      £dstfgs = fvvFGS
229500070309     C                   eval      £DSTNfv = fvvnfv
229600070309     C                   eval      £dstDFV = fvvdfv
229700070309     C                   MOVEL     FVVFSC        £dstFca
229800070309     C                   MOVEL     FVVFPP        £dstFPP
229900070309     C                   MOVEL     FVVPEP        £dstpEP
230000080121     C                   MOVEL     v1cpda        £dstpda
230100080208     C                   MOVEL     v1ctstpda     §dsttstpda
230200070309     C                   MOVEL     FVVspn        dfvvspn
230300070309     C                   MOVEL     §fvvtpm       £DSTTPM
230400160429     C                   MOVEL     §fvvturno     §DSTturno
230500160429     C                   MOVEL     ddstflr       £dstflr
230600070410     c                   if        %found(fidst09l)
230700080121     c* allineo i giri consegne
230800070312     C     KDST1         setll     FiDST31l
230900070312     c                   do        *hival
231000070312     C     KDST1         reade     FiDST31l
231100070312     c                   if        %eof(fidst31l)
231200070312     c                   leave
231300070312     c                   end
231400070316     c* annullamento
231500120718     c                   if        £dstatb <> ' '
231600070312     C                   eval      dst3ATB = 'A'
231700070312     c                   else
231800070316     c* ripristino
231900070312     C                   CLEAR                   DST3ATB
232000070312     c                   end
232100070312     C                   UPDATE    FiDST300
232200070312     c                   enddo
232300080121     c* allineo i giri ORM
232400080121     c                   if        §OGDDAO <> ' '
232500080121     C     KDST1         setll     FiDST51l
232600080121     c                   do        *hival
232700080121     C     KDST1         reade     FiDST51l
232800080121     c                   if        %eof(fidst51l)
232900080121     c                   leave
233000080121     c                   end
233100080121     c* annullamento
233200120718     c                   if        £dstatb <> ' '
233300080121     C                   eval      dst5ATB = 'A'
233400080121     c                   else
233500080121     c* ripristino
233600080121     C                   CLEAR                   DST5ATB
233700080121     c                   end
233800080121     C                   UPDATE    FiDST500
233900080121     c                   enddo
234000080121     c                   end
234100070517     C                   update    Fidst000
234200070517     c* aggiorna pesi/volumi/fermate assegnate x fidst00f
234300070517     c                   exsr      srfidg37
234400070309     c                   else
234500070316     c* INSERIMENTO
234600070316     c*
234700070316     c* aggancio estensione anagrafico aut. per giri
234800070316     c     kapd21        setgt     fiapd22l
234900070316     c     kapd2         readpe    fiapd22l
235000070316     c                   if        not %eof(fiapd22l) and apd2atb = ' ' and
235100070316     c                             fvvdfv>=apd2dde and fvvdfv<=apd2dscad
235200070316     c                   else
235300070316     c* se aut. non gestisce giri pulisco gli  archivi
235400070316     c                   clear                   fiapd2ds
235500070316    1C                   end
235600070309     c                   movea     tc1           tc1d
235700070309     c                   movea     ts1           ts1d
235800070309     c                   if        apd2ftc = ' '
235900070312     c                   eval      £dstftc = 'E'
236000070309     c                   else
236100070312     c                   eval      £dstftc = apd2ftc
236200070309     c                   end
236300070309     c                   if        apd2fts = ' '
236400070312     c                   eval      £dstfts = 'E'
236500070309     c                   else
236600070312     c                   eval      £dstfts = apd2fts
236700070309     c                   end
236800070312     c                   if        apd2vlu = 0
236900070312     c                   eval      £dstvlu = apdvlu
237000070312     c                   else
237100070312     c                   eval      £dstvlu = apd2vlu
237200070312     c                   end
237300070312     c                   if        apd2pkl = 0
237400070312     c                   eval      £dstpkl = apdpkl
237500070312     c                   else
237600070312     c                   eval      £dstpkl = apd2pkl
237700070312     c                   end
237800070312     c                   eval      £dstnbl = apd2nbl
237900070312     c                   eval      £dstnftl= apd2nftl
238000070312     c                   eval      £dstpsf = apd2psf
238100070312     c                   eval      £dsttol = apd2tol
238200070312     c                   if        apd2ftc = ' '
238300070312     c                   eval      £dstinc = 'S'
238400070309     c                   else
238500070312     c                   eval      £dstinc = apd2inc
238600070309     c                   end
238700080121     c                   if        §OGDDAO <> ' '
238800080121     c                   eval      £dstrvlu = apd2rvlu
238900080121     c                   eval      £dstrpkl = apd2rpkl
239000080121     c                   eval      £dstrnbl = apd2rnbl
239100080121     c                   eval      £dstrnftl= apd2rnftl
239200080121     c                   eval      £dstrtol = apd2rtol
239300080121     c                   if        apd2rinc = ' '
239400080121     c                   eval      £dstrinc = 'S'
239500080121     c                   else
239600080121     c                   eval      £dstrinc = apd2rinc
239700080121     c                   end
239800080121     c                   if        apd2rspi = ' '
239900080121     C*???
240000080121     c                   eval      £dstrspi = 'N'
240100080121     c                   else
240200080121     c                   eval      £dstrspi = apd2rspi
240300080121     c                   end
240400080121     c                   else
240500080121     c                   clear                   £dstrvlu
240600080121     c                   clear                   £dstrpkl
240700080121     c                   clear                   £dstrnbl
240800080121     c                   clear                   £dstrnftl
240900080121     c                   clear                   £dstrtol
241000080121     c                   clear                   £dstrinc
241100080121     c                   clear                   £dstrspi
241200080121     c                   end
241300080121     c                   eval      £DSTrPKLA=0
241400080121     c                   eval      £DSTrVLUA=0
241500080121     c                   eval      £DSTrSTPA=0
241600080121     c                   eval      £DSTrnblA=0
241700080121     c                   eval      £DSTrnbaA=0
241800070309     c* scrivo i giri
241900070309     c                   clear                   fidst300
242000070312     c                   eval      dst3fgs = fvvfgs
242100070312     c                   eval      dst3npg = fvvnpg
242200070312     c                   eval      dst3dfv = fvvdfv
242300070312     c                   eval      dst3nfv = fvvnfv
242400070309     c     apd2nras      setll     fiapd31l
242500070309     c                   do        *hival
242600070309     c     apd2nras      reade     fiapd31l
242700070309     c                   if        %eof(fiapd31l)
242800070309     c                   leave
242900070309     c                   end
243000070309     c                   eval      dst3fgsg= apd3fgsg
243100070309     c                   eval      dst3cgi = apd3cgi
243200070309     c                   eval      dst3prgg= apd3prgg
243300070309     c                   eval      dst3sql = apd3sql
243400070725     c                   clear                   fidg09ds
243500070725     c                   eval      d09ifgs = apd3fgsg
243600070725     c                   eval      d09icgi = apd3cgi
243700070725     c                   eval      D09ITUG = 'C'
243800070725     c                   exsr      srcgi
243900070725     c                   if        d09oerr <>'1'
244000070725     c                   write     fidst300
244100070725     c                   end
244200070309     c                   enddo
244300080121     c* scrivo i giri ORM
244400080121     c                   if        §OGDDAO <> ' '
244500080121     c                   clear                   fidst500
244600080121     c                   eval      dst5fgs = fvvfgs
244700080121     c                   eval      dst5npg = fvvnpg
244800080121     c                   eval      dst5dfv = fvvdfv
244900080121     c                   eval      dst5nfv = fvvnfv
245000080121     c     apd2nras      setll     fiapd51l
245100080121     c                   do        *hival
245200080121     c     apd2nras      reade     fiapd51l
245300080121     c                   if        %eof(fiapd51l)
245400080121     c                   leave
245500080121     c                   end
245600080121     c                   if        apd5atb <> ' '
245700080121     c                   iter
245800080121     c                   end
245900080121     c                   eval      dst5fgsg= apd5fgsg
246000080121     c                   eval      dst5cgi = apd5cgi
246100080121     c                   eval      dst5prgg= apd5prgg
246200080121     c                   eval      dst5sql = apd5sql
246300080121     c                   clear                   fidg09ds
246400080121     c                   eval      d09ifgs = apd5fgsg
246500080121     c                   eval      d09icgi = apd5cgi
246600080121     c                   eval      D09ITUG = 'R'
246700080121     c                   exsr      srcgi
246800080121     c                   if        d09oerr <>'1'
246900080121     c                   write     fidst500
247000080121     c                   end
247100080121     c                   enddo
247200080121     c                   end
247300070517     C                   WRITE     Fidst000
247400070517     c* aggiorna pesi/volumi/fermate assegnate x fidst00f (deve prima essere
247500070517     c* scritto il record su fidst)
247600070517     c                   exsr      srfidg37
247700070309     c                   end
247800090310     c*                  end
247900080506     c* chiamo il pgm per aggiornamento dati pda per chiudere in lr
248000080506     c                   if        §OGPDAcon <> ' ' and
248100080506     c                             (v1cpda =  'C' or v1cpda = 'E' or
248200080506     c                             v1ctstpda = 'C' or
248300080506     c                             v1ctstpda = 'E')
248400080506     c                   eval      co42tla = 'C'
248500080506     c                   exsr      srpda
248600080506     c                   end
248700070309     c*
248800070309     C                   ENDSR
248900070725     C**************************************************************************
249000070725     C* controllo e decodifico giro
249100070725     C**************************************************************************
249200070725     C     Srcgi         BEGSR
249300070725     C*
249400070725     c                   eval      D09IOP0 = '001'
249500070725     c                   move      fvvdfv        d09idat
249600070725     c                   eval      kpjbu = fidg09ds
249700070725     c                   call      'FIDG09R'
249800070725     c                   parm                    kpjba
249900070725     c                   eval      fidg09ds = kpjbu
250000070725     C*
250100070725     C                   ENDSR
250200070309     C*-----------------------------------------------------*
250300070517     C* aggiorna pesi/volumi/fermate assegnate x fidst00f   *
250400070309     C*-----------------------------------------------------*
250500070517     C     srfidg37      BEGSR
250600070328     c*
250700070517     c                   clear                   fidg37ds
250800070517     c                   eval      D37FGSI = fvvfgs
250900070517     c                   eval      D37DFVI = fvvdfv
251000070517     c                   eval      D37NFVI = fvvnfv
251100070517     c                   eval      D37cmtI = 'S'
251200070517     c                   eval      kpjbu = fidg37ds
251300070517     c                   call      'FIDG37C'
251400070517     c                   parm                    kpjba
251500070517     c                   eval      fidg37ds = kpjbu
251600070328     c*
251700070328     C                   ENDSR
251800070328     C*-----------------------------------------------------*
251900070328     C* Controllo che le spedizioni non siano già chiuse    *
252000070328     C*-----------------------------------------------------*
252100070328     C     Ctrarb        BEGSR
252200911018     C*
252300070309     C                   Z-ADD     ROVDDC        KDDC
252400070309     C                   Z-ADD     V1cLNA        KFGS
252500070309     C                   Z-ADD     V01NDC        KNDC
252600070309     C     KARB          SETLL     FNARB70L
252700070309     C                   DO        *HIVAL
252800160720     C     KARB          READE     FNARB70L
252900070309     c                   if        %eof(fnarb70l)
253000070309     c                   leave
253100070309     c                   end
253200120518     c                   if        arbfdc = 'S'
253300070309     c                   seton                                        lr
253400070309     c                   return
253500070309     c                   end
253600070309     c                   enddo
253700070309     C*
253800070309     C                   ENDSR
253900070309     C*-----------------------------------------------------*
254000070309     C* CARICAMENTO  SUBFILE  DAL FILE DI LAVORO            *
254100070309     C*-----------------------------------------------------*
254200070309     C     CARSFL        BEGSR
254300070309     C*
254400940323     C                   Z-ADD     0             XX
254500940323     C                   Z-ADD     0             STP
254600911018     C                   Z-ADD     0             V06STP
254700911018     C                   Z-ADD     *ZEROS        V06PKG
254800911018     C                   Z-ADD     *ZEROS        V06VOL
254900911018     C                   Z-ADD     0             V06TSP
255000911018     C                   Z-ADD     *ZEROS        V06TIN
255100990920     C                   Z-ADD     *ZEROS        V06TEN
255200941108     C                   Z-ADD     *ZEROS        V4CTIV
255300941108     C                   Z-ADD     *ZEROS        V3CSTP
255400990920     C* Abblenco divise
255500011009     C                   MOVEL     §GEDCR        V06DVI
255600990920     C                   MOVEL     DIVEUR        V06DVE
255700941108     C     *LOVAL        SETLL     FNARBW1L
255800911018     C*
255900911018     C                   DO        *HIVAL
256000941108     C                   SETOFF                                       55
256100941108     C                   Z-ADD     *ZEROS        COMASS
256200941108     C                   MOVEL     *BLANKS       COMVCA
256300941109     C                   MOVEL     *BLANKS       AR9TIC
256400941108     C                   READ      FNARBW1L                               55
256500911023     C  N55              DO
256600941108     C                   MOVE      ARBNGD        V3CNGD
256700911022     C     ARBSTP        IFNE      *ZEROS
256800940323     C     ARBSTP        LOOKUP    STP                                    13
256900940323     C     *IN13         IFEQ      '0'
257000940323     C                   ADD       1             V06STP
257100961217     C     XX            IFGE      999
257200961217     C                   SETON                                        79
257300961217     C                   MOVEL     ERR(20)       $ERR
257400961217     C                   END
257500961217     C     XX            IFLT      999
257600940323     C                   ADD       1             XX                3 0
257700940323     C                   Z-ADD     ARBSTP        STP(XX)
257800961128     C                   END
257900911018     C                   END
258000940323     C                   END
258100911022     C     ARBSTP        IFEQ      *ZEROS
258200911022     C                   ADD       1             V06STP
258300911022     C                   END
258400941108     C                   MOVE      ARBSTP        V3CSTP
258500941108     C                   MOVE      ARBSTP        V3CPST
258600911105     C*
258700911105     C* CONTROLLO SE LA BOLLA IN QUESTIONE HA LEGAME CON SECONDA BOLLA
258800941108     C     KARB1         CHAIN     FNLBL01L                           54
258900911113     C     *IN54         IFEQ      '0'
259000950126     C     ARBLNA        ANDEQ     LBLLAP
259100941108     C     KLBL          CHAIN(N)  FNARB01L                           56
259200980429     C*
259300980429     C     *IN56         IFEQ      '0'
259400980429     C     ARBCCA        ANDEQ     '2'
259500980429     C     *IN56         OREQ      '0'
259600980429     C     ARBCCA        ANDEQ     '6'
259700980429     C                   SETON                                        5456
259800980430     C     KARB2         CHAIN     FNARB01L                           30
259900980429     C                   END
260000980429     C*
260100941108     C  N56ARBICC        IFEQ      ' '
260200941108     C     ARBICC        OREQ      'R'
260300941108     C*  Controllo il tipo bolla mamma
260400941108     C                   Z-ADD     1             X                 2 0
260500941108     C                   MOVEL     *BLANKS       V3CCBO
260600941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
260700941108     C     *IN13         IFEQ      '1'
260800941109     C                   MOVEL     D3A(X)        DS3A
260900941108     C*  Se bolla mamma con C/Assegno vado a reperirlo
261000941108     C     §3AFCA        IFNE      0
261100051115     C     KLBL          CHAIN     FIAR901L                           56
261200941108     C     *IN56         IFEQ      '0'
261300941108     C                   Z-ADD     AR9CAS        COMASS           13 3
261400941108     C                   MOVEL     AR9VCA        COMVCA            3
261500941109     C                   MOVEL     AR9TIC        COMTIC            2
261600941108     C                   END
261700941108     C                   END
261800941108     C                   END
261900941108     C                   END
262000941108     C*
262100941108     C     KLBL1         CHAIN(N)  FNARB01L                           56
262200941108     C                   ELSE
262300941108     C     KARB1         CHAIN(N)  FNARB01L                           56
262400941108     C                   END
262500941108     C*
262600941108     C*  Controllo il tipo bolla
262700941108     C                   Z-ADD     1             X                 2 0
262800941108     C                   MOVEL     *BLANKS       V3CCBO
262900951121     C                   MOVEL     *BLANKS       WTBOL1            1
263000941108     C                   CLEAR                   DS3A
263100941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
263200941108     C     *IN13         IFEQ      '1'
263300941109     C                   MOVEL     D3A(X)        DS3A
263400941108     C                   MOVEL     §3ATB1        WTBOL1
263500990924     C                   MOVEL     §3ATB2        WTBOL2
263600941108     C                   END
263700951207     C                   MOVE      ARBKSC        V3CCLI
263800951121     C                   MOVEL     ARBRSD        V3CRAG
263900951121     C                   MOVE      ARBIND        V3CRAG
264000911018     C*
264100911105     C  N56              DO
264200911106     C     ARBFAN        IFEQ      'S'
264300941108     C                   MOVEL     'ANOM.'       V3DANO
264400911106     C                   ELSE
264500941108     C                   MOVEL     *BLANKS       V3DANO
264600911106     C                   END
264700941108     C                   MOVEL     ARBCBO        V3CCBO
264800941108     C                   MOVEL     §3ATB1        V3CTB1
264900941108     C                   MOVEL     *BLANK        V3CSEL
265000941108     C                   MOVEL     ARBRSD        V3CRSD
265100941108     C                   MOVEL     ARBIND        V3CIND
265200941108     C                   MOVEL     ARBLOD        V3CLOD
265300941108     C                   MOVEL     ARBPRD        V3CPRD
265400941108     C                   MOVEL     ARBCAD        V3CCAD
265500110804     C                   MOVEL     ARBxco        V3Cxco
265600941108     C                   Z-ADD     ARBAAS        V3CAAS
265700941108     C                   Z-ADD     ARBLNP        V3CLNP
265800941108     C                   Z-ADD     ARBNRS        V3CNRS
265900941108     C                   Z-ADD     ARBNSP        V3CNSP
266000941108     C                   Z-ADD     ARBLNA        V3CLNA
266100941108     C                   Z-ADD     ARBZNC        V3CZNC
266200941108     C                   Z-ADD     ARBNCL        V3CNCL
266300070612     c* peso in base alle regole VDL
266400070612     c                   exsr      calpes
266500070612     C                   Z-ADD     wpes          V3CPKF
266600070612     c* vol. in base alle regole VDL
266700070612     c                   exsr      calvol
266800070612     C                   Z-ADD     wvol          V3CVLF
266900911018     C                   MOVE      ARBDCR        MMGG              4 0
267000911018     C                   MOVEL     MMGG          MM                2 0
267100911018     C                   MOVE      MMGG          GG                2 0
267200941108     C                   MOVE      MM            V3CGMC
267300941108     C                   MOVEL     GG            V3CGMC
267400941108     C                   MOVE      ARBHCR        V3CHCR
267500941108     C                   MOVE      ARBGC1        V3CGC1
267600941108     C                   MOVE      ARBGC2        V3CGC2
267700941108     C                   MOVE      ARBCBO        V3CCBO
267800941108     C                   MOVE      ARBTSP        V3CTSP
267900941108     C                   Z-ADD     *ZEROS        V3CIMA
268000941109     C                   MOVEL     *BLANKS       V3CVCA
268100990920     C                   MOVEL     *BLANKS       V3CVAS
268200941109     C                   MOVEL     *BLANKS       V3CTIC
268300941109     C                   Z-ADD     *ZEROS        V3CCAS
268400941108     C*
268500941108     C* Decodifico i codici consegna particolare
268600941108     C                   MOVEL     *BLANKS       V3DTC1
268700941108     C                   MOVEL     *BLANKS       V3DTC2
268800941108     C                   Z-ADD     1             XY                3 0
268900941108     C     ARBTC1        LOOKUP    C1P(XY)                                23
269000941108     C     *IN23         IFEQ      '1'
269100941108     C                   MOVEL     D1P(XY)       V3DTC1
269200941108     C                   END
269300941108     C                   Z-ADD     1             XY                3 0
269400941108     C     ARBTC2        LOOKUP    C1P(XY)                                23
269500941108     C     *IN23         IFEQ      '1'
269600941108     C                   MOVEL     D1P(XY)       V3DTC2
269700941108     C                   END
269800990920     C*
269900990920     C* Verifico se devo gestire un assegnato
270000990920     C     WTBOL1        IFNE      *BLANKS
270100990920     C     WTBOL2        ORNE      *BLANKS
270200990920     C                   Z-ADD     ARBAAS        KAAS
270300990920     C                   Z-ADD     ARBLNP        KLNP
270400990920     C                   Z-ADD     ARBNRS        KNRS
270500990920     C                   Z-ADD     ARBNSP        KNSP
270600990920     C* Se devo gestire 2° bolla imposto tipo record '2' x posiz.FIAR6
270700990920     C     WTBOL2        IFNE      *BLANKS
270800990920     C                   MOVE      '2'           KTRC
270900990920     C* .... se devo gestire 1° bolla imposto tipo record '1'
271000990920     C                   ELSE
271100990920     C     WTBOL1        IFNE      'F'
271200990920     C                   MOVE      '1'           KTRC
271300990920     C                   END
271400990920     C                   END
271500990920     C* Aggancio FIAR6 x importo assegnato
271600990920     C     KAR6          CHAIN     FIAR601L                           74
271700990920     C  N74AR6DFT        IFNE      *ZEROS
271800990920     C* Se ho effettuato un ritorno all'incasso controllo se
271900990920     C     ARBICA        IFEQ      'R'
272000990920     C* ... si riferisce ad un incasso parziale
272100990920     C                   CLEAR                   ARIIPP
272200990920     C                   MOVEL     'A'           KTIP
272300990920     C     KARI          CHAIN     FIARI01L                           74
272400990921     C                   MOVEL     AR6DIV        V3CVAS
272500990921     C                   Z-ADD     AR6IFT        V3CIMA
272600990920     C* ... se si calcolo importo da incassare x differenza
272700990921     C     *IN74         DOWEQ     '0'
272800990921     C     AR6DIV        IFEQ      ARIDIV
272900990921     C                   SUB       ARIIPP        V3CIMA
273000990921     C                   ELSE
273100990921     C* Se la divisa di incasso è diversa converto importo già
273200990921     C* incassato nella divisa della bolla
273300990921     C                   CLEAR                   YEURDS
273400990921     C                   MOVEL     ARIDIV        YECDVI
273500990921     C                   Z-ADD     ARIIPP        YECIMI
273600990927     C                   MOVEL     AR6DIV        YECDVO
273700990921     C                   MOVEL     'H'           YECTAR
273800990921     C                   CALL      'YEURCO'
273900990921     C                   PARM                    YEURDS
274000990921     C                   SUB       YECIMO        V3CIMA
274100990921     C                   END
274200990921     C     KARI          READE     FIARI01L                               74
274300990921     C                   END
274400990920     C                   ELSE
274500990920     C* ... altrimenti imposto totale fattura
274600991202     C     ARBICA        IFEQ      ' '
274700991202     C     ARBICA        OREQ      'R'
274800990920     C                   Z-ADD     AR6IFT        V3CIMA
274900990920     C                   MOVEL     AR6DIV        V3CVAS
275000940323     C                   END
275100991202     C                   END
275200941108     C  N74WTBOL1        IFEQ      'F'
275300990920     C                   MOVE      AR6KSC        V3CCLI
275400941108     C                   MOVEL     'A'           V3CTB1
275500911206     C                   END
275600940323     C                   END
275700941108     C*
275800940323     C                   END
275900941108     C                   MOVEL     ARBRSM        V3CRSM
276000911118     C     ARBICC        IFEQ      ' '
276100941108     C     ARBICC        OREQ      'R'
276200941109     C     §3AFCA        IFNE      0
276300941108     C                   Z-ADD     0             AR9CAS
276400941108     C                   MOVEL     *BLANKS       AR9VCA
276500051115     C     KARB1         CHAIN     FIAR901L                           23
276600941109     C     *IN23         IFEQ      '0'
276700941108     C                   Z-ADD     AR9CAS        V3CCAS
276800941108     C                   MOVEL     AR9VCA        V3CVCA
276900941109     C                   MOVE      AR9TIC        V3CTIC
277000941108     C                   END
277100941109     C                   END
277200941108     C     COMASS        IFNE      *ZEROS
277300941108     C                   Z-ADD     COMASS        V3CCAS
277400941109     C                   MOVEL     COMVCA        V3CVCA
277500941109     C                   MOVE      COMTIC        V3CTIC
277600911105     C                   END
277700911113     C                   END
277800911112     C* SE SPED.DI SELEZIONE PRECEDENTE ILLUMINO IL NUMERO SPEDIZIONE
277900911112     C     ARBFLG        IFEQ      '1'
278000911112     C                   SETON                                        35
278100911112     C                   ELSE
278200911112     C                   SETOFF                                       35
278300911112     C                   END
278400911018     C*
278500000515     C*  Controllo se LE SPEDIZIONI SONO AUTOGENERATE
278600000515     C                   SETOFF                                       63
278700000515     C                   MOVEL     'A'           KTRC
278800060214     C     KAR6          CHAIN     FiAR401L                           95
278900000515     C  N95              MOVEL     AR4NOT        DSBL4A
279000000515     C  N95§B4ASO        COMP      '&'                                    63
279100000515     C*
279200911018     C                   ADD       1             NRR
279300911018     C*
279400991006     C* Accendo indicatori x C/Assegno e assegnato
279500160502     C*    V3CCAS        COMP      0                                      59
279600991006     C     V3CIMA        COMP      0                                      41
279700130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
279800130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
279900130221     C*****V3Cxco        COMP      ' '                                8888
280000130221     C     V3Cxco        COMP      '0'                                88
280100941109     C                   WRITE     LR50S03
280200941108     C                   MOVEL     *BLANKS       V3DANO
280300911018     C*
280400911018     C* CALCOLO I TOTALE DA VIDEATA 6
280500911018     C*
280600070612     C                   ADD       wpes          V06PKG
280700070612     C                   ADD       wvol          V06VOL
280800911018     C                   ADD       1             V06TSP
280900990920     C* Sommo totale da incassare in moneta di conto o altra divisa
281000990920     C* permessa
281100990920     C                   SELECT
281200990920     C     V3CVAS        WHENEQ    *BLANKS
281300011009     C     V3CVAS        OREQ      §GEDCR
281400990920     C                   ADD       V3CIMA        V06TIN
281500990920     C* ... altra divisa valida
281600990920     C     V3CVAS        WHENEQ    DIVEUR
281700990920     C                   ADD       V3CIMA        V06TEN
281800990920     C                   ENDSL
281900990920     C* Somma nel totale corretto: moneta di conto
282000990920     C                   SELECT
282100990920     C     V3CVCA        WHENEQ    *BLANKS
282200011009     C     V3CVCA        OREQ      §GEDCR
282300941108     C                   ADD       V3CCAS        V06TIN
282400990920     C* ... altra divisa valida
282500990920     C     V3CVCA        WHENEQ    DIVEUR
282600990920     C                   ADD       V3CCAS        V06TEN
282700990920     C* ... divise non Unione Europea
282800990920     C                   OTHER
282900941108     C                   ADD       V3CCAS        V4CTIV
283000990920     C                   ENDSL
283100911018     C                   END
283200911105     C*
283300911105     C                   END
283400911018     C*
283500911021     C  N55              END
283600911018     C*
283700911018     C                   ENDSR
283800911018     C*-----------------------------------------------------*
283900911018     C* GESTIONE FORMATO 6 TOTALI E CONFERMA                *
284000911018     C*-----------------------------------------------------*
284100911018     C     GESFM6        BEGSR
284200911018     C*
284300930428     C                   SETOFF                                       0706
284400050204     C                   MOVEL     'N'           V06ban
284500050207     C                   MOVEL     'N'           V06SIN
284600911021     C                   DO        *HIVAL                                       1
284700911018     C*
284800991117     C     V06TIN        COMP      0                                  0808
284900991117     C     V06TEN        COMP      0                                  0909
285000991117     C     V4CTIV        COMP      0                                  5353
285100941108     C                   EXFMT     LR50D04
285200070315     c* prima di aggiornare una distinta controllo che non
285300070315     c* sia stata sbloccata da altri e quindi già chiusa per evitare di
285400070315     c* perdere i riferimenti della chiusura distinta. Nel caso forzo
285500070315     c* l'uscita
285600070315     c                   if        v01ndc <> 0
285700070315     c                   exsr      ctrarb
285800070315     c                   end
285900911018     C*
286000911022     C  N80              DO                                                     2
286100911018     C*
286200941108     C   KF              SETON                                        0706
286300911018     C*
286400941108     C  NKF*IN07         IFEQ      '1'                                          3
286500950907     C   02              MOVEL     '1'           *INKC
286600911018     C* CONTROLLO SE ERO IN MODIFICA OPPURE IN CREAZIONE NUOVA DISTINTA
286700941107     C     V01NDC        IFEQ      *ZEROS                                       4
286800020905     c* prende il numero della distinta del po in gestione
286900020905     c                   do        *hival
287000020905     C                   CLEAR                   TRUL33DS
287100020905     c                   eval      i33tla = 'L'
287200020905     C                   Z-ADD     0             I33AAA
287300020905     C                   Z-ADD     12            I33CNU
287400020905     C                   Z-ADD     V1CLNA        I33PO1
287500020905     C                   Z-ADD     1             I33NUM
287600020905     c                   movel(p)  trul33ds      kpjbu
287700020905     c                   call      'TRUL33R'
287800020905     c                   parm                    kpjba
287900020905     c                   movel     kpjbu         trul33ds
288000020905     C                   IF        O33ERR = 0
288100020905     c* controllo che il numero non è già stato usato
288200020905     c                   clear                   fnlr6ds
288300020905     c                   eval      d6tla = 'L'
288400020905     C                   Z-ADD     8             D6npg
288500020905     C                   Z-ADD     V1CLNA        D6fgs
288600020905     C                   Z-ADD     O33Nrf        D6NDT
288700020905     c                   movel(p)  fnlr6ds       kpjbu
288800020905     c                   call      'FNLR6R'
288900020905     c                   parm                    kpjba
289000020905     c                   movel     kpjbu         fnlr6ds
289100020905     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
289200020905     c* come foglio o in una bolla
289300020905     c                   if        d6flg <> 'N'
289400020905     c                   iter
289500020905     c                   else
289600020905     C                   Z-ADD     O33Nrf        V01NDC
289700020905     c                   leave
289800020905     C                   END
289900020905     C                   END
290000020905     C                   ENDdo
290100020905     C                   MOVEL     V01NDC        V1CNDC
290200941107     C                   Z-ADD     V01NDC        FVVNFV            5 0
290300911018     C*
290400911018     C* VALORIZZO I CAMPI PER LA SCRITTURA SU FLVV00
290500911021     C*
290600950601     C                   MOVEL     *BLANKS       FVVATB
290700911021     C*
290800941109     C                   MOVEL     V1CLNA        FVVFGS
290900020916     C                   MOVEL     simfel        FVVFLE
291000911021     C*
291100941109     C                   Z-ADD     ROVDDC        FVVDFV
291200941109     C                   MOVEL     V06PAD        FVVDSF
291300941109     C                   MOVEL     *BLANK        FVVFCF
291400941109     C                   Z-ADD     4             FVVNPG
291500941109     C                   MOVE      *BLANK        FVVSPG
291600941109     C                   MOVE      *BLANK        FVVSPN
291700911021     C                   MOVEL     'S'           FVVFSC
291800941109     C                   MOVEL     V1CFPP        FVVFPP
291900950601     C                   MOVE      V1CPEP        FVVPEP
292000050523     c                   clear                   dfvvspn
292100050523     C                   MOVEl     V1Ctpm        §fvvtpm
292200050523     c                   if        v1ctpm = 'P'
292300050523     C                   MOVEl     *blanks       §fvvtpm
292400050523     c                   end
292500160429     C                   MOVEl     V1Ctur        §fvvturno
292600160429     c                   if        v1ctur = '1'
292700160429     C                   MOVEl     *blanks       §fvvturno
292800160429     c                   end
292900050523     c                   movel     dfvvspn       fvvspn
293000000515     C                   WRITE     FNFVV000
293100000515      *SCRIVO FOGLIO TIPO 8
293200000515     C                   Z-ADD     8             FVVNPG
293300000515     C                   WRITE     FNFVV000
293400000515     C                   Z-ADD     4             FVVNPG
293500041007     C* gestione file dei rientri
293600041007     c                   move      v1clna        wlna
293700041007     c                   z-add     v01ndc        wdis
293800041007     C     Kqdt          chain     fiqdt01l                           83
293900041007     c   83              clear                   fiqdt000
294000041007     C                   movel     v1clna        qdtfgs
294100041007     C                   z-add     v01ndc        qdtnfv
294200041007     C                   z-add     codpad        qdtpdr
294300041007     C                   z-add     fvvdfv        qdtddc
294400110311     C   83              z-add     fvvdfv        qdtdaiaf
294500110311     C   83              movel     whhdat        qdtoraaf
294600110311     C   83              move      *zeros        qdtnbnaf
294700110311     C   83              move      apdptp        qdtnbnaf
294800110311     C   83              move      'S'           qdtfnbaf
294900041007     c   83              write     fiqdt000
295000041007     c  n83              update    fiqdt000
295100950601     C*
295200911021     C* PER CONFERMA VADO A VARIARE/IMPUTARE IL NUMERO E DATA DISTINTA,
295300911021     C* GIRO,STOP E PROGRESSIVO DI STAMPA
295400911021     C*
295500941108     C     *LOVAL        SETLL     FNARBW1L
295600911021     C*
295700030722     C*                  Z-ADD     *ZEROS        PRGSTP            3 0
295800911021     C                   DO        *HIVAL                                       4
295900911108     C                   SETOFF                                           55
296000941108     C                   READ      FNARBW1L                               55
296100911021     C  N55              DO                                                     5
296200911021     C                   Z-ADD     ARBSTP        WSTP              3 0
296300911021     C                   Z-ADD     ARBNGD        WNGD              3 0
296400080505     c                   clear                   fidg42ds
296500911021     C*
296600941108     C     KARB1         CHAIN     FNARB01L                           56
296700930428     C  N56              DO
296800110127     c* controllo che la spedizione non sia già stata inserita in altra
296900110202     c* distinta eccetto che per i ritorni all'incasso perchè mantengono
297000110202     c* la vecchia distinta
297100110127     c                   if        arbndc <> 0 and arbndc <> v01ndc
297200110202     c                             and arbicc <> 'R' and arbica <> 'R'
297300110128     c                   seton                                        90
297400110202     c                   eval      $msg = 'Alcune spedizioni non sono state agg-
297500110202     c                             iornate correttamente. VERIFICARE!!!'
297600110127     c                   iter
297700110127     c                   end
297800980306     C* Controllo se devo scrivere MIC (evento messa in consegna)
297900980310     C     ARBDCM        IFEQ      0
298000980306     C                   EXSR      WRTMIC
298100980310     C                   END
298200980306     C*
298300020905     C                   Z-ADD     V1CLNA        ARBIFP
298400020905     C                   Z-ADD     V01NDC        ARBNDC
298500911021     C                   Z-ADD     ROVDDC        ARBDDC
298600930428     C                   Z-ADD     0             ARBNMI
298700930428     C                   MOVEL     *BLANKS       ARBFDC
298800030722     C                   Z-ADD     0             ARBNSS
298900911021     C                   Z-ADD     WSTP          ARBSTP
299000911021     C                   Z-ADD     WNGD          ARBNGD
299100080506     c                   if        codpad <> arbpdc AND ARBPDC <> 0
299200080505     c                   eval      co42tla = 'A'
299300080505     c                   else
299400080505     c                   clear                   co42tla
299500080505     c                   end
299600080505     C                   Z-ADD     CODPAD        ARBPDC
299700911021     C*
299800941109     C                   UPDATE    FNARB000
299900941109     C                   DELETE    FNARBW00
300000030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
300100030910     c* deve andare a legare l'orm alla distinta
300200070227     c*                  if        spunta <> *blank
300300030910     c                   exsr      srorm
300400070227     c*                  endif
300500070319     c* mettro i riferimenti della distinta anche su FIARG x evitare di
300600070319     c* elaborare la spedizione nelle distinte automatiche
300700070319     c                   exsr      srfiarg
300800080410     c* richiamo il pgm per aggiornare i dati per PDA
300900080410     c                   if        §OGPDAcon <> ' ' and
301000080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
301100080411     c                             v1ctstpda = 'C' or
301200080411     c                             v1ctstpda = 'E')
301300080505     c                   if        co42tla = 'A'
301400080505     c                   exsr      srpda
301500080505     c                   end
301600080410     c                   eval      co42tla = 'I'
301700080410     c                   exsr      srpda
301800080410     c                   end
301900911113     C                   END
302000911021     C*
302100930428     C                   END
302200930428     C  N55              END
302300911105     C                   ELSE
302400950601     C*
302500041007     C* gestione file dei rientri
302600041007     c                   move      v1clna        wlna
302700041007     c                   z-add     v01ndc        wdis
302800041007     C     Kqdt          chain     fiqdt01l                           83
302900041007     c   83              clear                   fiqdt000
303000041007     C                   movel     v1clna        qdtfgs
303100041007     C                   z-add     v01ndc        qdtnfv
303200041007     C                   z-add     codpad        qdtpdr
303300041007     C                   z-add     fvvdfv        qdtddc
303400041007     c   83              write     fiqdt000
303500041007     c  n83              update    fiqdt000
303600120718     C*  Aggiorno FiDST
303700120718     c                   exsr      srfidst
303800911128     C*
303900960702     C                   Z-ADD     ROVDDC        KDDC
304000020905     C                   Z-ADD     V1cLNA        KFGS
304100020905     C                   Z-ADD     V01NDC        KNDC
304200020905     C     KARB          SETLL     FNARB70L
304300911105     C*
304400930608     C                   DO        *HIVAL
304500941108     C                   SETOFF                                           50
304600911105     C*
304700020905     C     KARB          READE     FNARB70L                               50
304800911105     C*
304900930428     C  N50              DO
305000911105     C*
305100080318     C     KARB1         CHAIN(N)  FNARBW2L
305200080326     C                   if        not %found(fnarbw2l)
305300980309     C* Se era in consegna e adesso non c'è più annullo la messa
305400980309     C* consegna
305500980309     C     KARB1         CHAIN     FNARB01L                           31
305600110127     c*
305700980310     C     ARBDCM        IFEQ      0
305800980309     C                   EXSR      ANNMIC
305900980310     C                   END
306000950602     C                   Z-ADD     *ZEROS        ARBNSS
306100020905     C                   Z-ADD     *ZEROS        ARBIFP
306200020905     C                   Z-ADD     *ZEROS        ARBNDC
306300911111     C                   Z-ADD     *ZEROS        ARBDDC
306400911111     C                   Z-ADD     *ZEROS        ARBSTP
306500911111     C                   Z-ADD     *ZEROS        ARBNGD
306600941108     C                   Z-ADD     *ZEROS        ARBPDC
306700941109     C  N31              UPDATE    FNARB000
306800030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
306900030910     c* deve andare a togliere la distinta dall'orm
307000030910     c                   exsr      srorm
307100070319     c* tolgo i riferimenti della distinta anche su FIARG x
307200070319     c* elaborare eventualmente la spedizione nelle distinte automatiche
307300070319     c                   exsr      srfiarg
307400080410     c* richiamo il pgm per aggiornare i dati per PDA
307500080410     c                   if        §OGPDAcon <> ' ' and
307600080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
307700080411     c                             v1ctstpda = 'C' or
307800080411     c                             v1ctstpda = 'E')
307900080410     c                   clear                   fidg42ds
308000080410     c                   eval      co42tla = 'A'
308100080410     c                   exsr      srpda
308200080410     c                   end
308300930428     C                   END
308400930428     C                   END
308500911111     C  N50              END
308600911111     C* PER CONFERMA VADO A VARIARE/IMPUTARE IL NUMERO E DATA DISTINTA,
308700911111     C* GIRO,STOP E PROGRESSIVO DI STAMPA
308800080505     c                   clear                   fidg42ds
308900911111     C*
309000941109     C     *LOVAL        SETLL     FNARBW1L
309100911111     C*
309200030722     C*                  Z-ADD     *ZEROS        PRGSTP            3 0
309300911111     C                   DO        *HIVAL                                       4
309400911111     C                   SETOFF                                           55
309500941109     C                   READ      FNARBW1L                               55
309600911111     C  N55              DO                                                     5
309700911111     C                   Z-ADD     ARBSTP        WSTP              3 0
309800911111     C                   Z-ADD     ARBNGD        WNGD              3 0
309900911111     C*
310000941109     C     KARB1         CHAIN     FNARB01L                           56
310100930428     C  N56              DO
310200110127     c* controllo che la spedizione non sia già stata inserita in altra
310300110202     c* distinta eccetto che per i ritorni all'incasso perchè mantengono
310400110202     c* la vecchia distinta
310500110202     c                   if        arbndc <> 0 and arbndc <> v01ndc
310600110202     c                             and arbicc <> 'R' and arbica <> 'R'
310700110128     c                   seton                                        90
310800110202     c                   eval      $msg = 'Alcune spedizioni non sono state agg-
310900110202     c                             iornate correttamente. VERIFICARE!!!'
311000110127     c                   iter
311100110127     c                   end
311200980309     C* Controllo se devo scrivere MIC (evento messa in consegna)
311300980310     C     ARBDCM        IFEQ      0
311400980309     C                   EXSR      WRTMIC
311500980310     C                   END
311600980309     C*
311700020905     C                   Z-ADD     V1CLNA        ARBIFP
311800020905     C                   Z-ADD     V01NDC        ARBNDC
311900911111     C                   Z-ADD     ROVDDC        ARBDDC
312000930428     C                   Z-ADD     0             ARBNMI
312100930428     C                   MOVEL     *BLANKS       ARBFDC
312200030722     C                   Z-ADD     0             ARBNSS
312300911111     C                   Z-ADD     WSTP          ARBSTP
312400911111     C                   Z-ADD     WNGD          ARBNGD
312500080506     c                   if        codpad <> arbpdc AND ARBPDC <> 0
312600080505     c                   eval      co42tla = 'A'
312700080505     c                   else
312800080505     c                   clear                   co42tla
312900080505     c                   end
313000080505     C                   Z-ADD     CODPAD        ARBPDC
313100911111     C*
313200941109     C                   UPDATE    FNARB000
313300941109     C                   DELETE    FNARBW00
313400030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
313500030910     c* deve andare a legare l'orm alla distinta
313600030910     c                   exsr      srorm
313700070319     c* metto i riferimenti della distinta anche su FIARG x evitare di
313800070319     c* elaborare la spedizione nelle distinte automatiche
313900070319     c                   exsr      srfiarg
314000080410     c* richiamo il pgm per aggiornare i dati per PDA
314100080410     c                   if        §OGPDAcon <> ' ' and
314200080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
314300080411     c                             v1ctstpda = 'C' or
314400080411     c                             v1ctstpda = 'E')
314500080505     c                   if        co42tla = 'A'
314600080505     c                   exsr      srpda
314700080505     c                   end
314800080410     c                   eval      co42tla = 'I'
314900080410     c                   exsr      srpda
315000080410     c                   end
315100911114     C                   END
315200911111     C*
315300930428     C                   END
315400930428     C  N55              END
315500140930     C* IN CASO DI VARIZIONE DISTINTA GIA' ESISTENTE
315600140930     C* E SE DA TOGLIERE DALLA DISTINTA
315700140930     C                   Z-ADD     V01NDC        FVVNFV            5 0
315800140930     C                   Z-ADD     V1CLNA        FVVFGS
315900140930     C*
316000140930     C* VALORIZZO I CAMPI PER LA SCRITTURA SU FLVV00
316100140930     C*
316200140930     C     KFVV          CHAIN     FNFVV01L                           83
316300140930     C  N83              DO
316400140930     C                   MOVEL     *BLANKS       FVVATB
316500140930     C*
316600140930     C                   MOVEL     V1CLNA        FVVFGS
316700140930     C                   MOVEL     simfel        FVVFLE
316800140930     C*
316900140930     C                   MOVEL     V06PAD        FVVDSF
317000140930     C                   MOVEL     *BLANK        FVVFCF
317100140930     C                   Z-ADD     4             FVVNPG
317200140930     C                   MOVEL     'S'           FVVFSC
317300140930     C                   MOVEL     V1CFPP        FVVFPP
317400140930     C                   MOVEL     V1CPEP        FVVPEP
317500140930     C                   MOVEL     fvvspn        dfvvspn
317600140930     C                   MOVEL     v1ctpm        §fvvtpm
317700140930     c                   if        v1ctpm = 'P'
317800140930     C                   MOVEL     *blanks       §fvvtpm
317900140930     c                   end
318000160429     C                   MOVEL     v1ctur        §fvvturno
318100160429     c                   if        v1ctur = '1'
318200160429     C                   MOVEL     *blanks       §fvvturno
318300160429     c                   end
318400140930     C                   MOVEL     dfvvspn       fvvspn
318500140930     C*
318600140930     C                   UPDATE    FNFVV000
318700140930      *GESTIONE FOGLIO TIPO 8
318800140930     C                   MOVE      FVVDS         FVVDS2
318900140930     C                   Z-ADD     8             FVVNPG
319000140930     C     KFVV          CHAIN     FNFVV000                           83
319100140930     C     *IN83         IFEQ      *OFF
319200140930     C                   MOVE      FVVDS2        FVVDS
319300140930     C                   Z-ADD     8             FVVNPG
319400140930     C                   UPDATE    FNFVV000
319500140930     C                   MOVE      FVVDS2        FVVDS
319600140930     C                   END
319700140930     C                   Z-ADD     4             FVVNPG
319800140930     C                   END
319900930428     C                   END
320000070312     c* aggiorno il file FIDST00F
320100070312     c                   exsr      srfidst
320200080320     c* se variato il cod. aut. richiamo pgm orm se pda partito
320300080320     c                   exsr      srchgcpc
320400070312     C*
320500911023     C   55              SETON                                        8081
320600920304     C   50              EXSR      PULRIC
320700911021     C*
320800041008      * richiesta gestione inserimento bancali
320900041008     c                   if        v06ban  = 'S'
321000041008     c                   clear                   fnlrf8ds
321100041008     c                   z-add     fvvnfv        rf8dis
321200041008     c                   z-add     fvvfgs        rf8fgs
321300041008     c                   movel(p)  fnlrf8ds      kpjbu
321400041008     c                   call      'FNLRF8R'
321500041008     c                   parm                    kpjba
321600041008     c                   endif
321700911105     C* SE CHIEDO STAMPA
321800010216     C     V06SIN        IFNE      'N'
321900941109     C                   CLEAR                   FNLR55
322000950102     C                   Z-ADD     ROVDDC        D55DAT
322100020905     C                   Z-ADD     ARBNDC        D55NRD
322200950102     C                   Z-ADD     ARBNDC        D55NRA
322300950102     C                   MOVEL     'A'           D55TIP
322400950102     C                   MOVEL     V1CLNA        D55FIL
322500041007     C*                  Z-ADD     DSTNBN        D55NBN
322600041007     C                   Z-ADD     qdtnbnaf      D55NBN
322700050207     C                   MOVEL     QDTFNBAF      D55fba
322800120718     C                   MOVEL     £DSTNOT       D55NOT
322900010216     C                   MOVEL     V06SIN        D55STP
323000941109     C                   MOVEL     FNLR55        KPJBU
323100050427     C                   CALL      'FNLR55C'
323200911128     C                   PARM                    KPJBA
323300911105     C                   END
323400941107     C                   MOVE      *BLANKS       V1CCPC
323500941109     C                   MOVE      WHHDAT        V1CDDC
323600930428     C                   END
323700911021     C                   END
323800911022     C  N80              END
323900950907     C*
324000950907     C     D50TPR        IFEQ      ' '
324100941107     C                   MOVEL     V01NDC        V1CNDP
324200941107     C                   MOVE      *BLANK        V1CNDC
324300080208     c                   clear                   msgv
324400080318     c                   clear                   msgv1
324500080415     c                   setoff                                       45
324600941110     C                   MOVE      *ZEROS        SAVNDC
324700920423     C                   MOVE      *BLANKS       SAVCPC
324800950608     C                   MOVE      WHHDAT        V1CDDC
324900081028     c* non deve pulire i campi della prima videata se premuto f12
325000081028     c                   if        not *inkl
325100080306     C                   MOVE      *BLANK        V1CPEP
325200080306     C                   MOVE      *blanks       V1Ctpm
325300160429     C                   MOVEl     *blanks       V1Ctur
325400080306     c                   clear                   v1cpda
325500080306     c                   clear                   v1ctstpda
325600080306     c                   end
325700950907     C                   END
325800911021     C*
325900911021     C                   ENDSR
326000980306     C*-----------------------------------------------------*
326100980306     C* WRTMIC: Ctrl se devo inviare "Messa in consegna"    *
326200980306     C*-----------------------------------------------------*
326300980306     C     WRTMIC        BEGSR
326400980306     C*
326500980311     C* Scrivo evento di messa in consegna solo se il numero
326600980311     C* distinta sulla bolla è diverso da quella attuale
326700980311     C* (Sono in fase di immissione speidizione in distinta)
326800131031      **
326900131104     c                   clear                   Devb01
327000131104     C                   move      *zeros        §NOTNDC
327100131104     C                   move      *zeros        §NOTPDC
327200131031     C                   move      V01NDC        §NOTNDC
327300131031     C                   move      CODPAD        §NOTPDC
327400131031      **
327500980311     C     V01NDC        IFNE      ARBNDC
327600980306     C* Controllo se ho già scritto evento MIC
327700980318     C     KARB1         CHAIN     FNLBL01L                           87
327800980318     C     *IN87         IFEQ      '0'
327900980318     C     ARBLNA        ANDEQ     LBLLAP
328000980429     C*
328100980429     C     KLBL          CHAIN     FNARB01L                           30
328200980429     C     *IN56         IFEQ      '0'
328300980429     C     ARBCCA        ANDEQ     '2'
328400980429     C     *IN56         OREQ      '0'
328500980429     C     ARBCCA        ANDEQ     '6'
328600980429     C                   SETON                                        87
328700980429     C                   END
328800980429     C     KARB2         CHAIN     FNARB01L                           30
328900980429     C                   END
329000980429     C*
329100980429     C     *IN87         IFEQ      '0'
329200980429     C     ARBLNA        ANDEQ     LBLLAP
329300980318     C                   Z-ADD     LBLAAP        KAAS
329400980318     C                   Z-ADD     LBLLPP        KLNP
329500980318     C                   Z-ADD     LBLNRP        KNRS
329600980318     C                   Z-ADD     LBLNSP        KNSP
329700980318     C                   ELSE
329800980306     C                   Z-ADD     ARBAAS        KAAS
329900980306     C                   Z-ADD     ARBLNP        KLNP
330000980306     C                   Z-ADD     ARBNRS        KNRS
330100980306     C                   Z-ADD     ARBNSP        KNSP
330200980318     C                   END
330300031017      * verifica se deve scrivere il mic se esiste almeno una spunta lo scrive
330400031017     c                   exsr      eventimic
330500031017     c     spunta        cabeq     *blank        nomic
330600031017
330700980306     C                   MOVEL     'MIC'         KCEV
330800990803     C     KEVB          SETGT     FNEVB04L
330900990803     C     KEVB          READPE    FNEVB04L                               33
331000980311     C* Se l'evento di messa in consegna non esiste o esista ma
331100980311     C* ha data diversa dalla distinta di consegna scrivo un
331200980311     C* nuovo evento di messa in consegna
331300980311     C     *IN33         IFEQ      '1'                                          No Trv Mic
331400990803     C     FVVDFV        ORNE      EVBDEV
331500990803     C                   CLEAR                   FNEVB000
331600980309     C                   MOVEL     'MIC'         EVBCEV
331700980309     C                   Z-ADD     DATEU         EVBDTV
331800980309     C                   TIME                    EVBORV
331900980309     C                   Z-ADD     V1CLNA        EVBFLE
332000020905     C                   Z-ADD     DUTCOU        EVBCDU
332100980318     C                   Z-ADD     KAAS          EVBAAS
332200980318     C                   Z-ADD     KLNP          EVBLNP
332300980318     C                   Z-ADD     KNRS          EVBNRS
332400980318     C                   Z-ADD     KNSP          EVBNSP
332500980309     C                   Z-ADD     FVVDFV        EVBDEV
332600980309     C     FVVFPP        IFEQ      'M'
332700980310     C                   MOVEL     0800          EVBHEV
332800980309     C                   ELSE
332900980310     C                   MOVEL     1200          EVBHEV
333000980309     C                   END
333100131031     C                   MOVEL     Devb01        EVBNOT
333200990803     C                   MOVEL     *BLANKS       EVBDTC
333300990803     C                   WRITE     FNEVB000
333400980311     C                   ELSE                                                   No Trv Mic
333500980311     C* Se l'evento c'è e ha data uguale alla distinta controllo
333600980311     C* se è già stato trasmesso
333700980311     C     EVBDTR        IFEQ      0                                            MIC trasm
333800980311     C* Se non è stato trasmesso aggiorno la data e l'ora di
333900980311     C* di immissione evento con quella del giorno + altri dati
334000980311     C                   Z-ADD     DATEU         EVBDTV
334100980311     C                   TIME                    EVBORV
334200980311     C                   Z-ADD     V1CLNA        EVBFLE
334300020905     C                   Z-ADD     DUTCOU        EVBCDU
334400131031     C                   MOVEL     Devb01        EVBNOT
334500980311     C     FVVFPP        IFEQ      'M'
334600980311     C                   MOVEL     0800          EVBHEV
334700980311     C                   ELSE
334800980311     C                   MOVEL     1200          EVBHEV
334900980311     C                   END
335000990803     C                   MOVEL     *BLANKS       EVBDTC
335100990803     C                   UPDATE    FNEVB000
335200980311     C* Altrimenti scrivo un nuovo evento di messa in consegna
335300980311     C                   ELSE                                                   No Trasm
335400990803     C                   CLEAR                   FNEVB000
335500980311     C                   MOVEL     'MIC'         EVBCEV
335600980311     C                   Z-ADD     DATEU         EVBDTV
335700980311     C                   TIME                    EVBORV
335800980311     C                   Z-ADD     V1CLNA        EVBFLE
335900020905     C                   Z-ADD     DUTCOU        EVBCDU
336000980318     C                   Z-ADD     KAAS          EVBAAS
336100980318     C                   Z-ADD     KLNP          EVBLNP
336200980318     C                   Z-ADD     KNRS          EVBNRS
336300980318     C                   Z-ADD     KNSP          EVBNSP
336400980311     C                   Z-ADD     FVVDFV        EVBDEV
336500980311     C     FVVFPP        IFEQ      'M'
336600980311     C                   MOVEL     0800          EVBHEV
336700980311     C                   ELSE
336800980311     C                   MOVEL     1200          EVBHEV
336900980311     C                   END
337000131031     C                   MOVEL     Devb01        EVBNOT
337100990803     C                   MOVEL     *BLANKS       EVBDTC
337200990803     C                   WRITE     FNEVB000
337300980311     C                   END                                                    Mic Trasm
337400980311     C*
337500980311     C                   END                                                    Trv MIC
337600980309     C*
337700980311     C                   END                                                    ARBNDC<>
337800980309     C*
337900031017     C     nomic         ENDSR
338000031017     C*-----------------------------------------------------*
338100031017     C* verifica esistenza spunte per scrittura MIC         *
338200031017     C*-----------------------------------------------------*
338300031017     C     eventimic     BEGSR
338400031017     c                   clear                   spunta            1
338500031017     c                   z-add     v01ndc        knf6
338600031020     C                   Z-ADD     V1CLNA        KFGS3
338700031017     c     kart          setll     fnart01l
338800031017     c                   do        *hival
338900031017     c     kart          reade     fnart01l                               96
339000031017     c   96              leave
339100070118     c     kbrv          chain     fnbrv01l
339200070118     c                   if        %found(fnbrv01l)
339300031017     c                   move      'X'           spunta
339400031017     c                   leave
339500031017     c                   endif
339600031017     c                   enddo
339700031017     c                   endsr
339800980309     C*-----------------------------------------------------*
339900980309     C* ANNMIC: Annullo evento di  "Messa in consegna"      *
340000980309     C*-----------------------------------------------------*
340100980309     C     ANNMIC        BEGSR
340200980309     C*
340300980311     C* Se non sono in immissione (nr.distinta bolla=video)
340400980311     C     V01NDC        IFEQ      ARBNDC
340500980311     C* Controllo se ho scritto evento MIC
340600980318     C     KARB1         CHAIN     FNLBL01L                           87
340700980429     C*
340800980318     C     *IN87         IFEQ      '0'
340900980318     C     ARBLNA        ANDEQ     LBLLAP
341000980429     C     KLBL          CHAIN     FNARB01L                           30
341100980429     C     *IN56         IFEQ      '0'
341200980429     C     ARBCCA        ANDEQ     '2'
341300980429     C     *IN56         OREQ      '0'
341400980429     C     ARBCCA        ANDEQ     '6'
341500980429     C                   SETON                                        87
341600980429     C                   END
341700980429     C     KARB2         CHAIN     FNARB01L                           30
341800980527     C                   END
341900980429     C*
342000980429     C     *IN87         IFEQ      '0'
342100980429     C     ARBLNA        ANDEQ     LBLLAP
342200980318     C                   Z-ADD     LBLAAP        KAAS
342300980318     C                   Z-ADD     LBLLPP        KLNP
342400980318     C                   Z-ADD     LBLNRP        KNRS
342500980318     C                   Z-ADD     LBLNSP        KNSP
342600980318     C                   ELSE
342700980309     C                   Z-ADD     ARBAAS        KAAS
342800980309     C                   Z-ADD     ARBLNP        KLNP
342900980309     C                   Z-ADD     ARBNRS        KNRS
343000980309     C                   Z-ADD     ARBNSP        KNSP
343100980318     C                   END
343200980309     C                   MOVEL     'MIC'         KCEV
343300990803     C     KEVB          SETGT     FNEVB04L
343400990803     C     KEVB          READPE    FNEVB04L                               33
343500980309     C     *IN33         IFEQ      '0'
343600990803     C     FVVDFV        ANDEQ     EVBDEV
343700980311     C* Se MIC esiste e la data della distinta è la stessa
343800980311     C* dell'evento ... controllo se è già trasmesso
343900980311     C     EVBDTR        IFNE      0
344000990803     C                   MOVEL     EVBDEV        WDTMIC           12 0
344100980311     C                   MOVE      EVBHEV        WDTMIC
344200980311     C* se l'evento di messa in consegna è stato trasmesso
344300980311     C* controllo se esiste già un evento NIC tolto dalla
344400980311     C* consegna
344500980311     C                   MOVEL     'NIC'         KCEV
344600990803     C     KEVB          SETGT     FNEVB04L
344700990803     C     KEVB          READPE    FNEVB04L                               33
344800031021     C* SE HO TROVATO EVENTO NIC
344900980311     C     *IN33         IFEQ      '0'                                          Wtr Nic
345000990803     C     FVVDFV        ANDEQ     EVBDEV
345100031021     C* Se non è stato trasmesso aggiorno la data e l'ora di
345200031021     C* di immissione evento con quella del giorno + altri dati
345300031021     C     EVBDTR        IFEQ      0
345400031021     C                   move      dateu         EVBDTV
345500031021     C                   TIME                    EVBORV
345600031021     C                   Z-ADD     v1clna        EVBFLE
345700031021     C                   Z-ADD     DUTCOU        EVBCDU
345800031021     C     FVVFPP        IFEQ      'M'
345900031021     C                   MOVEL     0800          EVBHEV
346000031021     C                   ELSE
346100031021     C                   MOVEL     1200          EVBHEV
346200031021     C                   END
346300031021     C                   MOVEL     *BLANKS       EVBDTC
346400031021     C                   UPDATE    FNEVB000
346500031021     C* Se è stato trasmesso controllo se non ha data più
346600031021     C* alta del MIC
346700031021     C                   ELSE
346800031021     C                   MOVEL     EVBDEV        WDTNIC           12 0
346900031021     C                   MOVE      EVBHEV        WDTNIC
347000031021     C     WDTNIC        IFLE      WDTMIC
347100031021     C* Se la data non è più alta scrivo un nuovo NIC
347200031021     C                   CLEAR                   FNEVB000
347300031021     C                   MOVEL     'NIC'         EVBCEV
347400031021     C                   move      dateu         EVBDTV
347500031021     C                   TIME                    EVBORV
347600031021     C                   Z-ADD     v1clna        EVBFLE
347700031021     C                   Z-ADD     DUTCOU        EVBCDU
347800031021     C                   Z-ADD     KAAS          EVBAAS
347900031021     C                   Z-ADD     KLNP          EVBLNP
348000031021     C                   Z-ADD     KNRS          EVBNRS
348100031021     C                   Z-ADD     KNSP          EVBNSP
348200031021     C                   Z-ADD     FVVDFV        EVBDEV
348300031021     C     FVVFPP        IFEQ      'M'
348400031021     C                   MOVEL     0800          EVBHEV
348500031021     C                   ELSE
348600031021     C                   MOVEL     1200          EVBHEV
348700031021     C                   END
348800131104     c                   clear                   Devb01
348900131104     C                   move      *zeros        §NOTNDC
349000131104     C                   move      *zeros        §NOTPDC
349100131031     C                   move      ARBNDC        §NOTNDC
349200131031     C                   move      ARBPDC        §NOTPDC
349300131031     C                   MOVEL     Devb01        EVBNOT
349400031021     C                   MOVEL     *BLANKS       EVBDTC
349500031021     C                   WRITE     FNEVB000
349600031021     C                   END
349700031021     C*
349800031021     C                   END
349900031021     C* SE NON ESISTE EVENTO NIC avente la stessa data evento della
350000031021     C* distinta lo scrivo
350100031021     C                   ELSE
350200031021     C                   CLEAR                   FNEVB000
350300031021     C                   MOVEL     'NIC'         EVBCEV
350400031021     C                   move      dateu         EVBDTV
350500031021     C                   TIME                    EVBORV
350600031021     C                   Z-ADD     v1clna        EVBFLE
350700031021     C                   Z-ADD     DUTCOU        EVBCDU
350800031021     C                   Z-ADD     KAAS          EVBAAS
350900031021     C                   Z-ADD     KLNP          EVBLNP
351000031021     C                   Z-ADD     KNRS          EVBNRS
351100031021     C                   Z-ADD     KNSP          EVBNSP
351200031021     C                   Z-ADD     FVVDFV        EVBDEV
351300031021     C     FVVFPP        IFEQ      'M'
351400031021     C                   MOVEL     0800          EVBHEV
351500031021     C                   ELSE
351600031021     C                   MOVEL     1200          EVBHEV
351700031021     C                   END
351800131104     c                   clear                   Devb01
351900131104     C                   move      *zeros        §NOTNDC
352000131104     C                   move      *zeros        §NOTPDC
352100131031     C                   move      ARBNDC        §NOTNDC
352200131031     C                   move      ARBPDC        §NOTPDC
352300131031     C                   MOVEL     Devb01        EVBNOT
352400031021     C                   MOVEL     *BLANKS       EVBDTC
352500031021     C                   WRITE     FNEVB000
352600031021     C*
352700031021     C                   END
352800031021     C* Se MIC non è stato trasmesso annullo fisicamente record
352900031021     C                   ELSE
353000031021     C* annullo fisicamente l'evento se da trasmettere
353100031021     C  N33              DELETE    FNEVB04L
353200031021     C                   END
353300031021     C                   END
353400980311     C                   END                                                    Data dist.=
353500980309     C*
353600980309     C                   ENDSR
353700911105     C*-----------------------------------------------------*
353800080319     C* controllo presenza orm e se dati scaricati a PDA         *
353900911105     C*-----------------------------------------------------*
354000080320     C     srfasi        BEGSR
354100080318     C                   CLEAR                   WORM
354200080318     C                   Z-ADD     wnfv          FVVNF6
354300090211     C     KORM          SETLL     FNORM06L                               34
354400090211     C   34              MOVEL     'S'           WORM              1
354500080320     c* se ho la fase PDR (preparato x pda ORM)
354600080320     c* non faccio manutenzionare il flag matt/pom
354700090211     C                   CLEAR                   faseorm           3
354800080320     c                   eval      dsftrd = 'PDR'
354900080326     C     KDST4         chain     fidsf02l
355000080326     c                   if        %found(fidsf02l)
355100080320     c                   eval      faseorm = dsftrd
355200080320     c                   seton                                        82
355300080326     c                   end
355400080319     c* controllo se i flag per PDA e mat/pom sono modificabili
355500080320     c                   clear                   fasecon           3
355600080319     c                   setoff                                       7282
355700080319     c* se ho la fase PDC (preparato x pda con) o PDO x orm
355800080319     c* non faccio manutenzionare il flag matt/pom
355900080319     c                   eval      dsftrd = 'PDC'
356000080326     C     KDST4         chain     fidsf02l
356100080326     c                   if        %found(fidsf02l)
356200080320     c                   eval      fasecon = dsftrd
356300080319     c                   seton                                        82
356400080326     c                   end
356500080410     c* se ho la fase CUS (ok da spc) non faccio manutenzionare i flag
356600080410     c* dati a pda e matt/pom sia x con che x orm
356700080410     c                   eval      dsftrd = 'CUS'
356800080410     C     KDST4         chain     fidsf02l
356900080410     c                   if        %found(fidsf02l)
357000080410     c* chiamo il pgm per richiesta forzatura
357100080410     c                   exsr      srforza
357200130308     c*                  eval      fasecon = dsftrd
357300110224     c*                  seton                                        7282
357400080410     c                   end
357500080318     C                   ENDSR
357600080318     C*-----------------------------------------------------*
357700080410     C* pgm per forzare modifica distinta fase CUS          *
357800080318     C*-----------------------------------------------------*
357900080410     C     srforza       BEGSR
358000911105     C*
358100080410     c                   clear                   fidg44ds
358200080410     c                   eval      co44fgs=fvvfgs
358300080410     c                   eval      co44ndc=wnfv
358400080410     c                   eval      co44wnd = 'S'
358500080410     c                   eval      co44td = 'E'
358600080410     c* se ho la fase CUS e non la fase PDC vuol dire che è una distinta di
358700080410     c* di soli orm. Imposto O nel tipo distinta per evitare controlli
358800080410     c* inerenti alle consegne
358900080410     c                   if        fasecon = ' ' and worm = 'S'
359000080410     c                   eval      co44td = 'O'
359100080410     c                   end
359200080410     c                   movel(p)  fidg44ds      kpjbu
359300080410     c                   call      'FIDG44R'
359400080410     c                   parm                    kpjba
359500080410     c                   movel     kpjbu         fidg44ds
359600080410     c* se mi ritorna che non ha forzato accendo indicatore d'errore
359700080410     c                   if        co44fcus = 'N'
359800080410     c                   seton                                        90
359900080410     c                   end
360000080410     C*
360100080410     C                   ENDSR
360200080410     C*-----------------------------------------------------*
360300080410     C* ANNULLAMENTO DISTINTA                               *
360400080410     C*-----------------------------------------------------*
360500080410     C     ANNUL         BEGSR
360600080410     C*
360700911105     C* CONTROLLO SE SONO IN IMMMISSIONE O VARIAZIONE DISTINTA
360800911105     C*
360900000717     C                   SETOFF                                       78
361000000717     C                   Z-ADD     V01NDC        FVVNF6
361100070118     C     KFIBRV        SETLL     FNBRV01L                               78
361200000717     C     *IN78         IFEQ      '1'
361300000717     C                   SETON                                        839075
361400000717     C                   MOVEL     ERR(21)       $MSG
361500000717     C                   ELSE
361600941107     C     V01NDC        IFEQ      *ZEROS
361700940322     C                   SETON                                        7590
361800940322     C                   MOVEL     ERR(4)        $MSG
361900911105     C                   ELSE
362000010403     C* WORM = "S"  --->  ESISTONO O.R.M. IN FASE < 900
362100010403    3C     WORM          IFEQ      'S'
362200010403     C                   SETON                                        789075
362300010403     C                   MOVEL     ERR(22)       $MSG
362400010403   X3C                   ELSE
362500020905     C                   Z-ADD     V1CLNA        KFGS
362600020905     C                   Z-ADD     V01NDC        KNDC
362700960702     C                   Z-ADD     ROVDDC        KDDC
362800020905     C     KARB          SETLL     FNARB70L
362900911105     C*
363000911105     C                   DO        *HIVAL
363100911105     C*
363200080318     C     KARB          READE     FNARB70L
363300080318     C                   if        %eof(fnarb70l)
363400080318     c                   leave
363500080318     c                   end
363600941109     C     KARB1         CHAIN     FNARB01L                           31
363700980309     C* Se era in consegna e adesso non c'è più annullo la messa
363800980309     C* consegna
363900980309     C                   EXSR      ANNMIC
364000980309     C*
364100950602     C                   Z-ADD     *ZEROS        ARBNSS
364200020905     C                   Z-ADD     *ZEROS        ARBIFP
364300020905     C                   Z-ADD     *ZEROS        ARBNDC
364400911108     C                   Z-ADD     *ZEROS        ARBDDC
364500911108     C                   Z-ADD     *ZEROS        ARBSTP
364600911108     C                   Z-ADD     *ZEROS        ARBNGD
364700941108     C                   Z-ADD     *ZEROS        ARBPDC
364800941109     C                   UPDATE    FNARB000
364900030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
365000030910     c* deve andare a togliere la distinta dall'orm
365100030910     c                   exsr      srorm
365200070319     c* tolgo i riferimenti della distinta anche su FIARG x
365300070319     c* elaborare eventualmente la spedizione nelle distinte automatiche
365400070319     c                   exsr      srfiarg
365500080410     c* richiamo il pgm per aggiornare i dati per PDA
365600080410     c                   if        §OGPDAcon <> ' ' and
365700080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
365800080411     c                             v1ctstpda = 'C' or
365900080411     c                             v1ctstpda = 'E')
366000080410     c                   clear                   fidg42ds
366100080410     c                   eval      co42tla = 'A'
366200080410     c                   exsr      srpda
366300080410     c                   end
366400930624     C* ELIMINO EVENTUALI ANOMALIE RELATIVE ALLA BOLLA
366500931214     C  N31              EXSR      ANOM
366600911105     C*
366700160429     C     KARB1         CHAIN     FNARBW2L
366800160429     c                   if        %found(fnarbw2l)
366900160429     C                   DELETE    FNARBW2L
367000160429     c                   end
367100080318     C                   ENDdo
367200911105     C*
367300020905     C                   Z-ADD     V1CLNA        FVVFGS
367400020905     C     KFVV          CHAIN     FNFVV01L                           83
367500920914     C  N83              MOVEL     '*'           FVVATB
367600970401     C  N83              MOVEL     *BLANKS       FVVFCF
367700941109     C  N83              UPDATE    FNFVV000
367800000515     C* FOGLIO 8
367900000717     C  N83              Z-ADD     8             FVVNPG
368000020905     C  N83KFVV          CHAIN     FNFVV01L                           83
368100000515     C  N83              MOVEL     '*'           FVVATB
368200000515     C  N83              MOVEL     *BLANKS       FVVFCF
368300000515     C  N83              UPDATE    FNFVV000
368400000515     C                   Z-ADD     4             FVVNPG
368500950601     C*
368600120718     c* aggiorno il file FIDST00F
368700120718     C  N83KDST2         CHAIN     FiDST09L                           83
368800120807     C  N83              MOVEL     'A'           £DSTATB
368900120718     C  N83              UPDATE    FiDST000
369000120718     c                   exsr      srfidst
369100010403    3C                   END
369200911105     C                   END
369300000218     C*
369400000717     C                   END
369500000218     C* REIMPOSTO LA DATA DISTINTA CON LA DATA DEL GIORNO
369600000218     C                   MOVE      WHHDAT        V1CDDC
369700911105     C*
369800911105     C                   ENDSR
369900930623     C*-----------------------------------------------------*
370000930623     C* GESTIONE ANOMALIE DELLA BOLLA                       *
370100930623     C*-----------------------------------------------------*
370200930623     C     ANOM          BEGSR
370300930623     C*
370400960415     C                   CLEAR                   DSLR33
370500960415     C                   MOVE      4             D33FSC
370600960415     C                   MOVE      ROVDDC        D33DCH
370700960415     C*
370800990519     C                   MOVE      ARBAAS        WAA4              4 0
370900950915     C                   Z-ADD     135           §§§CAA
371000990519     C     KANM          SETLL     FNANM01L                           34
371100930624     C     *IN34         DOWEQ     '0'
371200990519     C     KANM          READE     FNANM01L                               34
371300960415     C     *IN34         IFEQ      *OFF
371400960415     C                   Z-ADD     ANMNR1        D33NRR
371500960415     C                   CALL      'FNLR33R'
371600960415     C                   PARM                    DSLR33
371700960415     C                   MOVE      '1'           W33R              1
371800960415     C                   END
371900960415     C                   ENDDO
372000950918     C*
372100950915     C                   Z-ADD     150           §§§CAA
372200990519     C     KANM          SETLL     FNANM01L                           34
372300950915     C     *IN34         DOWEQ     '0'
372400990519     C     KANM          READE     FNANM01L                               34
372500960415     C     *IN34         IFEQ      *OFF
372600960415     C                   Z-ADD     ANMNR1        D33NRR
372700960415     C                   CALL      'FNLR33R'
372800960415     C                   PARM                    DSLR33
372900960415     C                   MOVE      '1'           W33R              1
373000960415     C                   END
373100960415     C                   ENDDO
373200930623     C*
373300930623     C                   ENDSR
373400940322     C*-----------------------------------------------------*
373500940322     C* PULIZIA RICHIESTA PRECEDENTE                        *
373600940322     C*-----------------------------------------------------*
373700940322     C     PULRIC        BEGSR
373800911106     C*
373900911106     C* PULISCO  IL FILE DI LAVORO
374000911106     C*
374100941109     C     *LOVAL        SETLL     FNARBW1L
374200911106     C*
374300911106     C                   DO        *HIVAL                                       4
374400941109     C                   READ      FNARBW1L                               55
374500911106     C*
374600941109     C  N55              DELETE    FNARBW1L
374700911106     C  N55              END
374800911106     C*
374900950907     C     D50TPR        IFEQ      ' '
375000920423     C                   MOVEL     *BLANK        SAVCPC
375100941107     C                   MOVEL     *BLANK        V1CCPC
375200941107     C                   MOVE      V01NDC        V1CNDP
375300941107     C                   MOVEL     *BLANK        V1CNDC
375400950601     C                   MOVEL     *BLANK        V1CPEP
375500050524     C                   MOVEL     *blanks       V1Ctpm
375600160429     C                   MOVEL     *blanks       V1Ctur
375700080121     c                   clear                   v1cpda
375800080208     c                   clear                   v1ctstpda
375900080208     c                   clear                   msgv
376000080318     c                   clear                   msgv1
376100080415     c                   setoff                                       45
376200950608     C                   MOVE      WHHDAT        V1CDDC
376300170208     c                   clear                   hPDRAutista
376400950907     C                   END
376500911106     C*
376600911106     C                   ENDSR
376700940322     C*-----------------------------------------------------*
376800940322     C* Gestione eventuale attribuzione Stop                *
376900940322     C*-----------------------------------------------------*
377000940322     C     GESSTP        BEGSR
377100951121     C*
377200951207     C                   Z-ADD     0             CODCLI
377300951121     C                   MOVEL     *BLANKS       RAGCLI
377400951207     C*  Controllo se devo attribuire lo Stop
377500951207     C                   Z-ADD     1             X                 2 0
377600951207     C                   MOVEL     *BLANKS       V3CCBO
377700951207     C                   MOVEL     *BLANKS       WTBOL1
377800951207     C     ARBCBO        LOOKUP    C3A(X)                                 13
377900951207     C     *IN13         IFEQ      '1'
378000951207     C                   MOVEL     D3A(X)        DS3A
378100951207     C                   MOVEL     §3ATB1        WTBOL1
378200990920     C                   MOVEL     §3ATB2        WTBOL2
378300990920     C                   END
378400990920     C* Se la prima bolla è un assegnato eseguo posizionamento
378500990920     C* su FIAR6 con flag tipo bolla impostato a '1' .....
378600990920     C     WTBOL1        IFEQ      'A'
378700990920     C                   MOVEL     '1'           KTRC
378800990920     C                   Z-ADD     ARBAAS        KAAS
378900990920     C                   Z-ADD     ARBAAS        KLNP
379000990920     C                   Z-ADD     ARBAAS        KNRS
379100990920     C                   Z-ADD     ARBAAS        KNSP
379200990920     C     KAR6          CHAIN     FIAR601L                           74
379300990920     C* ... se non trovo bolla o il codice cliente non è
379400990920     C* impostato reperisco il codice da FNARB
379500990920     C     *IN74         IFEQ      '1'
379600990920     C     AR6KSC        OREQ      0
379700990920     C                   Z-ADD     ARBKSC        CODCLI
379800990920     C                   ELSE
379900990920     C* ... altrimenti recupero codice cliente da FIAR6
380000990920     C                   Z-ADD     AR6KSC        CODCLI
380100990920     C                   END
380200990920     C* .... se prima bolla è un porto franco ....
380300990920     C                   ELSE
380400990920     C* .... verifico se la seconda è un assegnato
380500990920     C     WTBOL2        IFEQ      'A'
380600990920     C                   MOVEL     '2'           KTRC
380700990920     C                   Z-ADD     ARBAAS        KAAS
380800990920     C                   Z-ADD     ARBAAS        KLNP
380900990920     C                   Z-ADD     ARBAAS        KNRS
381000990920     C                   Z-ADD     ARBAAS        KNSP
381100990920     C     KAR6          CHAIN     FIAR601L                           74
381200990920     C* ... e reperisco il codice bolla da lì
381300990920     C     *IN74         IFEQ      '0'
381400990920     C     AR6KSC        ANDNE     0
381500990920     C                   Z-ADD     AR6KSC        CODCLI
381600990920     C                   END
381700990920     C                   END
381800990920     C*
381900990920     C                   END
382000951207     C*
382100951207     C*  Controllo se cliente no generico e se si attribuisco stop
382200951207     C                   Z-ADD     CODCLI        APPOG             4 0
382300951207     C     CODCLI        IFNE      0
382400951207     C     APPOG         ANDNE     9999
382500951207     C     APPOG         ANDNE     8888
382600951211     C                   MOVEL     *BLANKS       RAGCLI
382700951211     C                   MOVEL     CODCLI        RAGCLI
382800951207     C                   ELSE
382900940322     C*  Controllo se devo attribuire lo Stop
383000951121     C                   MOVEL     ARBRSD        RAGCLI
383100951121     C                   MOVE      ARBIND        RAGCLI
383200951207     C                   END
383300940322     C*  Controllo se cliente esiste già
383400940322     C                   Z-ADD     1             Y                 3 0
383500951121     C     RAGCLI        LOOKUP    RSD2(Y)                                13
383600940322     C     *IN13         IFEQ      '1'
383700940322     C                   Z-ADD     STP2(Y)       ARBSTP
383800940322     C                   ELSE
383900961217     C     W             IFLT      999
384000940323     C                   ADD       1             ULTSTP            3 0
384100940322     C                   ADD       1             W
384200940323     C                   Z-ADD     ULTSTP        ARBSTP
384300940322     C                   Z-ADD     ULTSTP        STP2(W)
384400951121     C                   MOVEL     RAGCLI        RSD2(W)
384500940322     C                   END
384600951123     C                   END
384700940322     C*
384800940322     C*
384900940322     C                   ENDSR
385000940322     C*-----------------------------------------------------*
385100940322     C* Controllo gli stop                                  *
385200940322     C*-----------------------------------------------------*
385300940322     C     CTRSTP        BEGSR
385400940322     C*
385500940322     C                   SETOFF                                       16
385600941108     C     V3CPST        IFNE      V3CSTP
385700941108     C     V3CSTP        ANDNE     0
385800941108     C     V3CSTP        IFGT      ULTSTP
385900941108     C                   Z-ADD     V3CSTP        ULTSTP
386000940323     C                   END
386100951207     C                   MOVE      V3CCLI        APPOG
386200951207     C*
386300951207     C     V3CTB1        IFEQ      'A'
386400951207     C     APPOG         ANDNE     8888
386500951207     C     APPOG         ANDNE     9999
386600951211     C                   MOVEL     *BLANKS       RAGCLI
386700951211     C                   MOVEL     V3CCLI        RAGCLI
386800951207     C                   Z-ADD     1             X
386900951207     C     V3CSTP        LOOKUP    STP2(X)                                13
387000951207     C     *IN13         IFEQ      '1'
387100951207     C     RSD2(X)       ANDNE     RAGCLI
387200951207     C     RSD2(X)       ANDNE     *BLANKS
387300951207     C                   Z-ADD     NRR           NRRKEY
387400951207     C                   SETON                                        7516
387500951207     C                   MOVEL     ERR(5)        $MSG
387600951207     C                   ELSE
387700951207     C                   MOVEL     V3CSTP        V3CPST
387800951207     C     *IN13         IFEQ      '0'
387900951207     C                   ADD       1             W
388000951207     C                   Z-ADD     V3CSTP        STP2(W)
388100951207     C                   MOVEL     RAGCLI        RSD2(W)
388200951207     C                   END                                                    NEW STOP
388300951207     C                   END                                                    STOP GIA' ESIST.
388400951207     C                   ELSE                                                   ASSEGNATO COD.
388500940330     C*
388600940322     C                   Z-ADD     1             X
388700941108     C     V3CSTP        LOOKUP    STP2(X)                                13
388800940322     C*
388900940322     C     *IN13         IFEQ      '1'
389000951121     C     RSD2(X)       ANDNE     V3CRAG
389100951121     C     RSD2(X)       ANDNE     *BLANKS
389200940323     C                   Z-ADD     NRR           NRRKEY
389300940322     C                   SETON                                        7516
389400940322     C                   MOVEL     ERR(5)        $MSG
389500940322     C                   ELSE
389600941108     C                   MOVEL     V3CSTP        V3CPST
389700940322     C     *IN13         IFEQ      '0'
389800961217     C     W             ANDLT     999
389900940322     C                   ADD       1             W
390000951121     C                   MOVEL     V3CRAG        RAGCLI
390100941108     C                   Z-ADD     V3CSTP        STP2(W)
390200951121     C                   MOVEL     RAGCLI        RSD2(W)
390300951207     C                   END                                                    NEW STOP
390400951207     C                   END                                                    ESISTE GIA'
390500951207     C*
390600951207     C                   END                                                    ASSEGN. COD.
390700940322     C*
390800951123     C                   END
390900951123     C*
391000940322     C                   ENDSR
391100030910     C*-----------------------------------------------------*
391200070319     C*  toglie o mette distinta sul FIARG
391300030910     C*-----------------------------------------------------*
391400070319     C     srfiarg       BEGSR
391500070322     c                   if        comdda <= dateu and comdda <> 0
391600070319     c     karb1         chain(e)  fiarg01l
391700070531     c                   if        not %error and %found(fiarg01l)
391800070531     c                             and arbndc = 0
391900070531     C                   eval      argfgs = ARBIFP
392000070531     C                   eval      argNDC = ARBNDC
392100070531     C                   eval      argddc = ARBDDC
392200070531     C                   eval      argstp = ARBSTP
392300070531     C                   eval      argpdc = ARBPDC
392400070531     c                   clear                   argnftl
392500070531     c                   clear                   ARGDTVDIS
392600070531     c                   clear                   ARGHVDIS
392700070531     c                   eval      argslb = 0
392800100203     c                   movel     argflo        dargflo
392900100203     c                   clear                   §argattesa
393000100203     c                   movel     dargflo       argflo
393100070531     c                   update    fiarg000
393200070531     c                   end
393300070410     c                   if        not %found(fiarg01l) and arbndc <> 0
393400070509     c                             and not %Error
393500070410     c                   clear                   fiargds
393600070410     c                   z-add     arbaas        argaas
393700070410     c                   z-add     arblnp        arglnp
393800070410     c                   z-add     arbnrs        argnrs
393900070410     c                   z-add     arbnsp        argnsp
394000070410     c                   z-add     arblna        arglna
394100070410     c                   z-add     arbifp        argpoc
394200070508     c******* Questo bloccava Tellus nell'attribuzione dei GIRI
394300070508     c******* quindi è stato tolto.
394400070508     c*******            eval      ARGTGI = 'M'
394500070410     c                   write     fiarg000
394600070319     c                   end
394700080520     c                   unlock    fiarg01l
394800070410     c                   end
394900080410     c                   endsr
395000080410     C*----------------------------------------------------*
395100080410     C* se pda e tolgo o metto spedizione e fase PDC richiamo pgm per
395200080410     c* aggiornare i dati al PDA
395300080410     C*----------------------------------------------------*
395400080410     C     srpda         BEGSR
395500080410     c                   eval      co42FGS = v1clna
395600080410     C                   eval      co42NDC = v01ndc
395700080410     c                   eval      co42ddc = rovddc
395800080911     c                   if        co42tla = 'C'
395900080911     c                   clear                   co42aas
396000080911     c                   clear                   co42lnp
396100080911     c                   clear                   co42nrs
396200080911     c                   clear                   co42nsp
396300080911     c                   else
396400080410     c                   eval      co42aas = arbaas
396500080410     c                   eval      co42lnp  = arblnp
396600080410     c                   eval      co42nrs  = arbnrs
396700080410     c                   eval      co42nsp  = arbnsp
396800080911     c                   end
396900080410     c                   movel(p)  fidg42ds      kpjbu
397000080410     c                   call      'FIDG42R'
397100080410     c                   parm                    kpjba
397200080410     c                   parm                    fnarbds
397300080410     c* ritorno errore ???
397400080410     C                   ENDSR
397500070319     C*-----------------------------------------------------*
397600070319     C*  toglie o mette distinta sull'orm x particolarità RC
397700070319     C*-----------------------------------------------------*
397800070319     C     srorm         BEGSR
397900100514     C     arbgma        lookup    s7rc                                   84
398000100514     c                   if        *in84
398100030910     c                   clear                   fnlr99ds
398200030910     c                   eval      i99tla = 'L'
398300030910     c                   eval      i99aas = arbaas
398400030910     c                   eval      i99lnp = arblnp
398500030910     c                   eval      i99nrs = arbnrs
398600030910     c                   eval      i99nsp = arbnsp
398700030910     c                   eval      i99ndc = arbndc
398800030910     c                   eval      i99ddc = arbddc
398900071001     c                   eval      i99fgs = v1clna
399000030910     c* toglie
399100030910     c                   if        arbndc = 0
399200030910     c                   eval      i99fao = 390
399300030910     c                   else
399400030910     c* mette
399500030910     c                   eval      i99fao = 400
399600030910     c                   eval      i99npg = 4
399700071002     c                   eval      i99nftl= 99999
399800030910     c                   end
399900030910     c                   call      'FNLR99R'
400000030910     c                   parm                    kpjba
400100030910     c                   parm                    fnlr99ds
400200030910     c                   end
400300030910     C                   ENDSR
400400070612      *---------------------------------------------------------*
400500070612      *  Determinazione del volume da utilizzare
400600070612      *---------------------------------------------------------*
400700070612     C     CALVOL        BEGSR
400800070612     c* t=volume da fatturare
400900070612     C     ARBFVF        IFEQ      'T'
401000070612     C                   Z-ADD     ARBVLF        WVOL
401100070612     C                   ELSE
401200070612     C     ARBNCR        ifeq      ARBNCL
401300070612     C                   Z-ADD     ARBVLC        WVOL
401400070612     C                   ELSE
401500070612     C     ARBVLC        IFGE      ARBVLF
401600070612     C                   Z-ADD     ARBVLC        WVOL
401700070612     C                   ELSE
401800070612     C                   Z-ADD     ARBVLF        WVOL
401900070612     C                   ENDIF
402000070612     C                   ENDIF
402100070612     C                   ENDIF
402200070612     C*
402300070612     C                   ENDSR
402400070612      *---------------------------------------------------------*
402500070612      *  Determinazione del peso da utilizzare
402600070612      *---------------------------------------------------------*
402700070612     C     calPES        BEGSR
402800070612     C     arbncp        ifeq      arbncl
402900070612     C                   Z-ADD     arbPKC        wpes
403000070612     C                   ELSE
403100070612     C     arbPKC        ifge      arbPKF
403200070612     C                   Z-ADD     arbPKC        wpes
403300070612     C                   else
403400070612     C                   Z-ADD     arbPKF        wpes
403500070612     C                   end
403600070612     C                   ENDIF
403700070612     C                   ENDSR
403800141013      *_______________________________________________________________*
403900141013     c     verdispo      begsr
404000141013      *_______________________________________________________________*
404100141013     c                   eval      ilry09tla = 'L'
404200141029     c                   eval      ILRY09TDIS = 'E'
404300141029     c                   movel     fnlry09ds2    kpjbu
404400141013     c                   call      'FNLRY09R'
404500141013     c                   parm                    kpjba
404600141029     c                   movel     kpjbu         fnlry09ds2
404700141013     c                   endsr
404800160510     C*--------------------------------------------------------------*
404900160510     C*  parametri ultimo lancio assegnazione automatica
405000160510     C*--------------------------------------------------------------*
405100160510     C     srprm         BEGSR
405200160510     c                   movel     'FIDG17R'     pgm
405300160510     c     kprm          chain     fiprm01l
405400160510     c                   if        %found(fiprm01l) and prmdta = oggi
405500160510     c                   movel     prmpar        fidg17ds
405600160510     c                   else
405700160510     c                   clear                   fidg17ds
405800160510     c                   eval      d17tur1 = 'U'
405900160510     c                   end
406000160510     C                   ENDSR
406100940322     C*-----------------------------------------------------*
406200941107     C*    Operazioni iniziali
406300941107     C*-----------------------------------------------------*
406400940322     C     *INZSR        BEGSR
406500950907     C*
406600940322     C     *ENTRY        PLIST
406700940322     C                   PARM                    KPJBA
406800950907     C                   MOVEL     KPJBU         FNLR50
406900030701     c                   z-add     1             tblkut
407000020905     c*
407100020905     c     *dtaara       define    §azute        azuteds
407200020905     c     *dtaara       define    §datiute      ddatiute
407300020905     C                   in(E)     *dtaara
407400020905     C                   IF        %Error  or  RSUT = *blanks
407500020905     C                   call      'TIBS34R'
407600020905     C                   parm                    Tibs34Ds
407700020905     C                   in        *dtaara
407800020905     c                   ENDIF
407900941107     C*
408000941107     C* Inverto la data del giorno
408100941107     C                   TIME                    WHHDAT           14 0
408200941107     C                   MOVE      WHHDAT        G02DAT
408300940322     C                   MOVE      *BLANKS       G02ERR
408400941107     C                   CALL      'XSRDA8'
408500941107     C                   PARM                    WLBDA8
408600941107     C                   Z-ADD     G02INV        DATEU             8 0
408700160510     C                   movel     DATEU         oggi
408800020905     C*
408900990920     C*  Aggancio tabella 'GED' x reperire la divisa corrente
409000990920     C                   MOVEL     'GED'         KCOD1
409100990920     C                   MOVEL     *BLANKS       KKEY1
409200990920     C                   MOVEL     '1'           KKEY1
409300990920     C     KTBE          CHAIN     TNTBE01L                           13
409400990920     C     *IN13         IFEQ      '0'
409500990920     C                   MOVEL     TBEUNI        DGED
409600990920     C                   END
409700160316     C*  Aggancio tabella 'NSD' informazioni vedi pacco dpd
409800160316     C                   MOVEL     'NSD'         KCOD1
409900160316     C                   MOVEL     *BLANKS       KKEY1
410000160316     C                   MOVEL     '1'           KKEY1
410100160316     C     KTBE          CHAIN     TNTBE01L                           13
410200160316     C     *IN13         IFEQ      '0'
410300160316     C                   MOVEL     TBEUNI        DNSD
410400160316     C                   END
410500000929     C*
410600000929     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
410700000929     C                   MOVEL     'DGD'         KCOD1
410800000929     C                   MOVEL     *BLANKS       KKEY1
410900000929     C                   MOVEL     '1'           KKEY1
411000000929     C     KTBE          CHAIN     TNTBE01L                           13
411100000929     C     *IN13         IFEQ      '0'
411200000929     C                   MOVEL     TBEUNI        DDGD
411300000929     C                   END
411400940322     C*  CARICO DATI TABELLA 3A
411500940323     C                   MOVEL     '3A'          TBLCOD
411600940322     C                   Z-ADD     0             X
411700940322     C     KTAB1         CHAIN     TABEL00F                           13
411800940322     C     *IN13         DOWEQ     '0'
411900941108     C     X             ANDLT     100
412000940322     C                   ADD       1             X
412100940322     C                   MOVEL     TBLKEY        C3A(X)
412200941109     C                   MOVEL     TBLUNI        D3A(X)
412300940323     C     KTAB1         READE     TABEL00F                               13
412400940322     C                   END
412500941110     C*
412600100203     C*  CARICO DATI TABELLA 7r part. consegne attesa
412700100203     C                   MOVEL     '7R'          TBLCOD
412800100203     C                   Z-ADD     0             X
412900100203     C     KTAB1         CHAIN     TABEL00F                           13
413000100203     C     *IN13         DOWEQ     '0'
413100100514     c                   movel     tbluni        ds7r
413200100514     C                   if        x < 100 and
413300100514     c                             §7ratt = 'S'
413400100203     C                   ADD       1             X
413500100203     C                   MOVEL     TBLKEY        s7r(X)
413600100514     c                   end
413700100517     C                   if        xx < 100 and
413800100514     c                             §7RRC  = 'S'
413900100514     C                   ADD       1             Xx
414000100514     C                   MOVEL     TBLKEY        s7rc(Xx)
414100100514     c                   end
414200100203     C     KTAB1         READE     TABEL00F                               13
414300100203     C                   END
414400941110     C*  CARICO DATI TABELLA CV
414500990920     C                   MOVEL     *BLANKS       DIVEUR
414600941110     C                   MOVEL     'CV'          TBLCOD
414700941110     C                   Z-ADD     0             X
414800941110     C     KTAB1         CHAIN     TABEL00F                           13
414900941110     C     *IN13         DOWEQ     '0'
415000941110     C     X             ANDLT     100
415100941110     C                   ADD       1             X
415200990920     C                   MOVEL     TBLKEY        DIVCV             3
415300990920     C                   MOVEL     TBLUNI        DSCV
415400990920     C* Se divisa diversa dalla moneta di conto reperisco data
415500990920     C* validità x trovare altra divisa da utilizzare x incassi
415600011009     C     §GEDCR        IFNE      DIVCV
415700990920     C     DIVEUR        ANDEQ     *BLANKS
415800990920     C     §CVDVA        ANDGE     'S'
415900990920     C                   MOVEL     DIVCV         DIVEUR            3
416000990920     C                   END
416100941110     C     KTAB1         READE     TABEL00F                               13
416200941110     C                   END
416300941108     C*
416400941108     C*  CARICO DATI TABELLA 1P
416500941108     C                   MOVEL     '1P'          TBLCOD
416600941108     C                   Z-ADD     0             X
416700941108     C     KTAB1         CHAIN     TABEL00F                           13
416800941108     C     *IN13         DOWEQ     '0'
416900941108     C     X             ANDLT     50
417000941108     C                   ADD       1             X
417100941108     C                   MOVEL     TBLUNI        DS1P
417200941108     C                   MOVEL     TBLKEY        C1P(X)
417300941108     C                   MOVEL     §1PDES        D1P(X)
417400941108     C     KTAB1         READE     TABEL00F                               13
417500941108     C                   END
417600940322     C*
417700941107     C                   MOVEL     'FNLR50R'     VIDPGM
417800940322     C                   SETON                                        2003
417900950907      *  Controllo se pgm richiamato da gestione fogli distinta
418000950907      *  consegna
418100950907     C                   SETOFF                                       02
418200950907     C     D50TPR        IFEQ      'C'
418300950907     C                   SETON                                        02
418400950908     C                   Z-ADD     D50FIL        V1CLNA
418500950907     C                   MOVE      D50NDT        V1CNDC
418600950907     C                   Z-ADD     D50NDT        V01NDC
418700020905     C                   Z-ADD     D50FPD        V1CFPD
418800950907     C                   Z-ADD     D50DDT        V1CDDC
418900950907     C                   MOVE      D50CPD        V1CCPC
419000950907     C                   MOVEL     D50FPP        V1CFPP
419100950907     C                   MOVEL     D50PEP        V1CPEP
419200050523     C                   MOVEL     D50tpm        V1Ctpm
419300950908     C                   MOVE      D50FIL        FILIA             3 0
419400950907     C                   ELSE
419500020730      * x As Unico imposta la filiale del P.O.
419600020905     C                   z-add     dutpou        FILIA             3 0
419700020730     C                   SETOFF                                       21
419800020730      * se è il primo livello
419900020905     c                   if        dutlpo = '1'
420000020730     C                   SETON                                        21
420100020730     c                   end
420200020730      *
420300950907     C                   MOVE      WHHDAT        V1CDDC
420400950907     C                   MOVEL     'M'           V1CFPP
420500080121     C                   clear                   V1Cpda
420600080208     C                   clear                   V1Ctstpda
420700080208     C                   clear                   msgv
420800080318     C                   clear                   msgv1
420900080415     c                   setoff                                       45
421000050524     C                   MOVEL     *blanks       V1CTPM
421100160429     C                   MOVEL     *blanks       V1CTur
421200950907     C* VALORIZZO IL CAMPO A VIDEO DI LINEA
421300950907     C                   MOVE      FILIA         V1CLNA
421400950907     C                   MOVE      FILIA         V1CFPD
421500950907     C                   END
421600050520     C* Parcel / messaggerie
421700050520     c                   exsr      srtpm
421800050520     C* inverto la data distinta
421900050520     c                   exsr      srddc
422000020701     C* Aggancio la £6 e recupero le linee a loro associate
422100020701     c                   clear                   trul06ds
422200020701     c                   eval      d06cod = '£6'
422300020701     c                   movel     filia         d06key
422400040831     c                   move      'S'           d06key
422500040831     C                   Z-ADD     G02INV        d06drf
422600020701     c                   movel     trul06ds      kpjbu
422700020701     c                   call      'TRUL06R'
422800020701     c                   parm                    kpjba
422900020701     c                   movel     kpjbu         trul06ds
423000020701     c                   movea     lint          Lin
423100020701     c
423200020701     C* Aggancio la £6 e recupero le linee a loro associate
423300020701     c                   clear                   trul06ds
423400020701     c                   eval      d06cod = '£6'
423500020701     c                   movel     filia         d06key
423600020701     c                   move      'G'           d06esc
423700040831     c                   move      'S'           d06key
423800040831     C                   Z-ADD     G02INV        d06drf
423900020701     c                   movel     trul06ds      kpjbu
424000020701     c                   call      'TRUL06R'
424100020701     c                   parm                    kpjba
424200020701     c                   movel     kpjbu         trul06ds
424300020701     c                   movea     lint          L£6
424400140926      * verifica presenza disposizioni web
424500141029     c                   clear                   fnlry09ds2
424600140926     c                   eval      ilry09fgs = filia
424700140926     c                   eval      ilry09tch = 'C'
424800141013     c                   exsr      verdispo
424900141029     c                   if        OLRY09ESAL = '1' and
425000140926     c                             OLRY09ERR  = *blank
425100140926     c                   seton                                        97
425200170208      **
425300170208     c                   clear                   hPdrAutista       7 0
425400170208      **
425500140926     c                   endif
425600940322      *---------------------------------------------------------------*
425700940322      * CHIAVI                                                        *
425800940322      *---------------------------------------------------------------*
425900160510     c     kprm          klist
426000160510     c                   kfld                    v1clna
426100160510     c                   kfld                    pgm              10
426200070312     C     Kdst2         KLIST
426300070410     C                   KFLD                    fvvFGS
426400070312     C                   KFLD                    fvvnpg
426500070312     C                   KFLD                    wnfv
426600070312     C     Kdst1         KLIST
426700070312     C                   KFLD                    fvvFGS
426800070312     C                   KFLD                    wnfv
426900080121     C     Kdst3         KLIST
427000080121     C                   KFLD                    fvvfgs
427100080121     C                   KFLD                    knpg
427200080121     C                   KFLD                    wnfv
427300080318     C     Kdst4         KLIST
427400080318     C                   KFLD                    knpg
427500080318     C                   KFLD                    wnfv
427600080318     C                   KFLD                    fvvfgs
427700080319     C                   KFLD                    dsftrd
427800021204     C     Kapd          KLIST
427900021204     C                   KFLD                    apdtip
428000021204     C                   KFLD                    apdpdr
428100021204     c                   movel     'A'           apdtip
428200070312     c     Kapd21        Klist
428300070312     c                   Kfld                    apdtip
428400120718     c                   Kfld                    £dstpdr
428500070312     c                   Kfld                    fvvfpp
428600070312     c                   Kfld                    fvvdfv
428700070312     c     Kapd2         Klist
428800070312     c                   Kfld                    apdtip
428900120718     c                   Kfld                    £dstpdr
429000070312     c                   Kfld                    fvvfpp
429100940322     C     KTAB          KLIST
429200020906     C                   KFLD                    tblkut
429300940322     C                   KFLD                    TBLCOD
429400940322     C                   KFLD                    TBLKEY
429500940322     C     KTAB1         KLIST
429600020906     C                   KFLD                    tblkut
429700940322     C                   KFLD                    TBLCOD
429800990920     C     KTBE          KLIST
429900990920     C                   KFLD                    KCOD1
430000990920     C                   KFLD                    KKEY1
430100951211     C     KNUF          KLIST
430200941107     C                   KFLD                    AAA               2 0
430300940322     C                   KFLD                    CNU               3 0
430400951211     C     KNUF0         KLIST
430500951211     C                   KFLD                    AAA0              2 0
430600951211     C                   KFLD                    CNU               3 0
430700951211     C                   Z-ADD     12            CNU
430800941107     C                   MOVE      WHHDAT        AAA
430900951211     C                   Z-ADD     0             AAA0
431000940322     C     KARB1         KLIST
431100940322     C                   KFLD                    ARBAAS
431200940322     C                   KFLD                    ARBLNP
431300940322     C                   KFLD                    ARBNRS
431400940322     C                   KFLD                    ARBNSP
431500960702     C     KARB          KLIST
431600020906     C                   KFLD                    Kfgs
431700020905     C                   KFLD                    KNDC
431800960702     C                   KFLD                    KDDC
431900940322     C     KARB2         KLIST
432000940322     C                   KFLD                    LBLAAN
432100940322     C                   KFLD                    LBLLPN
432200940322     C                   KFLD                    LBLNRN
432300940322     C                   KFLD                    LBLNSN
432400940322     C     KFVV          KLIST
432500940322     C                   KFLD                    FVVNPG
432600940322     C                   KFLD                    FVVNFV
432700020905     C                   KFLD                    FVVFGS
432800950601     C     KDST          KLIST
432900950601     C                   KFLD                    FVVNFV
433000950601     C                   KFLD                    FVVFGS
433100000717     C     KFIBRV        KLIST
433200000717     C                   KFLD                    FVVNPG
433300000717     C                   KFLD                    FVVNF6            6 0
433400030226     C                   KFLD                    FVVfgs
433500980309     C     KEVB          KLIST
433600980309     C                   KFLD                    KAAS
433700980309     C                   KFLD                    KLNP
433800980309     C                   KFLD                    KNRS
433900980309     C                   KFLD                    KNSP
434000980309     C                   KFLD                    KCEV
434100990920     C     KAR6          KLIST
434200990920     C                   KFLD                    KAAS
434300990920     C                   KFLD                    KLNP
434400990920     C                   KFLD                    KNRS
434500990920     C                   KFLD                    KNSP
434600990920     C                   KFLD                    KTRC
434700990920     C     KARI          KLIST
434800990920     C                   KFLD                    KAAS
434900990920     C                   KFLD                    KLNP
435000990920     C                   KFLD                    KNRS
435100990920     C                   KFLD                    KNSP
435200990920     C                   KFLD                    KTIP
435300941229     C*
435400940322     C                   Z-ADD     4             FVVNPG
435500940322     C*
435600940322     C     KLBL          KLIST
435700940322     C                   KFLD                    LBLAAP
435800940322     C                   KFLD                    LBLLPP
435900940322     C                   KFLD                    LBLNRP
436000940322     C                   KFLD                    LBLNSP
436100940322     C*
436200940322     C     KLBL1         KLIST
436300940322     C                   KFLD                    LBLAAN
436400940322     C                   KFLD                    LBLLPN
436500940322     C                   KFLD                    LBLNRN
436600940322     C                   KFLD                    LBLNSN
436700940322     C*
436800940322     C     KANM          KLIST
436900990519     C                   KFLD                    WAA4
437000940322     C                   KFLD                    ARBLNP
437100940322     C                   KFLD                    ARBNRS
437200940322     C                   KFLD                    ARBNSP
437300940322     C                   KFLD                    §§§CAA            3 0
437400940322     C*
437500940322     C     KEYFT2        KLIST
437600940322     C                   KFLD                    FTWPDR            7 0
437700940322     C                   KFLD                    FTWFVL            1
437800941109     C                   KFLD                    FTWDDC            8 0
437900940322     C                   KFLD                    FTWTSR            1
438000950606     C*
438100950606     C     KCLN          KLIST
438200950606     C                   KFLD                    KTFP
438300950606     C                   KFLD                    KTFA
438400950606     C                   KFLD                    KANN
438500950606     C                   KFLD                    KMES
438600010403     C* ACCESSO FNORM06L
438700010403     C     KORM          KLIST
438800010403     C                   KFLD                    FVVFGS
438900010403     C                   KFLD                    FVVNF6
439000031017     C     Kbrv          KLIST
439100031020     C                   KFLD                    kNPG              1 0
439200031017     C                   KFLD                    knf6              6 0
439300031020     C                   KFLD                    kfgs3             3 0
439400031017     c                   KFLD                    artLNP
439500031017     c                   KFLD                    artLNA
439600031017     c                   KFLD                    artNRS
439700031017     c                   kfld                    artNSC
439800031017     C                   Z-ADD     4             kNPG
439900031017     C     Kart          KLIST
440000031017     C                   KFLD                    KAAS
440100031017     C                   KFLD                    KLNP
440200031017     C                   KFLD                    KNRS
440300031017     c                   KFLD                    KNSP
440400040217      *
440500040217     C     Kfiar5        KLIST
440600040217     C                   KFLD                    arbAAS
440700040217     C                   KFLD                    arbLNP
440800040217     C                   KFLD                    arbNRS
440900040217     C                   KFLD                    arbNSP
441000040217     C                   KFLD                    trcar5            3
441100041007     C     kqdt          klist
441200041007     C                   kfld                    wlna              3 0
441300041007     C                   kfld                    wdis              6 0
441400110224     c     Kpct          Klist
441500110225     c                   Kfld                    £dstFGS
441600110225     c                   Kfld                    £dstNFV
441700110225     c                   Kfld                    £dstpdr
441800110324     c                   Kfld                    wtrd              3
441900110324     c                   eval       wtrd  = 'CET'
442000940322     C*
442100000515     C     *LIKE         DEFINE    FVVDS         FVVDS2
442200940322     C     *LIKE         DEFINE    TBLKUT        §KUT
442300940322     C     *LIKE         DEFINE    TBLCOD        §COD
442400940322     C     *LIKE         DEFINE    TBLKEY        §KEY
442500950117     C     *LIKE         DEFINE    VIDANS        SAVAAS
442600950117     C     *LIKE         DEFINE    V03LNP        SAVLNP
442700950117     C     *LIKE         DEFINE    V03NRS        SAVNRS
442800950117     C     *LIKE         DEFINE    V03NSP        SAVNSP
442900950606     C     *LIKE         DEFINE    CLNTFP        KTFP
443000950606     C     *LIKE         DEFINE    CLNTFA        KTFA
443100950606     C     *LIKE         DEFINE    CLNANN        KANN
443200950606     C     *LIKE         DEFINE    CLNMES        KMES
443300020905     C     *LIKE         DEFINE    ARBIFP        KFGS
443400020905     C     *LIKE         DEFINE    ARBNDC        KNDC
443500960702     C     *LIKE         DEFINE    ARBDDC        KDDC
443600980309     C     *LIKE         DEFINE    EVBAAS        KAAS
443700980309     C     *LIKE         DEFINE    EVBLNP        KLNP
443800980309     C     *LIKE         DEFINE    EVBNRS        KNRS
443900980309     C     *LIKE         DEFINE    EVBNSP        KNSP
444000980309     C     *LIKE         DEFINE    EVBCEV        KCEV
444100990920     C     *LIKE         DEFINE    TBECOD        KCOD1
444200990920     C     *LIKE         DEFINE    TBEKE1        KKEY1
444300990920     C     *LIKE         DEFINE    ARITIP        KTIP
444400990920     C     *LIKE         DEFINE    AR6TRC        KTRC
444500950117     C*
444600950123     C                   SETOFF                                       1319
444700940322     C*
444800940322     C                   ENDSR
444900010403** ERR
445000941109Eventuali variazioni andranno perse premendo F12                      01
445100940503Non è possibile annullare distinta in immissione                      02
445200940503Abilitata inclusione o esclusione non entrambi                        03
445300940503Non è posssibile annullare la distinta in immissione                  04
445400940503Stop già utilizzato per un altro cliente                              05
445500940503Linea errata                                                          06
445600940503Data distinta errata                                                  07
445700940503Data distinta errata: già confermati i conteggi                       08
445800940503Codice padroncino inesistente                                         09
445900961115Codice padroncino appartenente a punto operativo non in gestione      10
446000080318Autotrasp. diverso da distinta F1 confermi variazione                 11
446100940503Impossibile gestire distinta attendi 5'' e ritenta                    12
446200940503Distinta errata                                                       13
446300940503Distinta al momento non disponibile: ATTENZIONE !!!                   14
446400961115Numero distinta in gestione ad altra punto operativo                  15
446500000929Data distinta anteriore o superiore alla data odierna dei gg.ammessi  16
446600020906Punto operat. padroncino deve essere = al punto operat. in gestione   17
446700961115Puto operativo di consegna gestita da altro punto operativo           18
446800961217Data distinta non modificabile                                        19
446900961217Numero stop non attribuito correttamente: ci sono più di 999 spediz.  20
447000000717DISTINTA NON ANNULLABILE ESISTONO DELLE SPUNTE SU QUESTO FOGLIO       21
447100010403DISTINTA NON ANNULLABILE: PRESENTI O.R.M.                             22
447200020419Data distinta superiore alla data odierna dei gg.ammessi              23
447300020419Data distinta inferiore alla data odierna inserimento non possibile   24
447400080208Filiale non ancora attiva per gestione PDA                            25
447500110225Scelta incongruente o non ammessa                                     26
447600080318La distinta è in una fase dove non è + possibile modificare il valore 27
447700160502Il turno è ammesso solo per distinte Parcel                           28
447800170224Autista BLOCCATO !!                                                   29
