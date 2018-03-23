000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020528     H* ficn19R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400020528     H*               GESTIONE storico TARIFFE PADRONCINI
000500000000     H*--------------------------------------------------------------*
000600020528     Fficn19D   CF   E             WORKSTN
000700020528     F                                     SFILE(CN19S02:NRR)
000800020528     Ffifgts0f  if   E           K DISK
000900021203     Ffiapd01l  if   E           K DISK
001000140117      ****-?
001100140117     D**** TRUL72DS      E DS
001200140117     D****  L72                           3s 0 DESCEND
001300140117     D****                                     DIM(60)
001400140117     D****                                     OVERLAY(O72Lia)
001500140117      ****-?Caricamento filiali contabili
001600140117     d TRUL31ds      e ds                  inz
001700140117     d $POg            s              3    inz   dim(250)
001800140117      ****-?
001900950113     D* SCHIERE PER CARICAMENTO L1 E L6
002000950113     D L6              S              3  0 DIM(30)                              FIL GESTITE
002100930923     D*
002200940919     D* DS PER TRUL06R - CARICAMENTO £1
002300940919     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002400940919     D  LIN                    1     90  0
002500940919     D                                     DIM(30)                              SKI COMODO
002600940930     D*
002700921102     D WLBDAT          DS
002800940906     D  G02DAT                 1      8  0
002900940906     D  G02INV                 9     16  0
003000940906     D  G02ERR                17     17
003100940906     D  G02TGI                18     22  0
003200940504     D                 DS
003300940906     D  AA                     1      4  0
003400940906     D  MM                     5      6  0
003500940906     D  GG                     7      8  0
003600940906     D  DATA                   1      8  0
003700000000     D KPJBA         E DS
003800940919     D  REM                    1      3
003900940919     D  REMFIL                 4      6
004000900320     D CNCR80        E DS
004100021203     D fnlv24ds      E DS
004200900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004300931115     D  TCU                  398    697
004400931115     D                                     DIM(50)                              TAB FLAG CNT
004500931115     D  KCU                  698    847P 0
004600931115     D                                     DIM(50)                              TAB CAPO CONTO
004700931115     D                                     PACKEVEN
004800931115     D  DTU                  848    895P 0
004900931115     D                                     DIM(12)                              DATE PARAM
005000931115     D                                     PACKEVEN
005100020129     d dataiso         s               d   datfmt(*iso)
005200020129     d dataeur         s               d   datfmt(*eur)
005300941005     D*
005400020130     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)            GG DEI MESI
005500930811     C*****************************************************************
005600930811     C*---------------------------------------------------------------*
005700930928     C* ESEGUO OPERAZIONI INIZIALI
005800930928     C                   EXSR      OPERAZ
005900020528     C                   EXSR      video1
006000930903     C*
006100930820     C                   SETON                                        LR
006200930928     C*
006300930811     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
006400930811     C     VIDEO1        BEGSR
006500011109     C*---------------------------------------------------------------*
006600930930     C*
006700020124     C                   EXSR      PULSF
006800930907     C*
006900930907     C     FOR01         TAG
007000020528     C                   EXFMT     CN19D01
007100011127     C                   SETOFF                                       045386
007200930929     C*
007300930811     C** CMD3  - FINE LAVORO
007400931013     C   KC              GOTO      ENDVD1
007500931013     C*
007600930907     C                   EXSR      CONTR1
007700930907     C   90              GOTO      FOR01
007800020528     c                   exsr      carsf
007900020528     c                   if        nrr > 0
008000020528     c                   do        *hival
008100020528     c                   z-add     1             rec
008200020528     C                   WRITE     CN19Z02
008300020528     C                   EXFMT     CN19C02
008400020528     C   kl              EXSR      PULSF
008500020528     c   kl              goto      for01
008600020528     c                   enddo
008700020528     c                   else
008800020528     c                   seton                                        55
008900020528     c                   goto      for01
009000020528     c                   endif
009100930811     C*
009200930907     C     ENDVD1        ENDSR
009300930811     C*
009400930811     C*--- CONTROLLI DELLA PRIMA VIDEATA -----------------------------*
009500930811     C     CONTR1        BEGSR
009600011109     C*---------------------------------------------------------------*
009700940920     C****  CODICE FILIALE  ****
009800950113     C* FILIALE OBBLIGATORIA
009900020528     c                   setoff                                       835540
010000950113     C     VIDFIL        IFEQ      0
010100950113     C                   SETON                                        8390
010200950113     C                   GOTO      ENDCT1
010300950113     C                   ENDIF
010400950113     C*
010500950113     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
010600030314     c                   if        tipo <> 'C'
010700950113     C     *IN89         IFEQ      *OFF
010800950113     C     VIDFIL        IFNE      SIMFEL
010900090424     C     simpou        andne     046
011000140117     C**** VIDFIL        LOOKUP    L72                                    35
011100140117     C                   move      vidfil        Fil03A            3
011200140117     C     Fil03A        LOOKUP    $POg                                   35
011300940920     C  N35              SETON                                        8390
011400940920     C  N35              GOTO      ENDCT1
011500940920     C                   ENDIF
011600950113     C*
011700950113     C                   ELSE
011800950113     C* 89 ON  - SONO UN "REMXXX" QUINDI CONTROLLO LA FILIALE NELLA L6
011900950113     C                   MOVEL     VIDFIL        WFIL              3
012000950113     C     WFIL          IFNE      REMFIL
012100950113     C     VIDFIL        LOOKUP    L6                                     35
012200950113     C  N35              SETON                                        8390
012300950113     C  N35              GOTO      ENDCT1
012400950113     C                   ENDIF
012500950113     C                   ENDIF
012600030314     c                   endif
012700940920     C*
012800930811     C****  CODICE PADRONCINO  ****
012900930908     C     '?'           SCAN      VIDPDR                                 90
013000930811     C*
013100930811     C     *IN90         IFEQ      *ON
013200021203     C                   Z-ADD     VIDFIL        d24FIL
013300021203     C                   Z-ADD     0             d24pdr
013400021203     C                   MOVEL     'R'           d24FLG
013500030203     C                   MOVEL     tipo          d24tip
013600021203     C                   MOVEL     *BLANKS       d24rsc
013700021203     C                   MOVEL     '$$'          d24ann
013800021203     C                   MOVEL(p)  fnlv24ds      KPJBU
013900940926     C                   CALL      'FNLV24R'
014000930811     C                   PARM                    KPJBA
014100021203     C                   MOVEL     KPJBU         fnlv24ds
014200021203     C                   move      d24pdr        vidpdr
014300021203     C                   MOVEL     d24rsc        despdr
014400020528     C                   end
014500930811     C*
014600930921     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
014700930921     C                   TESTN                   VIDPDR               35
014800930921     C  N35              SETON                                        4090
014900930921     C  N35              GOTO      ENDCT1
015000931111     C*
015100940915     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
015200940920     C                   MOVEL     VIDFIL        COMPDR
015300940920     C                   MOVE      VIDPDR        COMPDR
015400021203     C     kapdc         CHAIN     fiapd000                           32
015500940914     C* CODICE PADRONCINO ERRATO
015600940914     C   32              SETON                                        4090
015700940914     C   32              GOTO      ENDCT1
015800100223     C                   MOVEL     APDRSf        despdr
015900000620     C*
016000930928     C****  CONTROLLO SE ESISTONO TARIFFE PER QUEL PADRONCINO  ****
016100931012     C* IMPOSTO CAMPI DELLA CHIAVE
016200930928     C                   MOVEL     VIDFIL        COMPDR
016300930915     C                   MOVE      VIDPDR        COMPDR
016400020528     C                   MOVEL     ' '           COMSML            1
016500930908     C*
016600930811     C     ENDCT1        ENDSR
016700930811     C*
016800930811     C*
016900930812     C*--- PULIZIA SUBFILE -------------------------------------------*
017000930812     C     PULSF         BEGSR
017100011109     C*---------------------------------------------------------------*
017200930915     C                   SETON                                        87
017300020528     C                   WRITE     CN19C02
017400930915     C                   SETOFF                                       87
017500930812     C                   ENDSR
017600930812     C*
017700930812     C*--- CARICAMENTO SUBFILE ---------------------------------------*
017800930812     C     CARSF         BEGSR
017900011109     C*---------------------------------------------------------------*
018000930812     C                   Z-ADD     0             NRR               4 0
018100930812     C*
018200020528     c     kfgt          setll     fifgts00
018300020528     c                   do        *hival
018400020528     C     kfgt          READE     fifgts00                               31
018500020528     C   31              leave
018600090922     c* verifico il tipo servizio
018700090922     c                   if        tipo = 'A' and
018800090922     c                             (fgttsr ='X' or
018900090922     c                              fgttsr ='P' or
019000090922     c                              fgttsr ='T' )
019100090922     c                   iter
019200090922     c                   end
019300090922     c                   if        tipo = 'C' and
019400090922     c                             (fgttsr ='C' or
019500090922     c                              fgttsr ='R' or
019600090922     c                              fgttsr ='G' )
019700090922     c                   iter
019800090922     c                   end
019900930903     C*
020000930819     C* CODICE E PROGRESSIVO TARIFFA
020100930819     C                   MOVEL     FGTCTR        VI2CTR
020200930819     C                   MOVEL     FGTPRG        VI2PRG
020300011029     C                   MOVEL     FGTdiv        VI2div
020400930819     C*
020500930819     C* GIRO DATA DECORRENZA TARIFFA
020600940906     C                   Z-ADD     FGTDDT        DATA
020700940906     C                   Z-ADD     MM            GGMM              4 0
020800940906     C                   MOVEL     GG            GGMM
020900940906     C                   Z-ADD     AA            VI2DDT
021000940906     C                   MOVEL     GGMM          VI2DDT
021100930819     C* GIRO DATA SCADENZA TARIFFA
021200940906     C                   Z-ADD     FGTDST        DATA
021300940906     C                   Z-ADD     MM            GGMM
021400940906     C                   MOVEL     GG            GGMM
021500940906     C                   Z-ADD     AA            VI2DST
021600940906     C                   MOVEL     GGMM          VI2DST
021700930819     C* GIRO DATA ULTIMA VARIAZIONE
021800090424     c                   if        tipo = 'A'
021900090424     C                   Z-ADD     FGTDCN        DATA
022000090424     C                   Z-ADD     MM            GGMM
022100090424     C                   MOVEL     GG            GGMM
022200090424     C                   Z-ADD     AA            VI2DUV
022300090424     C                   MOVEL     GGMM          VI2DUV
022400090424     c                   else
022500940906     C                   Z-ADD     FGTDUV        DATA
022600940906     C                   Z-ADD     MM            GGMM
022700940906     C                   MOVEL     GG            GGMM
022800940906     C                   Z-ADD     AA            VI2DUV
022900940906     C                   MOVEL     GGMM          VI2DUV
023000090424     c                   end
023100930820     C*
023200930820     C* DESCRIZIONE TARIFFA
023300930819     C                   MOVEL     FGTDCV        VI2DCV
023400020528     c                   select
023500020528     c                   when      fgttsr = 'C'
023600030314     c                   eval      vi2tsr = 'Consegna    '
023700020528     c                   when      fgttsr = 'R'
023800030314     c                   eval      vi2tsr = 'Ritiro      '
023900020528     c                   when      fgttsr = 'G'
024000030314     c                   eval      vi2tsr = 'Giornata    '
024100030314     c                   when      fgttsr = 'X'
024200030314     c                   eval      vi2tsr = 'Mv.Magazzino'
024300030314     c                   when      fgttsr = 'P'
024400030314     c                   eval      vi2tsr = 'Serv.pulizie'
024500030314     c                   when      fgttsr = 'T'
024600030314     c                   eval      vi2tsr = 'M.Casse mob.'
024700020528     c                   endsl
024800930812     C*
024900930812     C                   ADD       1             NRR
025000020528     C                   WRITE     CN19S02
025100020528
025200930812     C                   ENDDO
025300930820     C*
025400930812     C                   ENDSR
025500020125     C*--- OPERAZIONI INIZIALI ---------------------------------------*
025600020125     C     OPERAZ        BEGSR
025700020125     C*---------------------------------------------------------------*
025800020125     C***
025900020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
026000020125     C***
026100020125     C                   TIME                    W0120            14 0
026200020125     C                   MOVE      W0120         WDAT              8 0
026300020125     C*
026400020125     C                   Z-ADD     WDAT          G02DAT
026500020125     C                   MOVEL     *BLANK        G02ERR
026600020125     C                   CALL      'XSRDA8'
026700020125     C                   PARM                    WLBDAT
026800020125     C* UDATE A 8 IN AAAA/MM/GG
026900020125     C                   Z-ADD     G02INV        WUDA
027000020125     C* DATA DEL GIORNO IN GG/MM/AAAA
027100020125     C                   Z-ADD     G02DAT        UDATE8
027200020125     C*
027300020125     C***
027400020125     C* CARICAMENTO TABELLE
027500020125     C***
027600020125     C                   EXSR      CARTAB
027700020125     C*
027800020125     C* PULISCO CODICE PADRONCINO E DESCRIZIONE SOLO A INIZIO PGM
027900020125     C*   IN MODO CHE NELLA PRIMA VIDEATA RESTI IMPOSTATO L'ULTIMO
028000020125     C                   MOVEL     *BLANKS       VIDPDR
028100020125     C                   MOVE      '?'           VIDPDR
028200020528     C                   MOVEL     *BLANKS       despdr
028300020125     C                   ENDSR
028400020125     C*
028500020125     C*--- CARICAMENTO TABELLE ---------------------------------------*
028600020125     C     CARTAB        BEGSR
028700020125     C*---------------------------------------------------------------*
028800020125     C***
028900070307     C* CARICO TABELLA FILIALI contabili
029000020125     C***
029100140117     c***                clear                   trul72ds
029200140117     c***                eval      i72tla = 'L'
029300140117     c***                eval      I72POU = simpou
029400140117     c***                eval      kpjbu = trul72ds
029500140117     C***                CALL      'TRUL72R'
029600140117     C***                PARM                    KPJBA
029700140117     c***                movel     kpjbu         trul72ds
029800140117     C***
029900140117      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
030000140117     c                   clear                   TRUL31DS
030100140117     c                   eval      I31abi = 'AC'
030200140117     c                   eval      I31cpo = simpou
030300140117     c                   call      'TRUL31R'
030400140117     c                   parm                    KPJBA
030500140117     c                   parm                    TRUL31DS
030600140117     c                   if        o31pog > *zeros
030700140117     c                   movea     o31pog        $pog
030800140117     c                   endif
030900020125     C***
031000020125     C* VEDO SE SONO SIMFEL O REMOTO
031100020125     C***
031200020125     C     REM           IFEQ      'REM'
031300020125     C     REMFIL        ANDGT     *ZEROS
031400020125     C                   MOVEL     REMFIL        VIDFIL
031500070307     C                   MOVEL     REMFIL        VIDpog            3 0
031600020125     C                   SETON                                        89
031700020125     C***
031800020125     C* CARICO TABELLA FILIALI GESTITE £6
031900020125     C***
032000020125     C                   CLEAR                   DSUL06
032100020125     C                   MOVE      '£6'          D06COD
032200020125     C                   MOVEL     REMFIL        D06KEY
032300020125     C                   MOVEL     DSUL06        KPJBU
032400020125     C*
032500020125     C                   CALL      'TRUL06R'
032600020125     C                   PARM                    KPJBA
032700020125     C                   MOVEL     KPJBU         DSUL06
032800020125     C                   MOVEA     LIN           L6
032900020125     C*
033000020125     C                   ELSE
033100020125     C* SONO SIMFEL
033200030203     C                   MOVEL     SIMpou        VIDFIL
033300020125     C                   ENDIF
033400020125     C*
033500020125     C                   ENDSR
033600020125     C*
033700020125      *--------------------------------------------------
033800020125     c     *inzsr        begsr
033900020125      *--------------------------------------------------
034000020125
034100020125     C     *ENTRY        PLIST
034200020125     C                   PARM                    KPJBA
034300030203     C                   movel     kpjbu         tipo              1
034400090424     c                   if        tipo = 'A'
034500090424     c                   eval      vidtes= 'Copia Fir.'
034600090424     c                   else
034700090424     c                   eval      vidtes= 'Ult. Var.'
034800090424     c                   end
034900020125     C                   Z-ADD     1             CODUT
035000020125     C                   CALL      'X§PARUT'
035100020125     C                   PARM                    UT§DSE
035200020125     C                   MOVEL     RAGUT         RSUT             20
035300020125     C                   MOVEL     REC80         CNCR80
035400020125     C*
035500020125     C* ACCESSO fifgt
035600020125     C     KFGT          KLIST
035700020125     C                   KFLD                    COMPDR
035800020125     C                   KFLD                    COMSML
035900021203     C* ACCESSO fiapd
036000021203     C     Kapdc         KLIST
036100030203     C                   KFLD                    tipo
036200021203     C                   KFLD                    COMpdr
036300020125     C     *LIKE         DEFINE    FGTDDT        WUDA                           *AAAA/MM/GG
036400020125     C     *LIKE         DEFINE    FGTDDT        UDATE8                         *GG/MM/AAAA
036500020125     C*
036600020125     C     *LIKE         DEFINE    FGTPDR        COMPDR
036700020125     C*
036800020125     c                   endsr
036900020130**   XDTF
037000020130312831303130313130313031
