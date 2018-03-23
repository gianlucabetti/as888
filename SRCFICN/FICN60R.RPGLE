000100990506     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990506     H*PARMS ACTGRP(QILE)
000300030211     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000400020403     H/TITLE  contabilizzazione conteggi
000500951002?     *--------------------------------------------------------------*
000600020510      * FICN60R                                                      *
000700951002      *     *----------------------------------------------*         *
000800020510      *         Gestione TLZFLO: flag per numerazione proria         *
000900020510      *     *----------------------------------------------*         *
001000951002      *                                                              *
001100951002?     *--------------------------------------------------------------*
001200951002      * KC - FINE LAVORO                                             *
001300951002?     *--------------------------------------------------------------*
001400020510     Ftntlz01l  uF a E           K DISK
001500030211     Fanrco98j  IF   E           K DISK
001600020417     Fanfrn01l  IF   E           K DISK
001700020510     Fficn60D   CF   E             WORKSTN
001800951002     D*---------------------------------------------------------------*
001900951002     D* SCHIERA
002000951002     D*---------------------------------------------------------------*
002100060630     D ERR             S             70    DIM(6) CTDATA PERRCD(1)              Errori
002200980910     D* Definizioni parametri chiamata moduli NDMVC104
002300980910    >D Societa         S              3
002400980910    >D Ctb             S              2
002500980910    >D Unita           S              4
002600980910     D TpReg           S              1
002700980911     D DtReg           S             10
002800980910     D DtLGio          S             10
002900980910     D RegIVA          S              1
003000980910     D TpRegIVA        S              1
003100980910     D Stato           S              2
003200980910     D Esito           S              1
003300980910     D Gesmsg          S              1
003400980910?     *--------------------------------------------------------------*
003500980910?     *  DS                                                          *
003600980910?     *--------------------------------------------------------------*
003700980910     D DSFMT           DS
003800980910     D  $TASTO               369    369
003900980910     D  NRG                  370    370
004000980910     D  NCL                  371    371
004100980910     D*-------------
004200980910     D* posizione cursore
004300980910     D CURSOR          DS
004400980910     D  RIRI                   1      2B 0 INZ
004500980910     D  R$$                    2      2
004600980910     D  COCO                   3      4B 0 INZ
004700980910     D  C$$                    4      4
004800980910     D*-------------
004900980910     D* Reperimento nome PGM
005000020510     D                SDS
005100020510     D  nompgm           *PROC
005200951002     D KPJBA         E DS
005300020417     Dkksc             s                   like(v1cpdr)
005400020510     D dtlz01        E DS
005500960215     D*------------------
005600020403     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
005700960215     D*------------------
005800960215     D PARAM           DS                  INZ
005900030211     D* TIPOLOGIA P_=autotrasport./C_=Cooperativa
006000000811     D  PARTIP                 1      2
006100020415     D* RAGIONE SOCIALE aoutotras.
006200000811     D  PARRSC                 3     37
006300020415     D* S.I.
006400000811     D  PARCSF                38     40
006500020415     D* CODICE
006600000811     D  PARPDR                41     47  0
006700020415     D* PARFLG = "3" --> NON ESISTONO autotras. PER LA CHIAVE
006800000811     D*                  ALFABETICA RICHIESTA
006900000811     D  PARFLG                48     48
007000980910     D soc001        E DS                  EXTNAME(xsoc001ds)
007100980910     D xsocds          DS          1000
007200990506     D*  Prototipazione delle procedure
007300990506     D/COPY *LIBL/SRCCPY,PJX002PR
007400951002?     *--------------------------------------------------------------*
007500951002?     *  CICLO PRINCIPALE                                            *
007600951002?     *--------------------------------------------------------------*
007700951002      *  Impostazione parametri prima videata
007800951002     C                   EXSR      INZ01
007900951002      *  Loop gestione videata
008000951002     C     WFINE         DOUEQ     'S'
008100951002     C                   EXSR      GESD01
008200951002     C                   END
008300951002      *  Fine Lavoro
008400951002     C     FINE          TAG
008500951002     C                   SETON                                        LR
008600951002?     *--------------------------------------------------------------*
008700951002?     *  INZ01: Inizializzazione prima videata                       *
008800951002?     *--------------------------------------------------------------*
008900951002     C     INZ01         BEGSR
009000951002      *
009100020403     C                   SETOFF                                       2898
009200020510     C                   SETOFF                                       4050
009300030211     c                   movel     'A'           tipov
009400020411     C                   movel     *blanks       V1CPDR
009500020404     C                   MOVEL     *BLANKS       V1DPDR
009600020510     C                   MOVEL     'S'           numpv
009700951002     C*
009800951002     C                   ENDSR
009900951002?     *--------------------------------------------------------------*
010000951002?     *  GESD01: Gestione prima videata                              *
010100951002?     *--------------------------------------------------------------*
010200951002     C     GESD01        BEGSR
010300951002      *
010400980910     C                   EXFMT     y350d01
010500020403     C                   SETOFF                                       2898
010600020510     C                   SETOFF                                       4050
010700951002      *  Fine Lavoro
010800951002     C     *INKC         IFEQ      '1'
010900951002     C                   MOVEL     'S'           WFINE
011000951002     C                   GOTO      FINVD1
011100951002     C                   END
011200020403     C*  effettua ricerche per fornitore
011300980910     c                   if        *inkd = *on
011400980910     c                   exsr      search
011500020503     C                   GOTO      FINVD1
011600980910     c                   end
011700951002      *  Controlli
011800951002     C                   EXSR      CTR01
011900951002     C   28              GOTO      FINVD1
012000020510      *  No errori
012100020510     c                   if        *inkf
012200020510     C                   EXSR      aggio
012300020510     C                   EXSR      inz01
012400020510     c                   end
012500951002      *
012600951002     C     FINVD1        ENDSR
012700951002?     *--------------------------------------------------------------*
012800951002?     *  CTR01: Controlli prima videata                              *
012900951002?     *--------------------------------------------------------------*
013000951002     C     CTR01         BEGSR
013100030211     C* tipo
013101060630     c                   eval      tlztip = tipov
013300060630     c                   if        tipov = 'A'
013400030211     c                   eval      tlztip = 'P'
013900060630     c                   end
014000020510      *  Controllo codice fornitore
014100020510     c                   if        v1cpdr = *blanks
014200020510     C                   SETON                                        4028
014300020510     C                   MOVEL     ERR(1)        $MSG
014400020510     C                   GOTO      FINCT1
014500020510     c                   end
014600020503     C                   clear                   v1dpdr
014700020411     C                   move      V1CPDR        kksc
014800020510     C                   move      V1CPDR        tlzpdr
014900980910     c                   movel     *blank        sogdes
015000980910     C     Kfrn          CHAIN     anfrn01l                           30
015100030211     C  n30Krco          CHAIN     anrco98j                           30
015200030211     C  N30ktlz          CHAIN     tntlz01l                           31
015300980911     C                   MOVEL(p)  sogdes        v1dpdr
015400030211     C     *IN31         IFEQ      '1'
015500060630     C                   if        tipov = 'A' or tipov = 'T'
015600951002     C                   SETON                                        4028
015700951002     C                   MOVEL     ERR(2)        $MSG
015800951009     C                   GOTO      FINCT1
015900951002     C                   END
016000030211     c                   else
016100030211     C                   if        tlzatb <> *blanks
016200030211     C                   SETON                                        4028
016300030211     C                   MOVEL     ERR(2)        $MSG
016400030211     C                   GOTO      FINCT1
016500030211     C                   END
016600030211     C                   END
016700030211     C     *IN30         IFEQ      '1'
016800030211     C                   SETON                                        4028
016900030211     C                   MOVEL     ERR(2)        $MSG
017000030211     C                   GOTO      FINCT1
017100030211     C                   END
017101060630     c                   select
017200060630     C                   when      tipov = 'A' and
017300030211     C                             rcoctgan02 <> 'N   ' and
017400020527     C                             rcoctgan02 <> 'I   ' and
017500020527     C                             rcoctgan02 <> 'M   '
017600980910     C                   SETON                                        4028
017700980910     C                   MOVEL     ERR(3)        $MSG
017800980910     C                   GOTO      FINCT1
017801060630     C                   when      tipov = 'T' and
017802060630     C                             rcoctgan02 <> 'T   '
017805060630     C                   SETON                                        4028
017806060630     C                   MOVEL     ERR(6)        $MSG
017807060630     C                   GOTO      FINCT1
017900060630     c                   endsl
017901080104     c* con la nuova logica tutti i fornitori con stessa partita iva devono
017902080104     c* avere il fornitore di fatturazione quindi non controllo più
017903080104     c* l'incongruenza
018000080104     C*                  if        numpv = 'S' and frnforfatt <> *blanks
018100080104     C*                  SETON                                        4028
018200080104     C*                  MOVEL     ERR(5)        $MSG
018300080104     C*                  GOTO      FINCT1
018400080104     c*                  end
018500951003      *
018600951009     C     FINCT1        ENDSR
018700020404     C************************************************************
018800020404     C* RICERCHE
018900020404     C************************************************************
019000020404     C     SEARCH        BEGSR
019100980910     C*
019200980910     C* determino Riga/Colonna del cursore
019300980910     C                   MOVE      nrg           R$$
019400980910     C                   MOVE      ncl           C$$
019500980910     C                   Z-ADD     RIRI          CURR
019600980910     C                   Z-ADD     COCO          CURC
019700980910     C*
019800980910     C* I campi che al momento dell'F4 risultano protetti non devono
019900980910     C* essere abilitati alla funzione di ricerca
020000980910     C*
0201009809101    C                   SELECT
020200020423     C* FMTD1  - ricerca fornitore
020300060630    >C                   WHEn      h1nmfl = 'V1CPDR' and (tipov = 'A' or
020301060630    >C                             tipov = 'T')
020400980910     C*  deposita la KPJBU
020500980910     C                   MOVEL     KPJBU         DKPJBU          256
020600020503     C                   movel     xscsoc        parcsf
020700020404     C                   MOVEl     V1DPDR        PARRSC
020800980910     C                   Z-ADD     *ZEROS        PARPDR
020900060630     C                   eval      partip = 'T '
021000980910     C                   MOVEL     *BLANKS       PARFLG
021100020419     C                   MOVEL(p)  PARAM         KPJBU
021200980910     C                   CALL      'TRUL16R'
021300980910     C                   PARM                    KPJBA
021400980910     C                   MOVEL     KPJBU         PARAM
021500980910IF  2C     PARFLG        IFEQ      '3'
021600980910     C                   MOVEL     *BLANKS       V1DPDR
021700020403     C                   SETON                                        5028
021800020510     C                   MOVEL     ERR(4)        $MSG
021900980910X   2C                   ELSE
022000020422     C                   MOVEl     *zeros        V1CPDR
022100020422     C                   MOVE      PARPDR        V1CPDR
022200020403     C                   MOVEL     PARRSC        V1DPDR
022300980910E   2C                   ENDIF
022400020510     C                   ENDsl
022500980910     C* REIMPOSTA LA KPJBU
022600980910     C                   MOVEL     DKPJBU        KPJBU
022700980910     C                   SETON                                        98
022800980910     C* imposto pos. del cursore
022900980910     C                   Z-ADD     CURR          H1RIGA
023000980910     C                   Z-ADD     CURC          H1COLO
023100980910     C                   ENDsr
023200951006?     *--------------------------------------------------------------*
023300020510?     *  AGGIORNAMENTO TLZFLO                                        *
023400951006?     *--------------------------------------------------------------*
023500020510     C     aggio         BEGSR
023600951006      *
023700060630     c                   eval      tlztip = tipov
023800060630     c                   if        tipov = 'A'
023900030211     c                   eval      tlztip = 'P'
024400060630     c                   end
024500020510     c                   move      v1cpdr        tlzpdr
024600020510     c     ktlz          chain     tntlz01l
024700020510     c                   if        %found(tntlz01l)
024800020510     c                   movel     tlzflo        dtlz01
024900020510     c* SI
025000020510     c                   if        §tlzfl1 = 'S' and numpv = 'N'
025100020510     c                   movel     *blanks       §tlzfl1
025200020510     c                   movel     dtlz01        tlzflo
025300060630     c                   if        tipov = 'A' or tipov = 'T'
025400020510     c                   movel     *blanks       tlzftr
025500020510     c                   cleAR                   tlzdtr
025600030211     c                   end
025700020510     c                   update    tntlz000
025800020510     c                   end
025900020510     c* NO
026000020510     c                   if        §tlzfl1 = ' ' and numpv = 'S'
026100020510     c                   movel     'S'           §tlzfl1
026200020510     c                   movel     dtlz01        tlzflo
026300060630     c                   if        tipov = 'A' or tipov = 'T'
026400020510     c                   movel     *blanks       tlzftr
026500020510     c                   cleAR                   tlzdtr
026600030211     c                   end
026700020510     c                   update    tntlz000
026800020510     c                   end
026900030211     c                   else
027000030211     c* se non lo trova e il tipo è cooperativa lo scrivo
027100030828     c                   if        numpv = 'S' and (tipov='C' or tipov='D')
027200030211     c                   clear                   tntlz000
027300030211     c                   eval      tlzcsf = xscsoc
027400030828     c                   eval      tlztip = tipov
027500030211     C                   move      V1CPDR        tlzpdr
027600030211     c                   eval      tlzrsc = sogdes
027700030211     c                   eval      tlzind = indindriz
027800030211     c                   eval      tlzloc = indlocalit
027900030211     c                   eval      tlzcae = indcap
028000030211     c                   eval      tlzpro = indprov
028100030211     c                   eval      tlznar = indstato
028200030211     c                   movel     'S'           §tlzfl1
028300030211     c                   movel     dtlz01        tlzflo
028400030211     c* faccio finta di aver trasmesso
028500030211     c                   eval      tlzftr = 'T'
028600030211     c                   move      *date         tlzdtr
028700030211     c                   write     tntlz000
028800020510     c                   end
028900030211     c                   end
029000951006      *
029100951006     C                   ENDSR
029200980910     C*----------------------------------------------------*
029300980910     C* Reperimento dati società
029400980910     C*----------------------------------------------------*
029500980910     C     REPSOC        BEGSR
029600980910     C*
029700980910     C                   CALLB     'XSOC'
029800980910     C                   PARM                    TIPXSC            6
029900980910     C                   PARM                    SOCXSC            3
030000980910     C                   PARM                    CDSXSC            9 0
030100980910     C                   PARM                    MODXSC            3
030200980910     C                   PARM      *blanks       RTNXSC            1
030300980910     C                   PARM                    XSOCDS
030400980910     C                   PARM                    KPJBA
030500980910     C*
030600980910     C                   ENDSR
030700951006?     *--------------------------------------------------------------*
030800951006?     * *INZSR: Operazioni iniziali                                  *
030900951006?     *--------------------------------------------------------------*
031000951009     C     *INZSR        BEGSR
031100951006      *
031200951006     C     *ENTRY        PLIST
031300951006     C                   PARM                    KPJBA
031400980910     C*---------- RICERCA DITTA :
031500980910     C                   MOVEL     'SOC001'      TIPXSC
031600980910     C                   MOVEL     *BLANK        SOCXSC
031700980910     C                   EXSR      REPSOC
031800980910     C     RTNXSC        IFNE      '1'
031900980910     C                   MOVEL     XSOCDS        SOC001
032000980910     C                   MOVEL     xscrgs        RSUT             20
032100020417     c                   else
032200020417     c                   goto      fine
032300980910     c                   end
032400020415     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
032500020415     C                   movel     'F     '      $kcc              6
032600020415     C                   movel     *blanks       $ksc              8
032700020415     C                   callb     'XCAPCLIFOR'
032800020415     C                   parm                    xscsoc
032900020415     C                   parm                    $kcc
033000020415     C                   parm                    $ksc
033100020415     C                   movel     $kcc          forita            6
033200020423     C     Kfrn          KLIST
033300020423     C                   KFLD                    xscsoc
033400020423     C                   KFLD                    kksc
033500980910     C     Krco          KLIST
033600020417     C                   KFLD                    xscsoc
033700030211     C                   KFLD                    rcosnatura
033800980910     C                   KFLD                    KKSC
033900030211     c                   eval      rcosnatura = 'F'
034000020510     C     Ktlz          KLIST
034100020510     C                   KFLD                    tlztip
034200020510     C                   KFLD                    tlzpdr
034300020510     C                   KFLD                    xscsoc
034400951006      *  Definisco variabili
034500980910     C                   Z-ADD     0             CURR              2 0
034600980910     C                   Z-ADD     0             CURC              2 0
034700951006      *  Inizializzo variabili
034800951006     C                   MOVEL     'N'           WFINE             1
034900980910      *
035000980910     c                   endsr
035100951006      *--------------------------------------------------------------*
035200951012** ERR
035300020404Codice fornitore obbligatorio                                         01
035400020404Codice fornitore inesiste o annullato                                 02
035500020415Codice digitato non appartenente ad un fornitore-autotrasportatore    03
035600020510Non esistono autotrasportatori con chiave alfabetica rischiesta       04
035700020510Incongruenza: per il fornitore esiste una numerazione automatica      05
035800060630Codice digitato non appartenente ad un fornitore-trazionista          06
