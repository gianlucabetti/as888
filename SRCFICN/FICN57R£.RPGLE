000100020415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020415     H*PARMS ACTGRP(QILE)
000300030828     H/TITLE  Lancio stampa autofattura per conteggi aut./cop./aff/defl
000400020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951009?     *--------------------------------------------------------------*
000600020423      * ficn57R                                                      *
000700951009      *     *----------------------------------------------*         *
000800030828      *         stampa copia iva conteggi aut./cop./aff/defl         *
000900951009      *     *---------------------------------------------*          *
001000951009      *                                                              *
001100951009?     *--------------------------------------------------------------*
001200951009      * KC - FINE LAVORO                                             *
001300951009      * KF - CONFERMA                                                *
001400951009?     *--------------------------------------------------------------*
001500020423     Fficn57D   CF   E             WORKSTN
001600020529     Ftntbe01l  IF   E           K DISK
001700020529     D dblc          E DS
001800030211     D dblo          E DS
001900030828     D dbld          E DS
001901040216     D dcaf          E DS
002000020411     D* Reperimento nome PGM
002100020411     D STATUS         SDS           333
002200020411     D  nompgm           *PROC
002300020411     D*---------------------------------------------------------------*
002400020411     D* CAMPI INTERNI
002500020411     D*---------------------------------------------------------------*
002600020411     D dataiso         s               d   datfmt(*iso)
002700020411     D dataeur         s               d   datfmt(*eur)
002800021127     D v1cdpr          s                   like(v1cdti)
002900951009     D KPJBA         E DS
003000020423     D soc001        E DS                  EXTNAME(xsoc001ds)
003100020423     D xsocds          DS          1000
003200020419     D ficn54ds      e ds                  inz
003201031124     D dspar           ds
003202031124     d  tipo                          1
003203031124     d  causale                       4
003300951009?     *--------------------------------------------------------------*
003400951009?     *  CICLO PRINCIPALE                                            *
003500951009?     *--------------------------------------------------------------*
003600951009      *  Loop gestione videata
003700951009     C     WFINE         DOUEQ     'S'
003800000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
003900951012     C                   END
004000020418     C                   END
004100980312     C*
004200020529     C     FINE          TAG
004300951009     C                   SETON                                        LR
004400020405?     *--------------------------------------------------------------*
004500020405?     *  GESS01: Gestione prima videata                              *
004600020405?     *--------------------------------------------------------------*
004700020405     C     GESS01        BEGSR
004800020405      *
004900020419     c                   if        *in28
005000020419     c                   setoff                                       28
005100020419     C                   write     y350d01
005200020419     c                   seton                                        28
005300020419     C                   END
005400020415     C                   EXFMT     y350d01
005500020415      *  Fine Lavoro
005600020415     C     *INKC         IFEQ      '1'
005700020415     C                   MOVEL     'S'           WFINE
005800020415     c                   goto      finvd1
005900020415     C                   END
006000951009      *  Controlli
006100951009     C                   EXSR      CTR01
006200020405     C   28              GOTO      FINVD1
006300020408      *  CONFERMA
006400951013     C     *INKF         IFEQ      '1'
006500020423     c                   clear                   ficn54ds
006600020419     c                   move      xscsoc        f54soc
006700020423     c                   movel     '999     '    f54uni
006800020423     c                   move      *all'0'       f54pd1
006900020423     c                   move      *all'9'       f54pd2
007000020419     c                   move      wdatda        f54dti
007100020419     c                   move      wdataa        f54dtf
007200020423     c                   move      'N'           f54lis
007201031124     c                   movel     tipo          f54tip
007202031124     c                   movel     causale       f54cau
007300020419     C                   MOVEL(p)  ficn54ds      KPJBU
007400020419     c     knmus         ifeq      *all'1'
007500030828     c                   select
007502031124     c                   when      causale <> ' '
007503031124     c                   call      'FICN96C'
007504031124     c                   parm                    kpjba
007600030828     c                   when      tipo = 'A'
007700020423     c                   call      'FICN56C'
007800020419     c                   parm                    kpjba
007900030828     c                   when      tipo = 'C'
008000030211     c                   call      'FICN65C'
008100030211     c                   parm                    kpjba
008200030828     c                   when      tipo = 'D'
008300030828     c                   call      'FICN68C'
008400030828     c                   parm                    kpjba
008500030828     c                   endsl
008600030211     c                   else
008700030828     c                   select
008702031124     c                   when      causale <> ' '
008703031124     C                   MOVEL     'FN96'        KCOAZ
008800030828     c                   when      tipo = 'A'
008900020422     C                   MOVEL     'FN55'        KCOAZ
009000030828     c                   when      tipo = 'C'
009100030211     C                   MOVEL     'FN65'        KCOAZ
009200030828     c                   when      tipo = 'D'
009300030828     c                   movel     'FN68'        kcoaz
009400030828     c                   endsl
009500030214     C                   CALL      'BCH10'
009600030214     C                   PARM                    KPJBA
009700030211     c                   end
009701040216     c* archiviaz. prestaz. occasionali dalle archiviazioni autotrasp./
009702040216     c* cooperative/aff/def.
009703040216     c                   if        residv = 'SI' and causale = ' '
009704040216     c                   exsr      srcau
009705040216     c                   end
009706040216     c*
009800020423     c                   movel     'S'           wfine
009900980911     C                   END                                                    V3CTOT = y35FAT
010000951009      *
010100951009     C     FINVD1        ENDSR
010200951009?     *--------------------------------------------------------------*
010300951009?     *  CTR01: Controlli prima videata                              *
010400951009?     *--------------------------------------------------------------*
010500951009     C     CTR01         BEGSR
010600951009      *
010700020419     c                   setoff                                       28
010800020415     c* data da
010900020415     c                   if        v1cdti <> 0
011000020415     c     *eur          test(d)                 v1cdti                 41
011100020415     c                   if        *in41
011200020415     c     *dmy          test(d)                 v1cdti                 41
011300030807     c  n41*dmy          move      v1cdti        dataeur
011400030807     c  n41              move      dataeur       v1cdti
011500020415     c                   end
011600020415     c                   else
011700020415     c                   seton                                        41
011800020415     c                   end
011900030807     c                   if        v1cdti <> 0 and not *in41
012000030807     c                   move      v1cdti        dataeur
012100030807     c                   move      dataeur       v1cdti
012200030807     c                   adddur    20:*d         dataeur
012300030807     c                   move      dataeur       v1cdtf
012400030807     c                   end
012500020415     c* data al
012600020415     c                   if        v1cdtf <> 0
012700020415     c     *eur          test(d)                 v1cdtf                 45
012800020415     c                   if        *in45
012900020415     c     *dmy          test(d)                 v1cdtf                 45
013000020415     c  n45*dmy          move      v1cdtf        dataeur
013100020415     c  n45              move      dataeur       v1cdtf
013200020415     c                   end
013300020415     c                   else
013400020423     c                   if        not *in41
013500020423     c                   move      v1cdti        v1cdtf
013600020423     c                   else
013700020415     c                   seton                                          2845
013800020415     c                   end
013900020423     c                   end
014000020415     c                   if        *in41 or *in45
014100020415     c                   seton                                          28
014200020415     c                   else
014300020415     c                   movel     v1cdti        dataeur
014400020415     c                   movel     dataeur       dataiso
014500020415     c                   movel     dataiso       wdatda            8 0
014600020415     c                   movel     v1cdtf        dataeur
014700020415     c                   movel     dataeur       dataiso
014800020415     c                   movel     dataiso       wdataa            8 0
014900020415      *  Controllo limiti due date
015000020415     C     WDATDA        IFGT      WDATAA
015100020415     C                   SETON                                        414528
015200020415     C                   END
015300020415     c                   end
015400021127     c* messaggio informativo se la data scelta è diversa dalla data fatt.
015500020529     c* in corso
015600020529     c                   if        v1cdti <> v1cdpr
015700020529     c                   eval      msgv = 'ATTENZIONE: la data scelta è diversa-
015800020529     c                              dalla data di fatturazione in corso'
015900020529     c                   else
016000020529     c                   clear                   msgv
016100020529     c                   end
016200951010      *
016300020419     C                   ENDSR
016400980911     C*----------------------------------------------------*
016500980911     C* Reperimento dati società
016600980911     C*----------------------------------------------------*
016700980911     C     REPSOC        BEGSR
016800980911     C*
016900980911     C                   CALLB     'XSOC'
017000980911     C                   PARM                    TIPXSC            6
017100980911     C                   PARM                    SOCXSC            3
017200980911     C                   PARM                    CDSXSC            9 0
017300020415     C                   PARM      *blanks       MODXSC            3
017400980911     C                   PARM      *blanks       RTNXSC            1
017500980911     C                   PARM                    XSOCDS
017600980911     C                   PARM                    KPJBA
017700980911     C*
017800980911     C                   ENDSR
017900951011?     *--------------------------------------------------------------*
018000040216?     *  srcau : archiviaz. prestaz. residuali                       *
018100951011?     *--------------------------------------------------------------*
018200040216     C     srcau         BEGSR
018201040216     c*
018202040216     c                   movel(p)  'CAF'         tbecod
018203040216     c     tbecod        setll     tntbe01l
018204040216     c                   do        *hival
018205040216     c     tbecod        reade     tntbe01l
018206040216     c* fine file
018207040216     c                   if        %eof(tntbe01l)
018208040216     c                   leave
018209040216     c                   end
018210040216     c* record annullato
018211040216     c                   if        tbeatb <> ' '
018212040216     c                   iter
018213040216     c                   end
018214040216     c* solo tipo anagrafico richiesto
018215040216     c                   movel(p)  tbeuni        dcaf
018216040216     c                   if        §caftip <> tipo
018217040216     c                   iter
018218040216     c                   end
018219040216     c                   movel     tbeke1        f54cau
018220040216     C                   MOVEL(p)  ficn54ds      KPJBU
018221040216     c     knmus         ifeq      *all'1'
018223040216     c                   call      'FICN96C'
018224040216     c                   parm                    kpjba
018225040216     c                   else
018228040216     C                   MOVEL     'FN96'        KCOAZ
018229040216     C                   CALL      'BCH10'
018230040216     C                   PARM                    KPJBA
018231040216     c                   end
018232040216     c                   enddo
018233040216     c*
018234040216     C                   ENDSR
018235040216?     *--------------------------------------------------------------*
018236040216?     *  *INZSR: Operazioni di inizializzazione dati                 *
018237040216?     *--------------------------------------------------------------*
018238040216     C     *INZSR        BEGSR
018300951011      *
018400951011     C     *ENTRY        PLIST
018500951011     C                   PARM                    KPJBA
018600031124     c                   movel     kpjbu         dspar
018700030828     c                   select
018800031124     c                   when      causale <> ' '
018801031124     c                   eval      destip = 'PRESTAZ.OCCASION.'
018802040216     c                   seton                                        60
018803031124     c                   when      tipo = 'A'
018900030211     c                   eval      destip = 'AUTOTRASPORTATORI'
019000030828     c                   when      tipo = 'C'
019100030211     c                   eval      destip = '   COOPERATIVE   '
019200030828     c                   when      tipo = 'D'
019300030828     c                   eval      destip = 'AFFLUEN./DEFLUEN.'
019400030828     c                   endsl
019401040216     c                   eval      residv = 'NO'
019500020418      *  Inizializzo variabili
019600020418     C                   MOVEL     '1'           WTPVID            1
019700020418     C                   MOVEL     'N'           WFINE             1
019800020415     C*---------- RICERCA DITTA :
019900020415     C                   MOVEL     'SOC001'      TIPXSC
020000020415     C                   MOVEL     *BLANK        SOCXSC
020100020415     C                   EXSR      REPSOC
020200980911     C     RTNXSC        IFNE      '1'
020300980911     C                   MOVEL     XSOCDS        SOC001
020400980911     C                   MOVEL     xscrgs        RSUT             20
020500020418     c                   else
020600020529     c                   goto      fine
020700980911     c                   end
020800020529     C     Ktbe          KLIST
020900020529     C                   KFLD                    tbecod
021000020529     C                   KFLD                    tbeke1
021100030211      *  lettura tabella BLC
021200030211     c                   movel(p)  '1'           tbeke1
021300030828     c                   select
021400030828     c                   when      tipo = 'A'
021500020529     c                   movel     'BLC'         tbecod
021600020529     c     ktbe          chain     tntbe01l
021700020529     c                   if        %found(tntbe01l)
021800020529     c                   movel     tbeuni        dblc
021900020529     c* data protocollo
022000020529     c                   move      §blcdtp       dataiso
022100020529     c                   else
022200020529     c                   clear                   dblc
022300020529     c                   end
022400030211      *  lettura tabella BLO
022500030828     c                   when      tipo = 'C'
022600030211     c                   movel     'BLO'         tbecod
022700030211     c     ktbe          chain     tntbe01l
022800030211     c                   if        %found(tntbe01l)
022900030211     c                   movel     tbeuni        dblo
023000030211     c* data protocollo
023100030211     c                   move      §blodtp       dataiso
023200030211     c                   else
023300030211     c                   clear                   dblo
023400030211     c                   end
023500030828      *  lettura tabella BLD
023600030828     c                   when      tipo = 'D'
023700030828     c                   movel     'BLD'         tbecod
023800030828     c     ktbe          chain     tntbe01l
023900030828     c                   if        %found(tntbe01l)
024000030828     c                   movel     tbeuni        dbld
024100030828     c* data protocollo
024200030828     c                   move      §blddtp       dataiso
024300030828     c                   else
024400030828     c                   clear                   dbld
024500030828     c                   end
024600030828     c                   endsl
024700030828     c*
024800030828     c                   if        %found(tntbe01l)
024900030828     c                   move      dataiso       dataeur
025000030828     c                   move      dataeur       v1cdti
025100030828     c                   move      dataeur       v1cdpr
025200030828     c                   adddur    20:*d         dataeur
025300030828     c                   move      dataeur       v1cdtf
025400030828     c                   else
025500030828     c                   move      0             v1cdti
025600030828     c                   move      0             v1cdtf
025700030828     c                   end
025800951012     C                   ENDSR
