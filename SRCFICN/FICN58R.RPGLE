000100020415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020415     H*PARMS ACTGRP(QILE)
000300020419     H/TITLE  Lancio stampa autofattura per conteggi autotras.
000400020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951009?     *--------------------------------------------------------------*
000600020529      * ficn58R                                                      *
000700951009      *     *----------------------------------------------*         *
000800020529      *         stampa valorizzaz. conteggi autotras. arch. CD       *
000900951009      *     *---------------------------------------------*          *
001000951009      *                                                              *
001100951009?     *--------------------------------------------------------------*
001200951009      * KC - FINE LAVORO                                             *
001300951009      * KF - CONFERMA                                                *
001400951009?     *--------------------------------------------------------------*
001500020529     Fficn58D   CF   E             WORKSTN
001600020529     Ftntbe01l  IF   E           K DISK
001700020529     D dblc          E DS
001800020411     D* Reperimento nome PGM
001900020411     D STATUS         SDS           333
002000020411     D  nompgm           *PROC
002100020411     D*---------------------------------------------------------------*
002200020411     D* CAMPI INTERNI
002300020411     D*---------------------------------------------------------------*
002400020411     D dataiso         s               d   datfmt(*iso)
002500020411     D dataeur         s               d   datfmt(*eur)
002600021127     D v1cdpr          s                   like(v1cdti)
002700951009     D KPJBA         E DS
002800020423     D soc001        E DS                  EXTNAME(xsoc001ds)
002900020423     D xsocds          DS          1000
003000020529     D ficn58ds      e ds                  inz
003100951009?     *--------------------------------------------------------------*
003200951009?     *  CICLO PRINCIPALE                                            *
003300951009?     *--------------------------------------------------------------*
003400951009      *  Loop gestione videata
003500951009     C     WFINE         DOUEQ     'S'
003600000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
003700951012     C                   END
003800020418     C                   END
003900980312     C*
004000020529     C     FINE          TAG
004100951009     C                   SETON                                        LR
004200020405?     *--------------------------------------------------------------*
004300020405?     *  GESS01: Gestione prima videata                              *
004400020405?     *--------------------------------------------------------------*
004500020405     C     GESS01        BEGSR
004600020405      *
004700020419     c                   if        *in28
004800020419     c                   setoff                                       28
004900020419     C                   write     y350d01
005000020419     c                   seton                                        28
005100020419     C                   END
005200020415     C                   EXFMT     y350d01
005300020415      *  Fine Lavoro
005400020415     C     *INKC         IFEQ      '1'
005500020415     C                   MOVEL     'S'           WFINE
005600020415     c                   goto      finvd1
005700020415     C                   END
005800951009      *  Controlli
005900951009     C                   EXSR      CTR01
006000020405     C   28              GOTO      FINVD1
006100020408      *  CONFERMA
006200951013     C     *INKF         IFEQ      '1'
006300020529     c                   clear                   ficn58ds
006400020529     c                   move      xscsoc        f58soc
006500020529     c                   move      wdatda        f58dti
006600020529     c                   move      wdataa        f58dtf
006700020529     C                   MOVEL(p)  ficn58ds      KPJBU
006800020419     c     knmus         ifeq      *all'1'
006900020531     c                   call      'FICN58C'
007000020419     c                   parm                    kpjba
007100020419     c                   else
007200020529     C                   MOVEL     'FN59'        KCOAZ
007300020419     C                   CALL      'BCH10'
007400020419     C                   PARM                    KPJBA
007500020419     c                   end
007600020423     c                   movel     'S'           wfine
007700980911     C                   END                                                    V3CTOT = y35FAT
007800951009      *
007900951009     C     FINVD1        ENDSR
008000951009?     *--------------------------------------------------------------*
008100951009?     *  CTR01: Controlli prima videata                              *
008200951009?     *--------------------------------------------------------------*
008300951009     C     CTR01         BEGSR
008400951009      *
008500020419     c                   setoff                                       28
008600020415     c* data da
008700020415     c                   if        v1cdti <> 0
008800020415     c     *eur          test(d)                 v1cdti                 41
008900020415     c                   if        *in41
009000020415     c     *dmy          test(d)                 v1cdti                 41
009100020415     c  n41*dmy          move      v1cdti        dataeur
009200020415     c  n41              move      dataeur       v1cdti
009300020415     c                   end
009400020415     c                   else
009500020415     c                   seton                                        41
009600020415     c                   end
009700030807     c                   if        v1cdti <> 0 and not *in41
009800030807     c                   move      v1cdti        dataeur
009900030807     c                   move      dataeur       v1cdti
010000030807     c                   adddur    20:*d         dataeur
010100030807     c                   move      dataeur       v1cdtf
010200030807     c                   end
010300020415     c* data al
010400020415     c                   if        v1cdtf <> 0
010500020415     c     *eur          test(d)                 v1cdtf                 45
010600020415     c                   if        *in45
010700020415     c     *dmy          test(d)                 v1cdtf                 45
010800020415     c  n45*dmy          move      v1cdtf        dataeur
010900020415     c  n45              move      dataeur       v1cdtf
011000020415     c                   end
011100020415     c                   else
011200020423     c                   if        not *in41
011300020423     c                   move      v1cdti        v1cdtf
011400020423     c                   else
011500020415     c                   seton                                          2845
011600020415     c                   end
011700020423     c                   end
011800020415     c                   if        *in41 or *in45
011900020415     c                   seton                                          28
012000020415     c                   else
012100020415     c                   movel     v1cdti        dataeur
012200020415     c                   movel     dataeur       dataiso
012300020415     c                   movel     dataiso       wdatda            8 0
012400020415     c                   movel     v1cdtf        dataeur
012500020415     c                   movel     dataeur       dataiso
012600020415     c                   movel     dataiso       wdataa            8 0
012700020415      *  Controllo limiti due date
012800020415     C     WDATDA        IFGT      WDATAA
012900020415     C                   SETON                                        414528
013000020415     C                   END
013100020415     c                   end
013200020529     c* messaggio informativo se la data scelata è diversa dalla data fatt.
013300020529     c* in corso
013400020529     c                   if        v1cdti <> v1cdpr
013500020529     c                   eval      msgv = 'ATTENZIONE: la data scelta è diversa-
013600020529     c                              dalla data di fatturazione in corso'
013700020529     c                   else
013800020529     c                   clear                   msgv
013900020529     c                   end
014000951010      *
014100020419     C                   ENDSR
014200980911     C*----------------------------------------------------*
014300980911     C* Reperimento dati società
014400980911     C*----------------------------------------------------*
014500980911     C     REPSOC        BEGSR
014600980911     C*
014700980911     C                   CALLB     'XSOC'
014800980911     C                   PARM                    TIPXSC            6
014900980911     C                   PARM                    SOCXSC            3
015000980911     C                   PARM                    CDSXSC            9 0
015100020415     C                   PARM      *blanks       MODXSC            3
015200980911     C                   PARM      *blanks       RTNXSC            1
015300980911     C                   PARM                    XSOCDS
015400980911     C                   PARM                    KPJBA
015500980911     C*
015600980911     C                   ENDSR
015700951011?     *--------------------------------------------------------------*
015800951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
015900951011?     *--------------------------------------------------------------*
016000951011     C     *INZSR        BEGSR
016100951011      *
016200951011     C     *ENTRY        PLIST
016300951011     C                   PARM                    KPJBA
016400020418      *  Inizializzo variabili
016500020418     C                   MOVEL     '1'           WTPVID            1
016600020418     C                   MOVEL     'N'           WFINE             1
016700020415     C*---------- RICERCA DITTA :
016800020415     C                   MOVEL     'SOC001'      TIPXSC
016900020415     C                   MOVEL     *BLANK        SOCXSC
017000020415     C                   EXSR      REPSOC
017100980911     C     RTNXSC        IFNE      '1'
017200980911     C                   MOVEL     XSOCDS        SOC001
017300980911     C                   MOVEL     xscrgs        RSUT             20
017400020418     c                   else
017500020529     c                   goto      fine
017600980911     c                   end
017700020529     C     Ktbe          KLIST
017800020529     C                   KFLD                    tbecod
017900020529     C                   KFLD                    tbeke1
018000020529      *  lettura tabella BLC
018100020529     c                   clear                   dblc
018200020529     c                   movel     'BLC'         tbecod
018300020529     c                   movel(p)  '1'           tbeke1
018400020529     c     ktbe          chain     tntbe01l
018500020529     c                   if        %found(tntbe01l)
018600020529     c                   movel     tbeuni        dblc
018700020529     c* data protocollo
018800020529     c                   move      §blcdtp       dataiso
018900020529     c                   move      dataiso       dataeur
019000021127     c                   move      dataeur       v1cdti
019100021127     c                   move      dataeur       v1cdpr
019200021127     c                   adddur    20:*d         dataeur
019300021127     c                   move      dataeur       v1cdtf
019400020529     c                   else
019500020529     c                   clear                   dblc
019600021127     c                   move      0             v1cdti
019700021127     c                   move      0             v1cdtf
019800020529     c                   end
019900951012     C                   ENDSR
