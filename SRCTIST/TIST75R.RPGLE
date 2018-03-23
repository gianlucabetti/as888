000300030219     H/TITLE  Lancio statistica spedizioni in distinta da sede
000400030219     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000500951009?     *--------------------------------------------------------------*
000600100204      * tist75R                                                      *
000800100204      * statistica spedizioni consegnate con particolarità di attesa *
001000951009      *                                                              *
001100951009?     *--------------------------------------------------------------*
001500100204     Ftist75D   CF   E             WORKSTN
001501100204     Fazorg01l  if   E           k disk
001502100215     Fcnaco00f  if   E           k disk
001503100215     Ftabel00f  if   E           k disk
001800020411     D* Reperimento nome PGM
001900020411     D STATUS         SDS           333
002000020411     D  nompgm           *PROC
002001100204     D ddatiute      e ds
002002100204     D azuteds       e ds                  extname(AZUTE00F)
002003100204     D tibs34ds      E DS                  inz
002100020411     D*---------------------------------------------------------------*
002200020411     D* CAMPI INTERNI
002300020411     D*---------------------------------------------------------------*
002400030310     D datacom         s               d   datfmt(*iso)
002401030310     D dataiso         s               d   datfmt(*iso)
002500020411     D dataeur         s               d   datfmt(*eur)
002600951009     D KPJBA         E DS
002900100204     D tist75ds      e ds                  inz
002901100204     d og148         e ds
002902100215     d ds7r          e ds
003000951009?     *--------------------------------------------------------------*
003100951009?     *  CICLO PRINCIPALE                                            *
003200951009?     *--------------------------------------------------------------*
003300951009      *  Loop gestione videata
003400951009     C     WFINE         DOUEQ     'S'
003500000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
003600951012     C                   END
003700020418     C                   END
003800980312     C*
003900020529     C     FINE          TAG
004000951009     C                   SETON                                        LR
004100020405?     *--------------------------------------------------------------*
004200020405?     *  GESS01: Gestione prima videata                              *
004300020405?     *--------------------------------------------------------------*
004400020405     C     GESS01        BEGSR
004500020405      *
004600020419     c                   if        *in28
004700020419     c                   setoff                                       28
004800020419     C                   write     y350d01
004900020419     c                   seton                                        28
005000020419     C                   END
005100020415     C                   EXFMT     y350d01
005200020415      *  Fine Lavoro
005300020415     C     *INKC         IFEQ      '1'
005400020415     C                   MOVEL     'S'           WFINE
005500020415     c                   goto      finvd1
005600020415     C                   END
005700951009      *  Controlli
005800951009     C                   EXSR      CTR01
005900020405     C   28              GOTO      FINVD1
006000031203      *  personalizzazione batch
006100031203     C     *INKv         IFEQ      '1'
006101031203     C                   eval      kritb = '1'
006102031203     C                   call      'BCH09'
006103031203     C                   parm                    kpjba
006104031203     c                   end
006105031203      *  CONFERMA
006106031203     C     *INKF         IFEQ      '1'
006200100204     c                   clear                   tist75ds
006402100204     c                   move      wdatda        t75dti
006500100204     c                   move      wdataa        t75dtf
006501100204     c                   move      v1cfgs        t75fgs
006502100215     c                   move      v1cksc        t75ksc
006503100215     c                   move      v1cgma        t75gma
006600100204     C                   MOVEL(p)  tist75ds      KPJBU
006700020419     c     knmus         ifeq      *all'1'
006800100204     c                   call      'TIST76C'
006900020419     c                   parm                    kpjba
007000020419     c                   else
007100100204     C                   MOVEL     'ST76'        KCOAZ
007200020419     C                   CALL      'BCH10'
007300020419     C                   PARM                    KPJBA
007400020419     c                   end
007500020423     c                   movel     'S'           wfine
007600980911     C                   END                                                    V3CTOT = y35FAT
007700951009      *
007800951009     C     FINVD1        ENDSR
007900951009?     *--------------------------------------------------------------*
008000951009?     *  CTR01: Controlli prima videata                              *
008100951009?     *--------------------------------------------------------------*
008200951009     C     CTR01         BEGSR
008300951009      *
008400020419     c                   setoff                                       28
008401100215     c                   do
008500020415     c* data da
008600020415     c                   if        v1cdti <> 0
008700020415     c     *eur          test(d)                 v1cdti                 41
008800020415     c                   if        *in41
008900020415     c     *dmy          test(d)                 v1cdti                 41
009000020415     c  n41*dmy          move      v1cdti        dataeur
009100020415     c  n41              move      dataeur       v1cdti
009200020415     c                   end
009300020415     c                   else
009400020415     c                   seton                                        41
009500020415     c                   end
009600020415     c* data al
009700020415     c                   if        v1cdtf <> 0
009800020415     c     *eur          test(d)                 v1cdtf                 45
009900020415     c                   if        *in45
010000020415     c     *dmy          test(d)                 v1cdtf                 45
010100020415     c  n45*dmy          move      v1cdtf        dataeur
010200020415     c  n45              move      dataeur       v1cdtf
010300020415     c                   end
010400020415     c                   else
010500020423     c                   if        not *in41
010600020423     c                   move      v1cdti        v1cdtf
010700020423     c                   else
010800100215     c                   seton                                          45
010900020415     c                   end
011000020423     c                   end
011002030219     c
011100100204     c                   if        *in41 or *in45
011200020415     c                   seton                                          28
011201100215     c                   leave
011300020415     c                   else
011400020415     c                   movel     v1cdti        dataeur
011500020415     c                   movel     dataeur       dataiso
011600020415     c                   movel     dataiso       wdatda            8 0
011700020415     c                   movel     v1cdtf        dataeur
011800020415     c                   movel     dataeur       dataiso
011900020415     c                   movel     dataiso       wdataa            8 0
012000020415      *  Controllo limiti due date
012100020415     C     WDATDA        IFGT      WDATAA
012200020415     C                   SETON                                        414528
012201100215     c                   leave
012300020415     C                   END
012400020415     c                   end
012401100215      *  Controllo cliente
012402100215     c                   clear                   v1dksc
012403100215     c                   if        v1cksc = 0
012404100215     c                   seton                                        4328
012405100215     c                   leave
012406100215     c                   else
012407100215     c     kaco          chain     cnaco00f
012408100215     c                   if        %found(cnaco00f)
012409100215     c                   eval      v1dksc = acorag
012410100215     c                   else
012411100215     c                   seton                                        4328
012412100215     c                   leave
012413100215     c                   end
012414100215     c                   end
012415100215      *  Controllo particolarità consegna
012416100215     c                   if        v1cgma = ' '
012417100215     c                   seton                                        4428
012418100215     c                   leave
012419100215     c                   else
012420100215     c                   movel(p)  v1cgma        tblkey
012421100215     c     ktab          chain     tabel00f
012422100215     c                   if        not %found(tabel00f)
012425100215     c                   seton                                        4428
012426100215     c                   leave
012427100215     c                   else
012428100215     c                   movel     tbluni        ds7r
012429100215     c* solo con attesa ???
012430100215     c*                  if        §7RATT <> 'S'
012431100215     c*                  seton                                        4428
012432100215     c*                  leave
012433100215     c*                  end
012434100215     c                   end
012435100215     c                   end
012436100204      *  Controllo filiale
012437100204     c                   if        v1cfgs <> 999
012438100204     c     v1cfgs        chain     azorg01l
012439100204     c                   if        %found(azorg01l)
012440100204     c                   eval      og148= orgde8
012441100204     c                   if        §OGLPO = ' '
012442100204     c                   seton                                        4228
012443100204     c                   end
012444100204     c                   movel     orgdes        v1dfgs
012445100204     c                   else
012446100204     c                   seton                                        4228
012447100204     c                   clear                   v1dfgs
012448100204     c                   end
012449100204     c                   else
012450100204     c                   eval      v1dfgs = 'Tutte'
012451100204     c                   end
013300951010      *
013301100215     c                   enddo
013400020419     C                   ENDSR
015000951011?     *--------------------------------------------------------------*
015100951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
015200951011?     *--------------------------------------------------------------*
015300951011     C     *INZSR        BEGSR
015400951011      *
015500951011     C     *ENTRY        PLIST
015600951011     C                   PARM                    KPJBA
015601100204     c     *dtaara       define    §azute        azuteds
015602100204     c     *dtaara       define    §datiute      ddatiute
015603100204     C                   in(E)     *dtaara
015604100204     C                   IF        %Error  or  RSUT = *blanks
015605100204     C                   call      'TIBS34R'
015606100204     C                   parm                    Tibs34Ds
015607100204     C                   in        *dtaara
015608100204     c                   ENDIF
015609100215     c*
015610100215     c     kaco          klist
015611100215     c                   kfld                    acokut
015612100215     c                   kfld                    acokcc
015613100215     c                   kfld                    v1cksc
015614100215     c                   eval      acokut = 1
015615100215     c                   eval      acokcc = 0151
015616100215     c*
015617100215     c     ktab          klist
015618100215     c                   kfld                    tblkut
015619100215     c                   kfld                    tblcod
015620100215     c                   kfld                    tblkey
015621100215     c                   eval      tblkut = 1
015622100215     c                   eval      tblcod = '7R'
015700020418      *  Inizializzo variabili
015800020418     C                   MOVEL     '1'           WTPVID            1
015900020418     C                   MOVEL     'N'           WFINE             1
015901100204     c                   eval      v1cfgs = 999
015903030310     c                   move      udate         datacom
015904030310     c                   subdur    30:*d         datacom
019100951012     C                   ENDSR
