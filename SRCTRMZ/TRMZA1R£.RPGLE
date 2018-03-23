000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400090518     Ftrmz85d   CF   E             WORKSTN
000500090518     Faitrs01l  if   e           k disk
000600090518     Faitra01l  if   e           k disk
000700090518     Fazorg01l  if   e           k disk
000800090519     Ftrmz850f  uf a e           k disk    usropn
000900050801     ftabel00f  if   e           k disk
001000020304     dkpjba          e ds
001100030630     d Tibs36Ds      e ds
001200070529     d Tibs02Ds      e ds
001300090518     d daip          e ds
001400070605     d dataiso         s               d   datfmt(*iso)
001500070605     d dataeur         s               d   datfmt(*eur)
001600070605     D WLBDA8          DS
001700070605     D  G02DAT                 1      8  0
001800070605     D  G02INV                 9     16  0
001900070605     D  G02ERR                17     17
002000070605     D  G02TGI                18     22  0
002100090518     D tibsSocI0...
002200090518     D               E DS                  QUALIFIED
002300090518     D                                     INZ
002400090518     D tibsSocO0...
002500090518     D               E DS                  QUALIFIED
002600090518     D                                     INZ
002700090518
002800090518     D prmRqsOpCode...
002900090518     D                 S             10A
003000090518     D prmRpyOpCode...
003100090518     D                 S             10A
003200090518     D prmRpyIdMsg...
003300090518     D                 S             10I 0
003400090518     D prmRqsFormato...
003500090518     D                 S             10A
003600090518     D prmRqsDataSize...
003700090518     D                 S             10I 0
003800090518     D prmRpyFormato...
003900090518     D                 S             10A
004000090518     D prmRpyDataSize...
004100090518     D                 S             10I 0
004200080502      *
004300090519     d coman           s             99
004400090519     d luncmd          s             15  5
004500090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
004600090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
004700090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
004800090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
004900090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
005000000000     C*---------------------------------------------------------------*
005100020304      *
005200031105     c                   do        *hival
005300031105     c                   exfmt     video1
005400031105     c                   clear                   $msg
005500031105     c   kc              leave
005600031105     c                   exsr      controv1
005700031105     c   96              iter
005800050623     c                   if        *inkf
005900050623     c                   exsr      elabora
006000050805     c                   leave
006100050623     c                   endif
006200050805     c                   enddo
006300050802
006400090518     ** Chiudo il programma.x dati societari
006500090518     C                   CALL      'TIBSSOCR'
006600090518     C                   PARM      'FINALIZE'    prmRqsOpCode
006700090518     C                   PARM                    prmRpyOpCode
006800090518     C                   PARM                    prmRpyIdMsg
006900090518      *
007000090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
007100090519     c                   movea     cmd4          coman
007200090519     c                   exsr      comandi
007300090519      *esegue crtdupobj da qtemp a wrkquery
007400090519     c                   movea     cmd5          coman
007500090519     c                   exsr      comandi
007600090519      *elimina file in qtemp x la filiale
007700090519     c                   close     trmz850f
007800090519     c                   movea     cmd3          coman
007900090519     c                   exsr      comandi
008000020308     c                   seton                                        lr
008100050802     c*-------------------------------------------------
008200090518     c     elabora       begsr
008300050802     c*-------------------------------------------------
008400090518     c     *loval        setll     aitrs01l
008500070530     c                   do        *hival
008600090518     c                   read      aitrs01l                               98
008700070530     c   98              leave
008800090518     c                   clear                   trmz8500
008900090518      * filtro se cerca società in forza alla data
009000090518     c                   if        vdatal > 0
009100090518     c                   if        trsdfc > 0 and trsdfc < inimese
009200070530     c                   iter
009300070530     c                   endif
009400090518     c                   endif
009500090518      * filtro se cerca società in forza alla data
009600090518     c                   if        vdatdal > 0
009700090518     c                   if        trsdfc > 0 or trsdec < vdatdalg
009800090518     c                   iter
009900090518     c                   endif
010000090518     c                   endif
010100090518     c                   clear                   t85socgd
010200090518     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
010300090518     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
010400090518     C                   RESET                   tibsSocI0
010500090518     C                   EVAL      tibsSocI0.idSocieta = trssocg
010600090518     C
010700090518     C                   CALL      'TIBSSOCR'
010800090518     C                   PARM      'GETANAGRAF'  prmRqsOpCode
010900090518     C                   PARM      *BLANK        prmRpyOpCode
011000090518     C                   PARM      *ZERO         prmRpyIdMsg
011100090518     C                   PARM      'TIBSSOCI0'   prmRqsFormato
011200090518     C                   PARM                    tibsSocI0
011300090518     C                   PARM                    prmRqsDataSize
011400090518     C                   PARM      'TIBSSOCO0'   prmRpyFormato
011500090518     C                   PARM                    tibsSocO0
011600090518     C                   PARM                    prmRpyDataSize
011700090518     c                   if         PRMRPYIDMSG >= 0 and
011800090518     c                              TIBSSOCO0.IDSOCIETA <> *blank
011900090518     c                   eval      t85socgd = tibsSocO0.RAGSOCIALE
012000090518     c                   else
012100090518     c                   eval      t85socgd = 'Società Errata'
012200090518     c                   end
012300090518      * ragione sociale società
012400090924     c                   eval      t85note = 'Non esistono autisti accreditati'
012500090924     c     trsnrc        setll     aitra01l
012600090924     c                   do        *hival
012700090924     c     trsnrc        reade     aitra01l
012800090924     c                   if        %eof(aitra01l)
012900090924     c                   leave
013000090924     c                   endif
013100090924     c                   movel     trasoc        t85rsoc
013200090924     c                   if        tradfi = 0
013300090924     c                   clear                   t85note
013400090924     c                   leave
013500090518     c                   endif
013600090924     c                   enddo
013700090518      * descrizione filiale
013800090518     c     trsfil        chain     azorg01l
013900090518     c                   if        %found(azorg01l)
014000090518     c                   movel     orgdes        t85fild
014100090518     c                   move      orgfl3        T85DIST
014200090518     c                   move      orgcar        T85AREA
014300090518     c                   endif
014400070530      * descrizione area
014500090518     c                   movel     orgcar        kkey
014600070530     c                   move      '05'          kcod
014700070530     c     ktab          chain     tabel00f
014800070530     c                   if        %found(tabel00f)
014900090518     c                   movel     tbluni        t85aread
015000070530     c                   endif
015100090518     c                   if        trsidp  <> *blank
015200090518     c                   clear                   tibs02ds
015300090518     c                   movel     'C'           t02mod
015400090518     c                   movel     knsif         t02sif
015500090518     c                   movel     'AIP'         t02cod
015600090518     c                   movel     trsidp        t02ke1
015700090518     c                   call      'TIBS02R'
015800090518     c                   parm                    KPJBA
015900090518     c                   parm                    TIBS02DS
016000090518     c                   if        t02err =  *blanks
016100090518     c                   movel     t02uni        daip
016200090518     c                   movel     §aipdes       t85idpd
016300090518     c                   endif
016400090518     c                   endif
016500090518     c                   z-add     TrsNRC        T85NRC
016600090518     c                   z-add     TrsFIL        T85FIL
016700090518     c                   move      TrsSOCG       T85SOCG
016800090518     c                   move      TrsIVA        T85IVA
016900090518     c                   z-add     TrsDEC        T85DEC
017000090518     c                   z-add     TrsDFC        T85DFC
017100090518     c                   move      TrsCFI        T85CFI
017200090518     c                   move      TrsIDP        T85IDP
017300090518     c                   move      TrsIDM        T85IDM
017400090518     c                   move      TrsIDA        T85IDA
017500090518     c                   move      TrsDIA        T85DIA
017600090518     c                   move      Trsiva        T85IPIVA
017700090518     c                   write     trmz8500
017800090518     c                   enddo
017900070530      *
018000070530     c                   endsr
018100031105     c*-------------------------------------------------
018200031105     c     controv1      begsr
018300031105     c*-------------------------------------------------
018400070606     c                   setoff                                       9697
018500070605      *data disaccr. per turnover
018600070605     c                   setoff                                       44
018700090518     c                   if        vdatdal > 0  and vdatal > 0
018800070605     c                   seton                                        4496
018900070605     c                   goto      endcontr
019000070605     c                   endif
019100090518     c                   if        vdatal > 0
019200090518     C                   MOVE      Vdatal        G02DAT
019300070605     C                   MOVEL     *BLANK        G02ERR
019400070605     C                   CALL      'XSRDA8'
019500070605     C                   PARM                    WLBDA8
019600070605     C     G02ERR        IFEQ      '1'
019700070605     C                   SETON                                        4295
019800070605     c                   goto      endcontr
019900070605     C                   END
020000090518     c                   move      g02dat        vdatal
020100090518     c                   move      g02inv        vdatalg           8 0
020200090518     c                   move      g02inv        inimese           8 0
020300090518     c                   move      01            inimese
020400070529     c                   endif
020500051121
020600090518     c                   if        vdatdal > 0
020700090518     C                   MOVE      Vdatdal       G02DAT
020800090518     C                   MOVEL     *BLANK        G02ERR
020900090518     C                   CALL      'XSRDA8'
021000090518     C                   PARM                    WLBDA8
021100090518     C     G02ERR        IFEQ      '1'
021200090518     C                   SETON                                        4195
021300090518     c                   goto      endcontr
021400090518     C                   END
021500090518     c                   move      g02dat        vdatdal
021600090518     c                   move      g02inv        vdatdalg          8 0
021700090518     c                   endif
021800051122     c     endcontr      endsr
021900090519     c*-------------------------------------------------
022000090519     c     comandi       begsr
022100090519     c*-------------------------------------------------
022200090519      *
022300090519     C                   z-add     99            luncmd
022400090519     C                   CALL      'QCMDEXC'                            34
022500090519     C                   parm                    coman
022600090519     C                   parm                    luncmd
022700090519     c                   endsr
022800011026     c*-------------------------------------------------
022900011026     c     *inzsr        begsr
023000011026     c*-------------------------------------------------
023100020304     c     *entry        plist
023200020304     c                   parm                    kpjba
023300050801
023400050801     c     ktab          klist
023500050801     c                   kfld                    kkut              1 0
023600050801     c                   kfld                    kcod              2
023700050801     c                   kfld                    kkey              8
023800050801     c                   z-add     1             kkut
023900050801     c                   move      '05'          kcod
024000050801     c
024100050801
024200020218     C                   time                    w0120            14 0
024300020218     C                   move      w0120         wdat              8 0
024400030630     C                   CLEAR                   Tibs36Ds
024500030630     C                   EVAL      I36ute = knmus
024600030630     C                   CALL      'TIBS36R'
024700030630     C                   PARM                    tibs36ds
024800090518     ** Inizializzo il programma. per dati societari
024900090518     C                   CALL      'TIBSSOCR'
025000090518     C                   PARM      'INIT'        prmRqsOpCode
025100090518     C                   PARM                    prmRpyOpCode
025200090518     C                   PARM                    prmRpyIdMsg
025300090518
025400090519      *clear file
025500090519     c                   movea     cmd1          coman
025600090519     c                   exsr      comandi
025700090519     c                   movea     cmd2          coman
025800090519     c                   exsr      comandi
025900090519     c                   open      trmz850f
026000090519
026100090518     c                   endsr
026200090519**  CMD1
026300090519CLRPFM    FILE(trmz850f)
026400090519**  CMD2
026500090519CRTDUPOBJ OBJ(trmz850F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
026600090519**  CMD3
026700090519DLTF FILE(QTEMP/trmz850F)
026800090519**  CMD4
026900090519dltf file(WRKQUERY/trmz850f)
027000090519**  CMD5
027100090519CRTDUPOBJ OBJ(trmz850F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz850f) DATA(*YES)
