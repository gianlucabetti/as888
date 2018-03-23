000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300090518     Ftrmz85d   CF   E             WORKSTN
000400090518     Faitrs01l  if   e           k disk
000500090518     Faitra01l  if   e           k disk
000600090518     Fazorg01l  if   e           k disk
000700090519     Ftrmz850f  uf a e           k disk    usropn
000800050801     ftabel00f  if   e           k disk
000900020304     dkpjba          e ds
001000030630     d Tibs36Ds      e ds
001100120504     d*** Tibs02Ds      e ds
001200090518     d daip          e ds
001300120301     d dtrsflr       e ds
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
005200130123     c                   move      'N'           vmese
005300031105     c                   do        *hival
005400031105     c                   exfmt     video1
005500031105     c                   clear                   $msg
005600031105     c   kc              leave
005700031105     c                   exsr      controv1
005800031105     c   96              iter
005900050623     c                   if        *inkf
006000050623     c                   exsr      elabora
006100050805     c                   leave
006200050623     c                   endif
006300050805     c                   enddo
006400050802
006500090518     ** Chiudo il programma.x dati societari
006600090518     C                   CALL      'TIBSSOCR'
006700090518     C                   PARM      'FINALIZE'    prmRqsOpCode
006800090518     C                   PARM                    prmRpyOpCode
006900090518     C                   PARM                    prmRpyIdMsg
007000090518      *
007100090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
007200090519     c                   movea     cmd4          coman
007300090519     c                   exsr      comandi
007400090519      *esegue crtdupobj da qtemp a wrkquery
007500090519     c                   movea     cmd5          coman
007600090519     c                   exsr      comandi
007700090519      *elimina file in qtemp x la filiale
007800090519     c                   close     trmz850f
007900090519     c                   movea     cmd3          coman
008000090519     c                   exsr      comandi
008100020308     c                   seton                                        lr
008200050802     c*-------------------------------------------------
008300090518     c     elabora       begsr
008400050802     c*-------------------------------------------------
008500090518     c     *loval        setll     aitrs01l
008600070530     c                   do        *hival
008700090518     c                   read      aitrs01l                               98
008800070530     c   98              leave
008900090518     c                   clear                   trmz8500
009000090518      * filtro se cerca società in forza alla data
009100120413     c                   if        trsann <> *blank
009200120413     c                   iter
009300120413     c                   endif
009400090518     c                   if        vdatal > 0
009500130123      *considera anche quelle disaccreditate nel mese
009600130123     c                   if        vmese = 'S'
009700090518     c                   if        trsdfc > 0 and trsdfc < inimese
009800070530     c                   iter
009900070530     c                   endif
010000130123     c                   else
010100130123      *le esclude
010200130123     c                   if        trsdfc > 0
010300130123     c                   iter
010400130123     c                   endif
010500130123     c                   endif
010600090518     c                   endif
010700090518      * filtro se cerca società in forza alla data
010800090518     c                   if        vdatdal > 0
010900090518     c                   if        trsdfc > 0 or trsdec < vdatdalg
011000090518     c                   iter
011100090518     c                   endif
011200090518     c                   endif
011300090518     c                   clear                   t85socgd
011400090518     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
011500090518     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
011600090518     C                   RESET                   tibsSocI0
011700090518     C                   EVAL      tibsSocI0.idSocieta = trssocg
011800090518     C
011900090518     C                   CALL      'TIBSSOCR'
012000090518     C                   PARM      'GETANAGRAF'  prmRqsOpCode
012100090518     C                   PARM      *BLANK        prmRpyOpCode
012200090518     C                   PARM      *ZERO         prmRpyIdMsg
012300090518     C                   PARM      'TIBSSOCI0'   prmRqsFormato
012400090518     C                   PARM                    tibsSocI0
012500090518     C                   PARM                    prmRqsDataSize
012600090518     C                   PARM      'TIBSSOCO0'   prmRpyFormato
012700090518     C                   PARM                    tibsSocO0
012800090518     C                   PARM                    prmRpyDataSize
012900090518     c                   if         PRMRPYIDMSG >= 0 and
013000090518     c                              TIBSSOCO0.IDSOCIETA <> *blank
013100090518     c                   eval      t85socgd = tibsSocO0.RAGSOCIALE
013200090518     c                   else
013300090518     c                   eval      t85socgd = 'Società Errata'
013400090518     c                   end
013500090518      * ragione sociale società
013600090924     c                   eval      t85note = 'Non esistono autisti accreditati'
013700090924     c     trsnrc        setll     aitra01l
013800090924     c                   do        *hival
013900090924     c     trsnrc        reade     aitra01l
014000090924     c                   if        %eof(aitra01l)
014100090924     c                   leave
014200090924     c                   endif
014300090924     c                   movel     trasoc        t85rsoc
014400090924     c                   if        tradfi = 0
014500090924     c                   clear                   t85note
014600090924     c                   leave
014700090518     c                   endif
014800090924     c                   enddo
014900130304      * se la società risulta senza autisti non la scriviamo più chiesto Stefano Bartolini 04/03/13
015000130304     c                   if        t85note = 'Non esistono autisti accreditati'
015100130304     c                   iter
015200130304     c                   endif
015300090518      * descrizione filiale
015400090518     c     trsfil        chain     azorg01l
015500090518     c                   if        %found(azorg01l)
015600090518     c                   movel     orgdes        t85fild
015700090518     c                   move      orgfl3        T85DIST
015800090518     c                   move      orgcar        T85AREA
015900090518     c                   endif
016000070530      * descrizione area
016100090518     c                   movel     orgcar        kkey
016200070530     c                   move      '05'          kcod
016300070530     c     ktab          chain     tabel00f
016400070530     c                   if        %found(tabel00f)
016500090518     c                   movel     tbluni        t85aread
016600070530     c                   endif
016700120504     c**********         if        trsidp  <> *blank
016800120504     c**********         clear                   tibs02ds
016900120504     c**********         movel     'C'           t02mod
017000120504     c**********         movel     knsif         t02sif
017100120504     c**********         movel     'AIP'         t02cod
017200120504     c**********         movel     trsidp        t02ke1
017300120504     c**********         call      'TIBS02R'
017400120504     c**********         parm                    KPJBA
017500120504     c**********         parm                    TIBS02DS
017600120504     c**********         if        t02err =  *blanks
017700120504     c**********         movel     t02uni        daip
017800120504     c**********         movel     §aipdes       t85idpd
017900120504     c**********         endif
018000120504     c**********         endif
018100120301     c                   movel     trsflr        dtrsflr
018200120301     c                   movel     §TRSSTB       t85stb
018300120301     c                   movel     §TRSQLI       t85qli
018400090518     c                   z-add     TrsNRC        T85NRC
018500090518     c                   z-add     TrsFIL        T85FIL
018600090518     c                   move      TrsSOCG       T85SOCG
018700090518     c                   move      TrsIVA        T85IVA
018800090518     c                   z-add     TrsDEC        T85DEC
018900090518     c                   z-add     TrsDFC        T85DFC
019000120504     c**********         move      TrsCFI        T85CFI
019100090518     c                   move      TrsIDP        T85IDP
019200120504     c                   if        T85IDP = *blank
019300120504     c                   eval      T85IDP = 'N'
019400120504     c                   end
019500120504     c**********         move      TrsIDM        T85IDM
019600120504     c**********         move      TrsIDA        T85IDA
019700090518     c                   move      TrsDIA        T85DIA
019800090518     c                   move      Trsiva        T85IPIVA
019900090518     c                   write     trmz8500
020000090518     c                   enddo
020100070530      *
020200070530     c                   endsr
020300031105     c*-------------------------------------------------
020400031105     c     controv1      begsr
020500031105     c*-------------------------------------------------
020600070606     c                   setoff                                       9697
020700070605      *data disaccr. per turnover
020800070605     c                   setoff                                       44
020900090518     c                   if        vdatdal > 0  and vdatal > 0
021000070605     c                   seton                                        4496
021100070605     c                   goto      endcontr
021200070605     c                   endif
021300090518     c                   if        vdatal > 0
021400090518     C                   MOVE      Vdatal        G02DAT
021500070605     C                   MOVEL     *BLANK        G02ERR
021600070605     C                   CALL      'XSRDA8'
021700070605     C                   PARM                    WLBDA8
021800070605     C     G02ERR        IFEQ      '1'
021900070605     C                   SETON                                        4295
022000070605     c                   goto      endcontr
022100070605     C                   END
022200090518     c                   move      g02dat        vdatal
022300090518     c                   move      g02inv        vdatalg           8 0
022400090518     c                   move      g02inv        inimese           8 0
022500090518     c                   move      01            inimese
022600070529     c                   endif
022700051121
022800090518     c                   if        vdatdal > 0
022900090518     C                   MOVE      Vdatdal       G02DAT
023000090518     C                   MOVEL     *BLANK        G02ERR
023100090518     C                   CALL      'XSRDA8'
023200090518     C                   PARM                    WLBDA8
023300090518     C     G02ERR        IFEQ      '1'
023400090518     C                   SETON                                        4195
023500090518     c                   goto      endcontr
023600090518     C                   END
023700090518     c                   move      g02dat        vdatdal
023800090518     c                   move      g02inv        vdatdalg          8 0
023900090518     c                   endif
024000051122     c     endcontr      endsr
024100090519     c*-------------------------------------------------
024200090519     c     comandi       begsr
024300090519     c*-------------------------------------------------
024400090519      *
024500090519     C                   z-add     99            luncmd
024600090519     C                   CALL      'QCMDEXC'                            34
024700090519     C                   parm                    coman
024800090519     C                   parm                    luncmd
024900090519     c                   endsr
025000011026     c*-------------------------------------------------
025100011026     c     *inzsr        begsr
025200011026     c*-------------------------------------------------
025300020304     c     *entry        plist
025400020304     c                   parm                    kpjba
025500050801
025600050801     c     ktab          klist
025700050801     c                   kfld                    kkut              1 0
025800050801     c                   kfld                    kcod              2
025900050801     c                   kfld                    kkey              8
026000050801     c                   z-add     1             kkut
026100050801     c                   move      '05'          kcod
026200050801     c
026300050801
026400020218     C                   time                    w0120            14 0
026500020218     C                   move      w0120         wdat              8 0
026600030630     C                   CLEAR                   Tibs36Ds
026700030630     C                   EVAL      I36ute = knmus
026800030630     C                   CALL      'TIBS36R'
026900030630     C                   PARM                    tibs36ds
027000090518     ** Inizializzo il programma. per dati societari
027100090518     C                   CALL      'TIBSSOCR'
027200090518     C                   PARM      'INIT'        prmRqsOpCode
027300090518     C                   PARM                    prmRpyOpCode
027400090518     C                   PARM                    prmRpyIdMsg
027500090518
027600090519      *clear file
027700090519     c                   movea     cmd1          coman
027800090519     c                   exsr      comandi
027900090519     c                   movea     cmd2          coman
028000090519     c                   exsr      comandi
028100090519     c                   open      trmz850f
028200090519
028300090518     c                   endsr
028400090519**  CMD1
028500090519CLRPFM    FILE(trmz850f)
028600090519**  CMD2
028700090519CRTDUPOBJ OBJ(trmz850F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
028800090519**  CMD3
028900090519DLTF FILE(QTEMP/trmz850F)
029000090519**  CMD4
029100090519dltf file(WRKQUERY/trmz850f)
029200090519**  CMD5
029300090519CRTDUPOBJ OBJ(trmz850F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz850f) DATA(*YES)
