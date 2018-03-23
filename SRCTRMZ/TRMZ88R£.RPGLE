000100091016     H DFTACTGRP(*NO) BNDDIR('PRNPGM')
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400091012     Ftrmz88d   CF   E             WORKSTN
000500090518     Faitrs01l  if   e           k disk
000600090518     Faitra01l  if   e           k disk
000700090518     Fazorg01l  if   e           k disk
000800091012     Ftrmz880f  uf a e           k disk    usropn
000900050801     ftabel00f  if   e           k disk
000901091013     Fprtf198   o    F  198        PRINTER prtctl(prtds)
000902091016     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
001000020304     dkpjba          e ds
001100030630     d Tibs36Ds      e ds
001200070529     d Tibs02Ds      e ds
001201091013     d prtds           ds
001202091013     d   spab                         3  0
001203091013     d   spaa                         3  0
001204091013     d   skab                         3  0
001205091013     d   skaa                         3  0
001206091013     d   line                         3  0
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
002601091016     D ycoUffI1      E DS                  QUALIFIED
002602091016     D ycoUffI2      E DS                  QUALIFIED
002603091016     D ycoUffO1      E DS                  QUALIFIED
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
004101091016      *
004104091016     D  rqsOpCode      s             10A
004105091016     D  rpyEsito       s             10I 0
004106091016     D  RQSFORMAT      s             10A
004109091016     D  rqsDataSize    s             10I 0
004111091016     D  RPYFORMAT      s             10A
004114091016     D  rpyDataSize    s             10I 0
004116091016     D esito           S             10I 0
004200080502      *
004201091012     d spia            s              1
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
006201091013     c                   exsr      stampa
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
007700091012     c                   close     trmz880f
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
008800091012     c                   clear                   trmz8800
008801091028     c                   if        trsann <> *blank
008802091028     c                   iter
008803091028     c                   endif
008900090518      * filtro se cerca società in forza alla data
009000090518     c                   if        vdatal > 0
009100091019     c                   if        trsdfc > 0
009200070530     c                   iter
009300070530     c                   endif
009400090518     c                   endif
010100091012     c                   clear                   t88socgd
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
011900091012     c                   eval      t88socgd = tibsSocO0.RAGSOCIALE
012000090518     c                   else
012100091012     c                   eval      t88socgd = 'Società Errata'
012200090518     c                   end
012300090518      * ragione sociale società
012400091012     c                   eval      spia    = *blank
012500090924     c     trsnrc        setll     aitra01l
012600090924     c                   do        *hival
012700090924     c     trsnrc        reade     aitra01l
012800090924     c                   if        %eof(aitra01l)
012900090924     c                   leave
013000090924     c                   endif
013100091012     c                   movel     trasoc        t88rsoc
013200090924     c                   if        tradfi = 0
013300091012     c                   eval      spia =  '1'
013400090924     c                   leave
013401091012     c                   else
013402091012     c                   if        tradfi > t88dult
013403091012     c                   z-add     tradfi        t88dult
013404091012     c                   endif
013500090518     c                   endif
013600090924     c                   enddo
013601091012     c                   if        spia <> *blank
013602091012     c                   iter
013603091012     c                   endif
013700090518      * descrizione filiale
013800090518     c     trsfil        chain     azorg01l
013900090518     c                   if        %found(azorg01l)
014000091012     c                   movel     orgdes        t88fild
014100091012     c                   move      orgfl3        t88DIST
014200091012     c                   move      orgcar        t88AREA
014300090518     c                   endif
014400070530      * descrizione area
014500090518     c                   movel     orgcar        kkey
014600070530     c                   move      '05'          kcod
014700070530     c     ktab          chain     tabel00f
014800070530     c                   if        %found(tabel00f)
014900091012     c                   movel     tbluni        t88aread
015000070530     c                   endif
016500091012     c                   z-add     TrsNRC        t88NRC
016600091012     c                   z-add     TrsFIL        t88FIL
016700091012     c                   move      TrsSOCG       t88SOCG
016800091012     c                   move      TrsIVA        t88IVA
016900091012     c                   z-add     TrsDEC        t88DEC
017001091012     c                   z-add     TrsDrC        t88DrC
017002091016     c                   eval      ycoUffi1.soc = t88socg
017003091016     c                   eval      ycoUffi1.cdiva = t88iva
017004091016     c                   EVAL      rqsDataSize = %SIZE(ycoUffi1)
017005091016     c                   EVAL      rpyDataSize = %SIZE(ycoUffO1)
017006091016     C                   CALL      'YCOUFFR'
017007091016     c                   PARM      'GETUFFINFO'  rqsOpCode
017008091016     c                   PARM                    esito
017009091016     c                   PARM      'YCOUFFI1'    rqsFormat
017010091016     c                   PARM                    ycoUffI1
017011091016     c                   PARM                    rqsDataSize
017012091016     c                   PARM      'YCOUFFO1'    rpyFormat
017013091016     c                   PARM                    ycoUffO1
017014091016     c                   PARM                    rpyDataSize
017015091016     c                   if        esito >= 0
017016091030     c                   eval      dataiso = ycoUffO1.ufdtdoc
017017091030     c                   move      dataiso       limfat            8 0
017018091030     c                   if        limfat <= vdatfatg
017019091016     c                   eval      t88MUFA = %subdt(ycoUffO1.ufdtdoc:*m)
017020091016     c                   eval      t88aUFA = %subdt(ycoUffO1.ufdtdoc:*y)
017021091030     c                   else
017022091030     c                   clear                   trmz8800
017023091030     c                   iter
017024091030     c                   endif
017025091016     c                   endif
017026091016      *
017700091012     c                   write     trmz8800
017800090518     c                   enddo
017900070530      *
018000070530     c                   endsr
018100031105     c*-------------------------------------------------
018200031105     c     controv1      begsr
018300031105     c*-------------------------------------------------
018400091030     c                   setoff                                       424496
018500091030      *data limite disaccreditamento
019100091030     c                   if        vdatal = 0
019101091030     C                   seton                                        4296
019102091030     c                   goto      endcontr
019103091030     c                   else
019200090518     C                   MOVE      Vdatal        G02DAT
019300070605     C                   MOVEL     *BLANK        G02ERR
019400070605     C                   CALL      'XSRDA8'
019500070605     C                   PARM                    WLBDA8
019600070605     C     G02ERR        IFEQ      '1'
019700091030     C                   seton                                        4296
019800070605     c                   goto      endcontr
019900070605     C                   END
020000090518     c                   move      g02dat        vdatal
020100090518     c                   move      g02inv        vdatalg           8 0
020400070529     c                   endif
020500051121
020502091030      *data limite disaccreditamento
020504091030     c                   if        vdatfat= 0
020505091030     C                   seton                                        4496
020506091030     c                   goto      endcontr
020507091030     c                   else
020508091030     C                   MOVE      Vdatfat       G02DAT
020509091030     C                   MOVEL     *BLANK        G02ERR
020510091030     C                   CALL      'XSRDA8'
020511091030     C                   PARM                    WLBDA8
020512091030     C     G02ERR        IFEQ      '1'
020513091030     C                   seton                                        4496
020514091030     c                   goto      endcontr
020515091030     C                   END
020516091030     c                   move      g02dat        vdatfat
020517091030     c                   move      g02inv        vdatfatg          8 0
020518091030     c                   endif
020519091030
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
022701091013     c*-------------------------------------------------
022702091013     c     stampa        begsr
022703091013     c*-------------------------------------------------
022704091013      *
022705091013     C                   z-add     99            line
022706091013     C     *loval        setll     trmz880f                             34
022707091013     C                   do        *hival
022708091013     C                   read      trmz880f
022709091013     c                   if        %eof(trmz880f)
022710091013     c                   leave
022711091013     c                   endif
022712091013     c                   if        line > 60
022713091013     c                   except    testata
022714091013     c                   endif
022715091016     c                   clear                   t88drcs
022716091016     c                   clear                   t88decs
022717091016     c                   clear                   t88dults
022719091013     c                   if        t88drc > 0
022720091013     c                   move      t88drc        dataiso
022721091013     c                   move      dataiso       dataeur
022722091013     c                   move      dataeur       t88drcs           8 0
022723091013     c                   endif
022724091013     c                   if        t88dec > 0
022725091013     c                   move      t88dec        dataiso
022726091013     c                   move      dataiso       dataeur
022727091013     c                   move      dataeur       t88decs           8 0
022728091013     c                   endif
022729091013     c                   if        t88dult> 0
022730091013     c                   move      t88dult       dataiso
022731091013     c                   move      dataiso       dataeur
022732091013     c                   move      dataeur       t88dults          8 0
022733091013     c                   endif
022739091013     c                   except    riga
022740091013     c                   enddo
022741091013      *
022742091013     c                   endsr
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
024301091030     C                   move      wdat          vdatal
024302091030     C                   move      wdat          vdatfat
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
025900091012     c                   open      trmz880f
026000090519
026100090518     c                   endsr
026101091013     C*--------------------------------------------------------------------
026102091013     Oprtf198   E            testata        1 02
026103091013     o                       knsif               10
026104091013     o                       knmus               50
026105091013     o                                        +   3 'Società con contratti -
026106091013     o                                              senza autisti accreditati'
026108091013     o                                        +   1 '/'
026110091013     o                                        +   1 'TRMZ88R'
026112091013     o                       udate         y  +   1
026113091013     O                       page          z  +   2
026114091013     O          E            testata     1
026115091013     o                                           50 'Situazione al'
026118091013     o                       vdatal            +  2 '  /  /    '
026119091013     O          E            testata     1  1
026120091013     o                                           12 'Area'
026121091013     o                                           37 'Filiale'
026122091013     o                                           73 'Società nostra'
026123091013     o                                           93 'P I.v.A.'
026124091013     o                                          120 'Società'
026125091013     o                                          142 'Ultimo dis.'
026126091013     o                                          154 'Emis.contr.'
026128091019     o                                          166 'Ric. contr.'
026129091016     o                                          188 'Ult.fatt.'
026130091016     o                                          198 'Contratto'
026148091013     O          E            riga        1
026150091013     o                       T88AREA              3
026151091013     o                       T88AREAD            30
026152091013     o                       T88FIL              33
026153091013     o                       T88FILD             55
026154091013     o                       T88SOCG             60
026155091013     o                       T88SOCGD            82
026156091013     o                       T88IVA             100
026157091013     o                       T88RSOC            130
026160091013     o                       T88DULTs           142 '  /  /    '
026161091013     o                       T88DECs            154 '  /  /    '
026163091019     o                       T88DRCs            166 '  /  /    '
026164091016     o                       t88mufa            183
026165091016     o                                              '/'
026166091019     o                       t88aufa           +  0
026167091016     o                       T88nrc             198
026200091013**  CMD1
026300091012CLRPFM    FILE(trmz880f)
026400090519**  CMD2
026500091012CRTDUPOBJ OBJ(trmz880F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
026600090519**  CMD3
026700091012DLTF FILE(QTEMP/trmz880F)
026800090519**  CMD4
026900091012dltf file(WRKQUERY/trmz880f)
027000090519**  CMD5
027100091012CRTDUPOBJ OBJ(trmz880F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz880f) DATA(*YES)
